library(shiny)
library(shinydashboard)
library(plyr)
library(dplyr)
library(tidyverse)
library(plotly)
library(leaflet)
library(data.table)
library(DT)
library(odbc)
library(DBI)

#global variables ####

#google maps basemap tiles
map_url_google <-"https://mts1.google.com/vt/lyrs=m&hl=en&src=app&x={x}&y={y}&z={z}&s=G"
#map zoom level
zoom_level <- 11


#read data from SQL Server
hhts19_con <- dbConnect(odbc(),
                        driver = "SQL Server",
                        server = "AWS-PROD-SQL\\Coho",
                        database = "HouseholdTravelSurvey2019",
                        trusted_connection = "yes"
)

trip_tbl <- dbGetQuery(hhts19_con, "SELECT rt.hhid,
                                           rt.personid,
                                           rt.daynum,
                                           rt.tripid,
                                           rt.tripnum,
                                           df.o_purpose,
                                           df.d_purpose,
                                           df.depart_dhm,
                                           df.arrive_dhm,
                                           rt.mode_type,
                                           rt.origin_lat,
                                           rt.origin_lng,
                                           rt.dest_lat,
                                           rt.dest_lng
                                    FROM HHSurvey.ResolvedTrip AS rt
                                    INNER JOIN HHSurvey.data2fixie AS df
                                        ON df.recid = rt.recid")

dbDisconnect(hhts19_con)


#count number of days for each person
trip_tbl <- ddply(trip_tbl,.(personid),transform, days = length(unique(daynum)))

#define list of person ids and the number of travel days
#this is used later to define the conditional selection of days
pers_days <- unique(trip_tbl[c("personid", "days")])

# UI side ####

ui <- dashboardPage(
  dashboardHeader(title = "Trip Origin-Destinationsy", titleWidth = 250),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     menuItem("Trips", tabName = "main_tab", icon = icon("map")),
                     menuItem("About", tabName = "about_tab", icon = icon("info-circle"))
                   )
  ),
  
  dashboardBody(
    tabItems(
    tabItem(tabName = "main_tab",
      fluidRow(
      tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}"))), #to show dropdown menu in front of the map
      box(title = 'Select/Enter Person ID:', status = 'primary', solidHeader = TRUE,
        selectInput(
          inputId ='personid'
          ,label =''
          ,choices = trip_tbl$personid
          ,selected= 1
        )
        
      ),
      
      box(title = 'Select Diary Day:', status = 'primary', solidHeader = TRUE,
        uiOutput('daynum') #use uiOutput along with renderUI to condition daynum choices on personid
      ),
      
      box(width = 12,
          leafletOutput('map01', height = '400px')
      ),
      
      box(width = 12, title='Map Controls', collapsible = T, collapsed = T,
          actionButton("reset_button", "Reset Map View"),
          actionButton("clear_selection", "Clear Selections")
      ),
      
      box(width=12,
          div(style = 'overflow-x: scroll', DT::dataTableOutput('table01'))
      )
    )
    
    #adjusting actionButton alignment
    ,tags$style(type='text/css', "#reset_button {margin-left: 25px;}")
    ,tags$style(type='text/css', "#clear_selection {margin-left: 25px;}")
    ),
    
  tabItem(tabName = 'about_tab',
    fluidRow(
    tabBox(width=12, title= 'About', id='about', side = 'left',
        tabPanel("Description",
               includeMarkdown('description.Rmd')
                 ),
        tabPanel("Author",
                 includeMarkdown('author.Rmd')
                 )
       )
    #adjusting font size of the headers of the about tabs'
    ,tags$style(type='text/css', "#about {font-size: 20px} ")
      )
    )
  )
  )
)

# Server side ####

server <- function(input, output, session) { 
    
    #create a variable containg maxdays to condition day input choices
    maxdays <- reactive({
      pers_days$days[pers_days$personid==input$personid]
    })
    
    #creating daynum selectInput based on UiOutput
    output$daynum <- renderUI({
      selectInput(inputId ='day_x',label = '', choices=seq(1,maxdays()))
    })
    
  #the app is initiated with a NULL value for conditions based on renderUI  
  #create a 'day' variable to store the input day 
  #this is only needed for the mode attribute in googleway to work properly- all the indicies have to have a value  
  day <- reactive({
      if(is.null(input$day_x) || input$day_x>maxdays()) return(1) else return(input$day_x)
    })
  
  #subset data based on selection inputs
  trip_sub <-  reactive({
    subset(trip_tbl, trip_tbl$personid==input$personid & trip_tbl$daynum==day())
  })
  
  
  output$table01 <- DT::renderDataTable({
    DT::datatable(
      trip_sub()[c('hhid', 'personid', 'tripid', 'tripnum', 'o_purpose', 'd_purpose',
                 'depart_dhm', 'arrive_dhm', 'mode_type')]
      ,selection = "single"
      ,rownames=FALSE
      ,options=list(stateSave = TRUE, 
                    dom = 't', 
                    order = list(3, 'asc'))
      ,colnames = c('Household Id', 'Person Id', 'Trip Id', 'Trip Sequence', 'Trip Origin','Trip Destination',
                    'Departure Time', 'Arrival Time', 'Travel Mode')
    )
  })
  
  
  output$map01 <- renderLeaflet({
    leaflet(data = trip_sub()) %>%
      setView(lng = mean(trip_sub()$origin_lng), lat = mean(trip_sub()$origin_lat), zoom = zoom_level) %>%
      addTiles(urlTemplate= map_url_google, group = 'Google Maps')%>%
      #another option for the base layer (light background)
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = 'Esri Canvas') %>% #CartoDB.Positron
      addLayersControl(
        position = c("topright"),
        baseGroups = c("Google Maps", "Esri Canvas"),
        options = layersControlOptions(collapsed = T, autoZIndex = T)
      )
  })
  
  #update map to plot markers and polylines for each personid/day
  observe({
    leafletProxy("map01") %>%
      clearShapes() %>% 
      addCircleMarkers(data=trip_sub()
                 ,lng= trip_sub()$origin_lng
                 ,lat = trip_sub()$origin_lat
                 ,layerId = trip_sub()$tripid
                 ,label = paste0("", trip_sub()$o_purpose)
                 ,labelOptions= labelOptions(direction = 'auto', noHide=F)
      )
  })
  
  #create a mechanism to highlight markers and polylines when selected in DT
  start_icon = makeAwesomeIcon(icon = 'map-marker', markerColor = 'green', iconColor = 'white')
  end_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
 
  #for some reason the selected row id doesn't print null when deselected when called from an observeEvent
  row_selected <- reactive({
    input$table01_rows_selected
  })
  
  observe({
  if(!is.null(row_selected())){
  observeEvent(input$table01_rows_selected, {
    row_selected <- trip_sub()[input$table01_rows_selected,]

    leafletProxy("map01") %>%
      addAwesomeMarkers(data = subset(trip_sub(), trip_sub()$tripid==input$table01_rows_selected)
                 ,lng= row_selected$origin_lng
                 ,lat =row_selected$origin_lat
                 ,layerId = "highlighted_marker_start"
                 ,label = paste0("", row_selected$o_purpose)
                 ,labelOptions= labelOptions(direction = 'auto', noHide=F)
                 ,icon=start_icon
                 ,options = markerOptions(riseOnHover = TRUE))  %>%
      addAwesomeMarkers(data = subset(trip_sub(), trip_sub()$tripid==input$table01_rows_selected)
                        ,lng= row_selected$dest_lng
                        ,lat =row_selected$dest_lat
                        ,layerId = "highlighted_marker_end"
                        ,label = paste0("", row_selected$d_purpose)
                        ,labelOptions= labelOptions(direction = 'auto', noHide=F)
                        ,icon=end_icon
                        ,options = markerOptions(riseOnHover = TRUE))  %>%
    fitBounds(lng1 = min(row_selected$origin_lng, row_selected$dest_lng)*0.9996,
              lng2 = max(row_selected$origin_lng, row_selected$dest_lng)*1.0004,
              lat1 = min(row_selected$origin_lat,  row_selected$dest_lat)*0.9996,
              lat2 = max(row_selected$origin_lat,  row_selected$dest_lat)*1.0004)
  })
 
  }else{
    leafletProxy("map01") %>%
      removeMarker("highlighted_marker_start") %>%
      removeMarker("highlighted_marker_end")
  }
    
  })
  

  #reset view
  observe({
    input$reset_button
    leafletProxy("map01") %>%
      setView(lng = mean(trip_sub()$origin_lng), lat = mean(trip_sub()$origin_lat), zoom = zoom_level)
  })
  
  #clear selection
  observe({
    input$clear_selection
    leafletProxy("map01")%>%
      removeMarker("highlighted_marker_start") %>%
      removeMarker("highlighted_marker_end")
  })
  
}

# Run the app ####

shinyApp(ui = ui, server = server)
