# Necessary Packages
library(shiny)
library(shinyFeedback)
library(leaflet)
library(tidyverse)

# DATA-------------------------------------------------------------------------
# Location Data
location_data <- tibble(
  'name' = c('Bonhooghly Bus Stand', 'Indian Statistical Institute Kolkata'),
  'lng' = c(88.3775, 88.3769),
  'lat' = c(22.6452, 22.6487)
)


# UI---------------------------------------------------------------------------
# Map UI
map_ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  # Map
  leafletOutput("map", width = "100%", height = "100%"),
  
  # Marker Details
  absolutePanel(
    top = 10, right = 10,
    checkboxInput('isPutMarker', 'Add marker on click?', value = FALSE),
    selectInput('markerName', 'View Place', choices = location_data$name),
    useShinyFeedback(),
    uiOutput('newMarkerName')
  )
)

# SERVER-----------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Location Data
  data <- reactiveVal(location_data)
  
  # Render First Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  # Add Circle Markers on clicks on the Map if Add Marker is TRUE
  observeEvent(input$map_click, {
    if (input$isPutMarker) {
      click = input$map_click
      
      # Update the location_data
      # Name of the place whose circle is added
      new_point_name <- input$markerName
      if (input$markerName == 'Other') {
        # Raise Warning if No place name is entered in Other
        feedbackWarning('newMarkerNameText', input$newMarkerNameText == "", "Place name can't be empty!")
        req(input$newMarkerNameText != "")
        new_point_name <- input$newMarkerNameText
      }
      
      leafletProxy('map') %>%
        addCircleMarkers(lng = click$lng, lat = click$lat)
      
      new_point_data <- tibble(
        'name' = new_point_name, 
        'lng' = click$lng,
        'lat' = click$lat
      )
      dat <- data() %>%
        full_join(new_point_data)
      data(dat)
      print(data())
    }
  })
  
  # Remove Markers when toggle Add Marker
  observeEvent(input$isPutMarker, {
    leafletProxy('map') %>%
      clearMarkers()
    
    if (!input$isPutMarker) {
      leafletProxy('map', data = data()) %>%
      addMarkers(~lng, ~lat)
    }
  })
  
  # Select Add Marker Name
  # If Add Marker Mode TRUE then put an option of Others to add new Marker Name 
  observeEvent(input$isPutMarker, {
    if (input$isPutMarker) {
      updateSelectInput(session, 'markerName', 'Marker Place Name', choices = c(unique(data()$name), 'Other'))
    } else {
      updateSelectInput(session, 'markerName', 'View Place', choices = unique(data()$name))
    }
  })
  
  # If Other is selected then display textInput to enter name of the place otherwise remove this Other textInput
  observeEvent(input$markerName, {
    if (input$markerName == 'Other') {
      output$newMarkerName <- renderUI(
        textInput('newMarkerNameText', 'Other', placeholder = 'If not present above')
      )
    } else {
      if(!is.null(input$newMarkerNameText)){
        removeUI(selector = "#newMarkerNameText")
        removeUI(selector = "#newMarkerNameText-label")
      }
    }
  })
  
  
}

shinyApp(map_ui, server)