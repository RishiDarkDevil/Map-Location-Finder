# Necessary Packages
library(shiny)
library(leaflet)
library(tidyverse)

# DATA-------------------------------------------------------------------------
# Location Data
location_data <- tibble(
  'name' = c('Bonhooghly Bus Stand', 'Indian Statistical Institute Kolkata'),
  'lng' = c(22.6452, 22.6487),
  'lat' = c(88.3775, 88.3769)
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
    uiOutput('newMarkerName')
  )
)

# SERVER-----------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Render First Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  # Add Circle Markers on clicks on the Map if Add Marker is TRUE
  observeEvent(input$map_click, {
    if (input$isPutMarker) {
      click = input$map_click
      
      leafletProxy('map') %>%
        addCircleMarkers(lng = click$lng, lat = click$lat)
    }
  })
  
  # Remove Markers when toggle Add Marker
  observeEvent(input$isPutMarker, {
      leafletProxy('map') %>%
        clearMarkers()
  })
  
  # Select Add Marker Name
  # If Add Marker Mode TRUE then put an option of Others to add new Marker Name 
  observeEvent(input$isPutMarker, {
    if (input$isPutMarker) {
      updateSelectInput(session, 'markerName', 'Marker Place Name', choices = c(location_data$name, 'Other'))
    } else {
      updateSelectInput(session, 'markerName', 'View Place', choices = location_data$name)
    }
  })
  
  # If Other is selected then display textInput to enter name of the place
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