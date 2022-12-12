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

# UI --------------------------------------------------------------------------
# Upload Page -----------------------------------------------------------------
UploadPageUI <- tabPanel(
  'UploadPage',
  fileInput(
    'uploadImages', 'Upload JPEG Map Screenshots',
    multiple = TRUE, accept = c('.jpg', '.jpeg')
  ),
  actionButton(
    'MarkUpButton', 'Markup Map Screenshots'
  )
)

# Markup Page -----------------------------------------------------------------
MarkUpPageUI <- tabPanel(
  'MarkupPage',
  bootstrapPage(
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
)

# Results Page ----------------------------------------------------------------
ResultPageUI <- tabPanel(
  'ResultPage',
  "View Results"
)

ui <- fluidPage(
  tabsetPanel(
    id = 'MapLocApp',
    type = 'hidden',
    UploadPageUI,
    MarkUpPageUI,
    ResultPageUI
  )
)

# SERVER ----------------------------------------------------------------------
server <- function(input, output, session) {
  
}

shinyApp(ui, server)