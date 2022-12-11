# Necessary Packages
library(shiny)
library(leaflet)
library(RColorBrewer)


# Map UI
map_ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  # Map
  leafletOutput("map", width = "100%", height = "100%"),
)

server <- function(input, output, session) {
  
  # Render First Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
}

shinyApp(map_ui, server)