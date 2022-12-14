# Necessary Packages
library(shiny)
library(shinyFeedback)
library(leaflet)
library(jpeg)
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
    'screenshots', 'Upload JPEG Map Screenshots',
    multiple = TRUE, accept = c('.jpg', '.jpeg')
  ),
  actionButton(
    'MarkUpButton', 'Markup Map Screenshots'
  )
)

# Markup Page -----------------------------------------------------------------
# The Javascript Code is used to get the size of the browser window
MarkUpPageUI <- tabPanel(
  'MarkUpPage',
  tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
  imageOutput(
    'image',
    width = 'auto', height = 'auto', inline = TRUE, 
    click = 'image_click'
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
  
  # Store the input images
  screenshots <- reactive({
    req(input$screenshots)
    input$screenshots
  })
  
  # Display images for markup
  observeEvent(input$MarkUpButton, {
    updateTabsetPanel(inputId = 'MapLocApp', selected = 'MarkUpPage')
    print(input$dimension)
    output$image <- renderImage({
      list(
        src = screenshots()[1,'datapath'],
        contentType = 'image/jpeg',
        alt = 'screenshot',
        width = paste0(input$dimension[0], 'px'), height = paste0(input$dimension[1], 'px'),
        style="display: block; margin-left: auto; margin-right: auto;"
      )
    }, deleteFile = FALSE)  
  })
  
  # Update Image on click
  observeEvent(input$image_click, {
    click <- input$image_click
    
  })
  
  
}

shinyApp(ui, server)