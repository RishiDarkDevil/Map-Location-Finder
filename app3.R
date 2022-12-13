# Necessary Packages
library(shiny)
library(shinyFeedback)
library(leaflet)
library(jpeg)
library(tidyverse)
library(cowplot)
library(magick)

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
  plotOutput( # Plot Screenshot
    'plot', inline = TRUE,
    click = 'plot_click'
  )
)

# Results Page ----------------------------------------------------------------
ResultPageUI <- tabPanel(
  'ResultPage',
  "View Results"
)

# Put all UI together
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
    screenshots_image <- readJPEG(screenshots()[1, 'datapath'])
    output$plot <- renderPlot({
      ggdraw(xlim = c(0, input$dimension[1]), ylim = c(0, input$dimension[2])) +
        draw_image(screenshots_image, width = input$dimension[1], height = input$dimension[2])
    }, res = 96, height = input$dimension[2], width = input$dimension[1])  
  })
  
  # Update Image on click
  observeEvent(input$plot_click, {
    click <- input$plot_click
    print(click$x)
  })
  
  
}

shinyApp(ui, server)