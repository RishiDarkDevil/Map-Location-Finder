# Necessary Packages
library(shiny)
library(shinyFeedback)
library(shinyWidgets)
library(shinydashboard)
library(miniUI)
library(leaflet)
library(jpeg)
library(tidyverse)
library(cowplot)
library(magick)

# DATA-------------------------------------------------------------------------
# Location Data
location_data <- tibble(
  'screenshot' = c(1),
  'name' = c(''),
  'rel_x' = c(0),
  'rel_y' = c(0)
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
  miniTitleBar("Mark Locations on Screenshot",
               left = miniTitleBarButton("prev", "Previous", primary = TRUE),
               right = miniTitleBarButton("nex", "Next", primary = TRUE)
  ),
  fluidRow(
    column(
      12, align = 'center',
      plotOutput( # Plot Screenshot
        'plot', inline = TRUE,
        click = 'plot_click'
      )
    )
  )
)

# Results Page ----------------------------------------------------------------
ResultPageUI <- tabPanel(
  'ResultPage',
  useShinydashboard(),
  fluidRow(
    column(
      12, align = 'center',
      box(
        title = "Vector Map", status = "success", solidHeader = TRUE, collapsible = TRUE,
        plotOutput( # Plot VectorMap
          'plotVM', 
          hover = hoverOpts(id = "plot_hover", delay = 100, 
                            delayType = "debounce",
                            clip = TRUE, nullOutside = TRUE)
        )
      )
    )
  )
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
    print(input$screenshots)
  })
  
  # Current Screenshot Number
  currindex <- reactiveVal(1)
  
  # Marked locations on an image
  points <- reactiveVal(
    tibble('screenshot' = c(0), 'name' = c(''), 'rel_x' = c(NA), 'rel_y' = c(NA), 'dim_x' = c(NA), 'dim_y' = c(NA))
  )
  
  # Helper function for displaying images for markup
  displayImage <- function(img_idx) {
    screenshots_image <- readJPEG(screenshots()[img_idx, 'datapath'])
    if (nrow(points() %>% filter(screenshot == img_idx)) == 0) {
      output$plot <- renderPlot({
        ggdraw(xlim = c(0, dim(screenshots_image)[2]), ylim = c(0, dim(screenshots_image)[1])) +
          draw_image(screenshots_image, width = dim(screenshots_image)[2], height = dim(screenshots_image)[1])
      }, res = 96, height = dim(screenshots_image)[1], width = dim(screenshots_image)[2])
    } else {
      output$plot <- renderPlot({
        ggdraw(xlim = c(0, dim(screenshots_image)[2]), ylim = c(0, dim(screenshots_image)[1])) +
          draw_image(screenshots_image, width = dim(screenshots_image)[2], height = dim(screenshots_image)[1]) +
          geom_point(aes(rel_x, rel_y), data = points() %>% filter(screenshot == currindex()), size = 5)
      }, res = 96, height = dim(screenshots_image)[1], width = dim(screenshots_image)[2])
    }
    
  }
  
  # Display images for markup
  observeEvent(input$MarkUpButton, {
    updateTabsetPanel(inputId = 'MapLocApp', selected = 'MarkUpPage')
    displayImage(currindex()) 
  })
  
  # Update image on click and the location database for that image
  observeEvent(input$plot_click, {
    click <- input$plot_click
    screenshots_image <- readJPEG(screenshots()[currindex(), 'datapath']) # MAKE IT CUSTOM SS
    
    click_data <- points() %>%
      full_join(tibble('screenshot' = c(currindex()), 'name' = c(''), 'rel_x' = click$x, 'rel_y' = click$y, 'dim_x' = dim(screenshots_image)[2], 'dim_y' = dim(screenshots_image)[1]))
    points(click_data)
    print(points())
    print(input$dimension)
    displayImage(currindex())
    
    # Add name of place
    inputSweetAlert(
      session = session,
      "locName",
      input = "text",
      title = "Enter name of this location.",
      inputPlaceholder = "e.g.: Indian Statistical Institute Kolkata",
      allowOutsideClick = FALSE,
      showCloseButton = FALSE
    )
  })
  
  # Update Place Name for the current point clicked on Image
  observe({
    req(input$locName)
    click_data_name <- points()
    click_data_name[nrow(click_data_name), 'name'] <- input$locName
    points(click_data_name)
    print(points())
  })
  
  # Change to previous screenshot
  observeEvent(input$prev, {
    if (currindex() > 1) {
      currindex(currindex() - 1)
      displayImage(currindex())
    }
  })
  
  # Change to next screenshot
  observeEvent(input$nex, {
    if (currindex() < nrow(screenshots())) {
      currindex(currindex() + 1)
      displayImage(currindex())
      if (currindex() == nrow(screenshots()))
        updateActionButton(session, 'nex', 'Finish')
    } else {
      show_alert(
        title = "Success !!",
        text = "Vector Map Generated",
        type = "success"
      )
      points(points()[2:nrow(points()),])
      fit_x <- lm(rel_x ~ name + screenshot - 1, data = points())
      fit_y <- lm(rel_y ~ name + screenshot - 1, data = points())
      m <- length(unique(points()$name))
      x_coord_names <- fit_x$coefficients[1:m]
      y_coord_names <- fit_y$coefficients[1:m]
      VMSdata <- tibble(
        'name' = substring(names(x_coord_names), 5),
        'x' = x_coord_names,
        'y' = y_coord_names
      )
      print(VMSdata) ##########
      output$plotVM <- renderPlot({
        VMSdata %>%
        ggplot(aes(x, y)) +
          geom_point() +
          xlim(range(VMSdata$x)[1] - 10, range(VMSdata$x)[2] + 10) +
          ylim(range(VMSdata$y)[1] - 10, range(VMSdata$y)[2] + 10)
      })
      updateTabsetPanel(inputId = 'MapLocApp', selected = 'ResultPage')
    }
  })
  
}

shinyApp(ui, server)