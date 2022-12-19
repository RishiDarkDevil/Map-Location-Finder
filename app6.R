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
      ),
      box(
        title = "X Coordinate Model Residuals", status = "warning", solidHeader = TRUE, collapsible = TRUE,
        plotOutput( # Plot X Model Residuals
          'plotXRes', 
          hover = hoverOpts(id = "plot_hover_x_res", delay = 100, 
                            delayType = "debounce",
                            clip = TRUE, nullOutside = TRUE)
        )
      ),
      box(
        title = "Y Coordinate Model Residuals", status = "warning", solidHeader = TRUE, collapsible = TRUE,
        plotOutput( # Plot Y Model Residuals
          'plotYRes', 
          hover = hoverOpts(id = "plot_hover_y_res", delay = 100, 
                            delayType = "debounce",
                            clip = TRUE, nullOutside = TRUE)
        )
      ),
      box(
        title = "X DFFITs", status = "info", solidHeader = TRUE, collapsible = TRUE,
        plotOutput( # Plot X DFFITs
          'plotDFFITX', 
          hover = hoverOpts(id = "plot_hover_dffit_x", delay = 100, 
                            delayType = "debounce",
                            clip = TRUE, nullOutside = TRUE)
        )
      ),
      box(
        title = "Y DFFITs", status = "info", solidHeader = TRUE, collapsible = TRUE,
        plotOutput( # Plot Y DFFITs
          'plotDFFITY', 
          hover = hoverOpts(id = "plot_hover_dffit_y", delay = 100, 
                            delayType = "debounce",
                            clip = TRUE, nullOutside = TRUE)
        )
      ),
      box(
        title = "X DFBETAs", status = "info", solidHeader = TRUE, collapsible = TRUE,
        plotOutput( # Plot X DFBETAs
          'plotDFBETAX', 
          hover = hoverOpts(id = "plot_hover_dfbeta_x", delay = 100, 
                            delayType = "debounce",
                            clip = TRUE, nullOutside = TRUE)
        )
      ),
      box(
        title = "Y DFBETAs", status = "info", solidHeader = TRUE, collapsible = TRUE,
        plotOutput( # Plot Y DFBETAs
          'plotDFBETAY', 
          hover = hoverOpts(id = "plot_hover_dfbeta_y", delay = 100, 
                            delayType = "debounce",
                            clip = TRUE, nullOutside = TRUE)
        )
      ),
      box(
        title = "X Cook's Distance", status = "info", solidHeader = TRUE, collapsible = TRUE,
        plotOutput( # Plot X DFFITs
          'plotCOOKX', 
          hover = hoverOpts(id = "plot_hover_cook_x", delay = 100, 
                            delayType = "debounce",
                            clip = TRUE, nullOutside = TRUE)
        )
      ),
      box(
        title = "Y Cook's Distance", status = "info", solidHeader = TRUE, collapsible = TRUE,
        plotOutput( # Plot Y DFFITs
          'plotCOOKY', 
          hover = hoverOpts(id = "plot_hover_cook_y", delay = 100, 
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
  
  # Vector Map Data
  pointsVM <- reactiveVal(0)
  
  # Residual Map Data
  pointsRes <- reactiveVal(0)
  
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
      # If all screenshot finished and finished button clicked then change to result page and generate results
      show_alert(
        title = "Success !!",
        text = "Vector Map and Analysis Generated",
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
        'y' = y_coord_names,
      )
      pointsVM(VMSdata)
      print(VMSdata) ##########
      
      # Vector Map Plot
      output$plotVM <- renderPlot({
        pointsVM() %>%
          ggplot(aes(x, y)) +
          geom_point() +
          xlim(range(pointsVM()$x)[1] - 10, range(pointsVM()$x)[2] + 10) +
          ylim(range(pointsVM()$y)[1] - 10, range(pointsVM()$y)[2] + 10)+
          labs(
            x = "Global X coordinates",
            y = "Global Y coordinates"
          ) +
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            strip.background = element_blank()
          ) 
      }, res = 96)
      
      resData <- tibble(
        'name' = points()$name,
        'screenshot' = points()$screenshot,
        'pred_x' = predict(fit_x, points()),
        'pred_y' = predict(fit_y, points()),
        'res_x' = fit_x$residuals,
        'res_y' = fit_y$residuals
      )
      
      print(resData) ######
      pointsRes(resData)
      
      infXData <- as_tibble(influence.measures(fit_x)$infmat)
      infYData <- as_tibble(influence.measures(fit_y)$infmat)
      
      # X residuals plot
      output$plotXRes <- renderPlot({
        resData %>%
          ggplot(aes(pred_x, res_x)) +
          geom_point() +
          labs(
            x = "Fitted X ordinate",
            y = "Residuals"
          ) +
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            strip.background = element_blank()
          )
      }, res = 96)
      
      # Y residuals plot
      output$plotYRes <- renderPlot({
        resData %>%
          ggplot(aes(pred_y, res_y)) +
          geom_point() +
          labs(
            x = "Fitted Y abscissa",
            y = "Residuals"
          ) +
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            strip.background = element_blank()
          )
      }, res = 96)
      
      
      updateTabsetPanel(inputId = 'MapLocApp', selected = 'ResultPage')
    }
  })
  
  # Enables Hover Function on a plot with proper arguments passed to it
  enableHover <- function(hover_data, dat, xvar, yvar, xlabel, ylabel) {
    if(nrow(hover_data) == 1) {
      dat %>%
        ggplot(aes_string(xvar, yvar)) +
        geom_point()  +
        geom_label(data = hover_data, 
                   aes(label = label), 
                   nudge_x = 0.2
        ) +
        xlim(range(dat[[xvar]])[1] - 10, range(dat[[xvar]])[2] + 10) +
        ylim(range(dat[[yvar]])[1] - 10, range(dat[[yvar]])[2] + 10)+
        labs(
          x = xlabel,
          y = ylabel
        ) +
        theme_bw() +
        theme(
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.background = element_blank()
        )
    } else { # back to original state if no hover_data
      dat %>%
        ggplot(aes_string(xvar, yvar)) +
        geom_point()  +
        geom_label(data = hover_data, 
                   aes(label = label), 
                   nudge_x = 0.2
        ) +
        xlim(range(dat[[xvar]])[1] - 10, range(dat[[xvar]])[2] + 10) +
        ylim(range(dat[[yvar]])[1] - 10, range(dat[[yvar]])[2] + 10)+
        labs(
          x = xlabel,
          y = ylabel
        ) +
        theme_bw() +
        theme(
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.background = element_blank()
        ) 
    } 
  }
  
  # Result Page Vector Map Plot Hover
  observeEvent(input$plot_hover, {
    # hover tooltip
    hover_data <- nearPoints(pointsVM(), input$plot_hover) %>% 
      mutate(label = paste("name:", name, 
                           "\nglobal x:", round(x, 3), 
                           "\nglobal y:", round(y, 3), sep = ""))
    
    # hover x and y coordinates
    hover_summary <- paste0("x=", round(input$plot_hover$x, 3), 
                            "\ny=", round(input$plot_hover$y, 3))
    
    # output the plot post-startup
    output$plotVM <- renderPlot({
      
      enableHover(hover_data, pointsVM(), 'x', 'y', 'Global X Coordinates', 'Global Y Coordinates') 
    })
  })
  
  # Result Page X Res Plot Hover
  observeEvent(input$plot_hover_x_res, {
    # hover tooltip
    hover_data <- nearPoints(pointsRes(), input$plot_hover_x_res) %>% 
      mutate(label = paste("name:", name, 
                           "\nscreenshot:", screenshot, 
                           "\nresidual:", round(res_x, 3), sep = ""))
    
    
    # output the plot post-startup
    output$plotXRes <- renderPlot({
      
      enableHover(hover_data, pointsRes(), 'pred_x', 'res_x', 'Fitted X ordinate', 'X Residuals')
      
    })
  })
  
  # Result Page Y Res Plot Hover
  observeEvent(input$plot_hover_y_res, {
    # hover tooltip
    hover_data <- nearPoints(pointsRes(), input$plot_hover_y_res) %>% 
      mutate(label = paste("name:", name, 
                           "\nscreenshot:", screenshot, 
                           "\nresidual:", round(res_y, 3), sep = ""))
    
    
    # output the plot post-startup
    output$plotYRes <- renderPlot({
      
      enableHover(hover_data, pointsRes(), 'pred_y', 'res_y', 'Fitted Y abscissa', 'Y Residuals')
    })
  })
  
  # Result Page Y Res Plot Hover
  observeEvent(input$plot_hover_y_res, {
    # hover tooltip
    hover_data <- nearPoints(pointsRes(), input$plot_hover_y_res) %>% 
      mutate(label = paste("name:", name, 
                           "\nscreenshot:", screenshot, 
                           "\nresidual:", round(res_y, 3), sep = ""))
    
    
    # output the plot post-startup
    output$plotYRes <- renderPlot({
      
      enableHover(hover_data, pointsRes(), 'pred_y', 'res_y', 'Fitted Y abscissa', 'Y Residuals')
    })
  })
  
}

shinyApp(ui, server)