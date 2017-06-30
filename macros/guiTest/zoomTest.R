library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(shiny)

ui <- fluidPage(width = 4, h4("Drag and then double-click to zoom"),
                plotOutput("plot1", height = 300,dblclick = "plot1_dblclick",brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE)
                           )
                )

server <- function(input, output) {

  # -------------------------------------------------------------------
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)

  output$plot1 <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
            
}

shinyApp(ui, server)
