testUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      selectInput(ns("plotType"), "Plot Type",
                  c(Scatter = "scatter", Histogram = "hist")
      ),
      # Only show this panel if the plot type is a histogram
      conditionalPanel(
        condition = "input.plotType == 'hist'",
        ns = ns,
        selectInput(
          ns("breaks"), "Breaks",
          c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
        ),
        # Only show this panel if Custom is selected
        conditionalPanel(
          condition = "input.breaks == 'custom'",
          ns = ns,
          sliderInput(ns("breakCount"), "Break Count", min = 1, max = 50, value = 10)
        )
      )
    ),
    mainPanel(
      plotOutput(ns("plot"))
    )
  )
}

testServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    x <- rnorm(100)
    y <- rnorm(100)
    
    output$plot <- renderPlot({
      if (input$plotType == "scatter") {
        plot(x, y)
      } else {
        breaks <- input$breaks
        if (breaks == "custom") {
          breaks <- input$breakCount
        }
        
        hist(x, breaks = breaks)
      }
    })
  })
}

testApp <- function(){
  ui <- fluidPage(
    testUI("myTestId")
  )
  server <- function(input, output, session) {
    testServer("myTestId")
  }
  shinyApp(ui, server)
}

# testApp()
