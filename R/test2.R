library(shiny)

ui <- fluidPage(
    uiOutput("firstTab")
)

server <- function(input, output, session) {
  output$firstTab <- renderUI({
    tabsetPanel(
      tabPanel(
        title = "Test Panel 1",
        sliderInput(inputId = "slider1", "", min = 0, max = 10, step = 1, value = 5)
      )
    )
  })
}

shinyApp(ui, server)