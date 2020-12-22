elegirAlumnoUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui"))
  )
}

elegirAlumnoServer <- function(id, choices_alumnos) {
  moduleServer(id, function(input, output, session) {
    output$ui <- renderUI({
      ns <- session$ns
      selectInput(ns("select_alumno"), label = "Seleccione participante", choices = choices_alumnos())
    })
    reactive(input$select_alumno)
  })
}

elegirAlumnoApp <- function(){
  ui <- fluidPage(
    elegirAlumnoUI("myTestId")
  )
  server <- function(input, output, session) {
    choices_alumnos <- reactive(c("alumno1", "alumno2", "alumno3", "alumno4", "alumno5"))
    elegirAlumnoServer("myTestId", choices_alumnos)
  }
  shinyApp(ui, server)
}

# elegirAlumnoApp()
