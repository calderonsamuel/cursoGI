moduloDocenteUI <- function(id) {
  ns <- NS(id)
  tagList(
    navlistPanel(
      widths = c(3, 9),
      "Modulo I: Proyecto Educativo Institucional",
      tabPanel("Evaluación de Foro 1",
               tags$h3("Evaluación del Foro 1"),
               tags$p("A continuación se te presenta la evaluación de la participación en el Foro 1. 
                      Toma en cuenta la rúbrica.")
      ),
      tabPanel(
        title = "Evaluación de Tarea 1",
        evalTarea1UI(ns("evaltarea1"))
      ),
      "Modulo II: Manual de Perfil de Puesto",
      tabPanel("Evaluación de Foro 2"),
      tabPanel(
        title = "Evaluación de Tarea 2",
        evalTarea2UI(ns("evaltarea2"))
      ),
      "Modulo III: Plan Anual de Trabajo",
      tabPanel("Evaluación de Foro 3"),
      tabPanel(
        title = "Evaluación de Tarea 3",
        evalTarea3UI(ns("evaltarea3"))
      ),
      tabPanel(
        title = "Evaluación de Tarea 4",
        evalTarea4UI(ns("evaltarea4"))
      )
    )
  )
}

moduloDocenteServer <- function(id, choices_alumnos) {
  moduleServer(id, function(input, output, session) {
    evalTarea1Server("evaltarea1", choices_alumnos)
    evalTarea2Server("evaltarea2")
    evalTarea3Server("evaltarea3")
    evalTarea4Server("evaltarea4")
  })
}

moduloDocenteApp <- function(){
  ui <- fluidPage(
    moduloDocenteUI("myTestId")
  )
  server <- function(input, output, session) {
    choices_alumnos <- reactive(c("alumno1", "alumno2", "alumno3", "alumno4", "alumno5"))
    moduloDocenteServer("myTestId", choices_alumnos)
  }
  shinyApp(ui, server)
}

# moduloDocenteApp()
