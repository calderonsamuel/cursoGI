moduloDocenteUI <- function(id) {
  ns <- NS(id)
  tagList(
    navlistPanel(
      "Modulo I: Proyecto Educativo Institucional",
      tabPanel("Evaluación de Foro 1",
               tags$h3("Evaluación del Foro 1"),
               tags$p("A continuación se te presenta la evaluación de la participación en el Foro 1. Toma en cuenta la rúbrica.")),
      tabPanel("Evaluación de Tarea 1"),
      "Modulo II: Manual de Perfil de Puesto",
      tabPanel("Evaluación de Foro 2"),
      tabPanel("Evaluación de Tarea 2"),
      "Modulo III: Plan Anual de Trabajo",
      tabPanel("Evaluación de Foro 3"),
      tabPanel("Evaluación de Tarea 3"),
      tabPanel("Evaluación de Tarea 4")
    )
  )
}

moduloDocenteServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}

moduloDocenteApp <- function(){
  ui <- fluidPage(
    moduloDocenteUI("myTestId")
  )
  server <- function(input, output, session) {
    moduloDocenteServer("myTestId")
  }
  shinyApp(ui, server)
}

# moduloDocenteApp()
