moduloAlumnoUI <- function(id) {
  ns <- NS(id)
  tagList(
    navlistPanel(
      "Inicio",
      tabPanel("Evaluación de entrada",
               tags$h3("¡Bienvenido a la evaluación de entrada!")),
      "Modulo I: Proyecto Educativo Institucional",
      tabPanel("Video 1: Evaluación del PEI"),
      tabPanel("Foro 1: Alineamiento y cumplimiento del PEI en la actualidad"),
      tabPanel("Tarea 1: Alineamiento de los componentes del PEI"),
      "Modulo II: Manual de Perfil de Puesto",
      tabPanel("Video 2: Evaluación del MPP"),
      tabPanel("Foro 2: Alineamiento y cumplimiento de perfiles"),
      tabPanel("Tarea 2: Alineamiento de los componentes del MPP"),
      "Modulo III: Plan Anual de Trabajo",
      tabPanel("Video 3: Evaluación del PAT alineado"),
      tabPanel("Foro 3: Importancia de la evaluación operativa para el éxito de la gestión institucional"),
      tabPanel("Tarea 3: Evaluación y monitoreo del PAT en coherencia con evaluación del PEI y MPP"),
      tabPanel("Tarea 4: Matrices de alineamiento del PAT 2021"),
      "Cierre",
      tabPanel("Evaluación de salida"),
      tabPanel("Encuesta de satisfacción")
    )
    )
}

moduloAlumnoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}

moduloAlumnoApp <- function(){
  ui <- fluidPage(
    moduloAlumnoUI("myTestId")
  )
  server <- function(input, output, session) {
    moduloAlumnoServer("myTestId")
  }
  shinyApp(ui, server)
}

# moduloAlumnoApp()
