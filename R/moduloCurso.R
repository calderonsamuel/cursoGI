moduloCursoUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("ui"))
  )
}

moduloCursoServer <- function(id, tipo_usuario, choices_alumnos, nombre_usuario) {
  moduleServer(id, function(input, output, session) {
    output$ui <- renderUI({
      ns <- session$ns
      if(tipo_usuario() == "alumno"){
          moduloAlumnoUI(ns("moduloAlumno"))
      } else if (tipo_usuario() == "docente") {
          moduloDocenteUI(ns("moduloDocente"))
      }
    })
    
    observeEvent(tipo_usuario() == "alumno",{
      moduloAlumnoServer("moduloAlumno", nombre_usuario)
    })
    
    observeEvent(tipo_usuario() == "docente",{
      moduloDocenteServer("moduloDocente", choices_alumnos)
    })
  })
}

moduloCursoApp <- function(){
  ui <- fluidPage(
    theme = bs_theme(version = 3, bootswatch = "flatly"),
    moduloCursoUI("myTestId")
  )
  server <- function(input, output, session) {
    tipo_usuario <- reactive("docente")
    choices_alumnos <- reactive(c("alumno1", "alumno2", "alumno3", "alumno4", "alumno5"))
    nombre_usuario <- reactive("docente1")
    moduloCursoServer("myTestId", tipo_usuario, choices_alumnos, nombre_usuario)
  }
  shinyApp(ui, server)
}

# moduloCursoApp()
