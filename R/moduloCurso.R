moduloCursoUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("ui"))
  )
}

moduloCursoServer <- function(id, tipo_usuario) {
  moduleServer(id, function(input, output, session) {
    # moduloAlumnoServer("moduloAlumno")
    output$ui <- renderUI({
      ns <- session$ns
      if(tipo_usuario() == "alumno"){
          moduloAlumnoUI(ns("moduloAlumno"))
      } else if (tipo_usuario() == "docente") {
          moduloDocenteUI(ns("moduloDocente"))
      }
    })
    
    observeEvent(tipo_usuario() == "alumno",{
      moduloAlumnoServer("moduloAlumno")
    })
    
    observeEvent(tipo_usuario() == "docente",{
      moduloDocenteServer("moduloDocente")
    })
  })
}

moduloCursoApp <- function(){
  ui <- fluidPage(
    moduloCursoUI("myTestId")
  )
  server <- function(input, output, session) {
    tipo_usuario <- reactive("alumno")
    
    moduloCursoServer("myTestId", tipo_usuario)
  }
  shinyApp(ui, server)
}

moduloCursoApp()
