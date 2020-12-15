moduloCursoUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("ui"))
  )
  
  # if(tipo_usuario == "alumno"){
  #   tagList(
  #     moduloAlumnoUI(ns(id))
  #   )
  # } else if (tipo_usuario == "docente") {
  #   tagList(
  #     moduloDocenteUI(ns(id))
  #   )
  # }
}

moduloCursoServer <- function(id, tipo_usuario) {
  moduleServer(id, function(input, output, session) {
    
    output$ui <- renderUI({
      if(tipo_usuario() == "alumno"){
          moduloAlumnoUI(id)
      } else if (tipo_usuario() == "docente") {
          moduloDocenteUI(id)
      }
    })
    reactive({
      if(tipo_usuario() == "alumno") {
        moduloAlumnoServer(id)
      } else if (tipo_usuario() == "docente") {
        moduloDocenteServer(id)
      }
    })
    
  })
}

moduloCursoApp <- function(){
  ui <- fluidPage(
    # uiOutput("test")
    moduloCursoUI("myTestId")
  )
  server <- function(input, output, session) {
    tipo_usuario <- reactive("alumno")
    # output$test <- renderUI(moduloCursoUI("myTestId", tipo_usuario))
    
    moduloCursoServer("myTestId", tipo_usuario)
  }
  shinyApp(ui, server)
}

# moduloCursoApp()
