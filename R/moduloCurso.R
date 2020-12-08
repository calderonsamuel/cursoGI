moduloCursoUI <- function(id, tipo_usuario) {
  ns <- NS(id)
  if(tipo_usuario == "alumno"){
    tagList(
      moduloAlumnoUI(ns(id))
    )
  } else if (tipo_usuario == "docente") {
    tagList(
      moduloDocenteUI(ns(id))
    )
  }
}

moduloCursoServer <- function(id, tipo_usuario) {
  moduleServer(id, function(input, output, session) {
    if(tipo_usuario == "alumno") {
      moduloAlumnoServer(id)
    } else if (tipo_usuario == "docente") {
      moduloDocenteServer(id)
    }
  })
}

moduloCursoApp <- function(tipo_usuario){
  ui <- fluidPage(
    moduloCursoUI("myTestId", tipo_usuario)
  )
  server <- function(input, output, session) {
    moduloCursoServer("myTestId", tipo_usuario)
  }
  shinyApp(ui, server)
}

moduloCursoApp("docente")
