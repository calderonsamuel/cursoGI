guardarTablaUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("boton"), label = "Guardar", icon = icon("save"))
  )
}

guardarTablaServer <- function(id, data, nombre_carpeta, nombre_usuario){
  moduleServer(id, function(input, output, session){
    observeEvent(input$boton, {
      data <- data()
      file <- file.path("data", nombre_carpeta, nombre_usuario())
      write_csv(data, paste0(file, ".csv"))
      showNotification("Guardado correctamente", type = "message", duration = 3)
    })
  })
}

guardarTablaApp <- function(){
  ui <- fluidPage(
    guardarTablaUI("myTestId")
  )
  server <- function(input, output, session){
    data <- reactive(tibble(x = 1, y = 2))
    nombre_usuario <- reactive("alumno1")
    nombre_carpeta <- "tarea1"
    guardarTablaServer("myTestId", data, nombre_carpeta, nombre_usuario)
  }
  shinyApp(ui, server)
}

# guardarTablaApp()
