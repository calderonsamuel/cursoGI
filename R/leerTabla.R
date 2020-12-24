leerTablaUI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

leerTablaServer <- function(id, nombre_carpeta, alumno_elegido) {
  moduleServer(id, function(input, output, session) {
    reactive({
      file <- file.path("data", nombre_carpeta, alumno_elegido())
      read_csv(paste0(file, ".csv"))
    })
  })
}

leerTablaApp <- function(){
  ui <- fluidPage(
    selectInput("elegir", "Elije", choices = c("alumno1", "alumno2", "alumno3", "alumno4", "alumno5")),
    uiOutput("tabla")
  )
  server <- function(input, output, session) {
    nombre_carpeta <- "tarea1/aliEstrategico"
    # alumno_elegido <- reactive("alumno1")
    output$tabla <- renderUI({
      leerTablaServer("myTestId", nombre_carpeta, reactive(input$elegir))() %>% 
        flextable() %>% 
        htmltools_value()
      })
  }
  shinyApp(ui, server)
}

# leerTablaApp()
