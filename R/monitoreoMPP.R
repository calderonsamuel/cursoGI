monitoreoMPPUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        style = "overflow-y:scroll; max-height: 600px; position:relative;",
        uiOutput(ns("inputs_funciones"))
      ),
      mainPanel(
        uiOutput(ns("tabla"))
      )
    )
  )
}

monitoreoMPPServer <- function(id, nombre_usuario) {
  moduleServer(id, function(input, output, session) {
    data_funciones <- reactive({
      file <- paste0("data/tarea2/alineamiento_MPP_RI/", nombre_usuario(), ".csv")
      read_csv(file)
    })
    
    id_funcion <- reactive(paste0("funcion", seq_len(nrow(data_funciones()))))
    id_estado <- reactive(paste0("estado_", id_funcion()))
    id_descripcion <- reactive(paste0("descripcion_", id_funcion()))
    id_mejora <- reactive(paste0("mejora_", id_funcion()))
    label_funcion <- reactive(paste0("Función Nº ", seq_along(id_funcion())))
    
    output$inputs_funciones <- renderUI({
      ns <- session$ns
      argumentos <- list(id_estado(), 
                         id_descripcion(),
                         id_mejora(),
                         label_funcion())
      pmap(argumentos, function(estado, descripcion, mejora, label){
        choices <- c("", "Logrado", "En proceso", "En inicio")
        tagList(
          wellPanel(
            selectInput(ns(estado), paste0("Estado de ", label), choices = choices),
            textAreaInput(ns(descripcion), paste0("Descripción del nivel de avance")),
            textAreaInput(ns(mejora), paste0("Posibilidad de mejora al 2021"))
          )
        )
      })
    })
    
    data_monitoreo <- reactive({
      tibble(
        `Número de función` = label_funcion(),
        Estado = map_chr(id_estado(), ~input[[.x]]),
        `Descripción del nivel de avance` = map_chr(id_descripcion(), ~input[[.x]]),
        `Posibilidad de mejora al 2021` = map_chr(id_mejora(), ~input[[.x]])
      ) %>% 
        bind_cols(data_funciones(), .)
    })
    
    output$tabla <- renderUI({
      data_monitoreo() %>% 
        flextable() %>% 
        htmltools_value()
    })
      
  })
}

monitoreoMPPApp <- function(){
  ui <- fluidPage(
    monitoreoMPPUI("myTestId")
  )
  server <- function(input, output, session) {
    nombre_usuario <- reactive("alumno1")
    monitoreoMPPServer("myTestId", nombre_usuario)
  }
  shinyApp(ui, server)
}

# monitoreoMPPApp()