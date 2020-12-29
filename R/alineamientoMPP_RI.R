alineamientoMPP_RI_UI <- function(id) {
  puestos <- c("Director General", "Otros")
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        title = "Competencias del MPP",
        sidebarLayout(
          sidebarPanel(
            tabsetPanel(
              tabPanel(
                title = "Director General",
                numericInput(ns("n_competencias"), 
                             "Cantidad de competencias del cargo", 
                             value = 2),
                uiOutput(ns("competencias_dir"))
              )
            )
          ),
          mainPanel(
            uiOutput(ns("tabla")),
            guardarTablaUI(ns("guardar_comp"))
          )
        )
      ),
      tabPanel(
        title = "Funciones del RI",
        sidebarLayout(
          sidebarPanel(
            uiOutput(ns("inputs_funciones"))
          ),
          mainPanel(
            uiOutput(ns("tabla_funciones")),
            guardarTablaUI(ns("guardar_funciones"))
          )
        )
      )
    )
  )
}

alineamientoMPP_RI_Server <- function(id, nombre_usuario) {
  moduleServer(id, function(input, output, session) {
    
    competencia_id <- reactive(paste0("competencia", seq_len(input$n_competencias)))
    competencia_label <- reactive(paste("Competencia", seq_len(input$n_competencias)))
    
    output$competencias_dir <- renderUI({
      ns <- session$ns
      map2(.x = competencia_id(), 
           .y = competencia_label(), 
           ~textAreaInput(ns(.x), .y, value = isolate(input[[.x]])))
    })
    
    data_competencias <- reactive({
      competencias <- map_chr(competencia_id(), ~input[[.x]])
      tibble(Puesto = "Director General",
             `Competencias del MPP` = competencias)
    })
    
    output$tabla <- renderUI({
      data_competencias() %>% 
        flextable() %>% 
        merge_v("Puesto") %>% 
        htmltools_value()
    })
    
    guardarTablaServer("guardar_comp", data_competencias, "tarea2/data_comp", nombre_usuario)
    
    data_comp_cargada <- reactive({
      file <- paste0("data/tarea2/data_comp/", nombre_usuario(),".csv")
      read_csv(file)
    })
    
    funcion_id <- reactive({
      paste0("funcion", seq_along(data_comp_cargada()$Puesto)) %>% 
        map(~paste0(.x, c("_1", "_2"))) %>% 
        unlist()
    })
    
    funcion_ph <- reactive({
      data_comp_cargada()[["Competencias del MPP"]] %>% 
        map(~paste(c("Primera función de:", "Segunda función de:"), .x)) %>% 
        unlist()
    })
    
    output$inputs_funciones <- renderUI({
      ns <- session$ns
      map2(funcion_id(), funcion_ph(), ~textAreaInput(ns(.x), NULL, placeholder = .y))
    })
    
    data_funciones <- reactive({
      tibble(`Competencias del MPP` = rep(data_comp_cargada()[["Competencias del MPP"]], each = 2),
             `Funciones del RI` = map_chr(funcion_id(), ~input[[.x]])) %>% 
        left_join(data_comp_cargada(), .)
    })
    
    output$tabla_funciones <- renderUI({
      data_funciones() %>% 
        flextable() %>% 
        merge_v(c("Puesto", "Competencias del MPP")) %>% 
        htmltools_value()
    })
    
    guardarTablaServer("guardar_funciones", data_funciones, "tarea2/alineamiento_MPP_RI", nombre_usuario)
    
  })
}

alineamientoMPP_RI_App <- function(){
  ui <- fluidPage(
    alineamientoMPP_RI_UI("myTestId")
  )
  server <- function(input, output, session) {
    nombre_usuario <- reactive("alumno1")
    alineamientoMPP_RI_Server("myTestId", nombre_usuario)
  }
  shinyApp(ui, server)
}

# alineamientoMPP_RI_App()
