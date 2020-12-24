evaluacionPEI_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        textAreaInput(ns("cumplimiento_oesp"), "Descripción de cumplimiento de objetivos específicos"),
        textAreaInput(ns("cumplimiento_oest"), "Determinación de avance de los objetivos estratégicos"),
        textAreaInput(ns("cumplimiento_identidad"), "Descripción de la evaluación institucional (misión, visión y valores)")
      ),
      mainPanel(
        style = "overflow-y:scroll; max-height: 600px; position:relative;",
        uiOutput(ns("tabla")),
        guardarTablaUI(ns("guardar"))
      )
    )
  )
}

evaluacionPEI_Server <- function(id, nombre_usuario) {
  moduleServer(id, function(input, output, session) {
    
    color_table <- read_csv("data/color_table.csv")
    
    data <- reactive({
      file <- paste0("data/tarea1/monitoreoPEI/", nombre_usuario(), ".csv")
      read_csv(file)
    })
    
    data_evaluacion <- reactive({
      data() %>% 
        filter(`Descripción del estado de avance` != "") %>% 
        select(`Descripción del estado de avance`, Componente) %>% 
        mutate(`Descripción de cumplimiento de objetivos específicos` = input$cumplimiento_oesp,
               `Determinación de avance de los objetivos estratégicos` = input$cumplimiento_oest,
               `Descripción de la evaluación institucional` = input$cumplimiento_identidad)
    })
    
    output$tabla <- renderUI({
      color_matriz <- data_evaluacion() %>% left_join(color_table, by = c("Componente"="componente"))
      columnas_color <- data_evaluacion() %>% select(-Componente) %>% names()
      
      data_evaluacion() %>% 
        select(-Componente) %>% 
        flextable() %>% 
        theme_vanilla() %>%
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        merge_v(c("Descripción de cumplimiento de objetivos específicos",
                  "Determinación de avance de los objetivos estratégicos",
                  "Descripción de la evaluación institucional")) %>% 
        color(j = columnas_color, 
              color = scales::col_factor(color_matriz$color2, levels = color_matriz$`Descripción del estado de avance`), 
              source = "Descripción del estado de avance") %>%
        bg(j = columnas_color, 
           bg = scales::col_factor(color_matriz$bg, levels = color_matriz$`Descripción del estado de avance`), 
           source = "Descripción del estado de avance") %>% 
        htmltools_value()
    })
    
    guardarTablaServer("guardar", data_evaluacion, "tarea1/evaluacionPEI", nombre_usuario)
    
  })
}

evaluacionPEI_App <- function(){
  ui <- fluidPage(
    evaluacionPEI_UI("myTestId")
  )
  server <- function(input, output, session) {
    nombre_usuario <- reactive("alumno1")
    evaluacionPEI_Server("myTestId", nombre_usuario)
  }
  shinyApp(ui, server)
}

# evaluacionPEI_App()
