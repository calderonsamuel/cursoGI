monitoreoPEI_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 5,
        style = "overflow-y:scroll; max-height: 600px; position:relative;",
        uiOutput(ns("inputs"))
      ),
      mainPanel(
        width = 7,
        style = "overflow-y:scroll; max-height: 600px; position:relative;",
        uiOutput(ns("tabla")),
        guardarTablaUI(ns("guardar"))
      )
    )
  )
}

monitoreoPEI_Server <- function(id, nombre_usuario) {
  moduleServer(id, function(input, output, session) {
    
    color_table <- read_csv("data/color_table.csv")
    
    data_aliEsp <- reactive({
      file <- paste0("data/tarea1/aliEspecifico/", nombre_usuario(), ".csv")
      read_csv(file) %>% select(-Año)
    })
    
    output$inputs <- renderUI({
      ns <- session$ns
      metas <- data_aliEsp()$Metas
      salida <- tagList()
      for (i in seq_along(metas)) {
        mediosv_id <- paste0("mediosv", i)
        estado_id <- paste0("estado", i)
        descripcion_id <- paste0("descripcion", i)
        acciones_id <- paste0("acciones", i)
        placeholder <- metas[i]
        
        salida[[i]] <- tagList(
          wellPanel(
            fluidRow(
              column(6, textInput(ns(mediosv_id), "Medio de Verificación", placeholder = placeholder)),
              column(6, selectInput(ns(estado_id), "Estado de avance", choices = c("", "En inicio", "En proceso", "Logrado")))
            ),
            fluidRow(
              column(6, textAreaInput(ns(descripcion_id), NULL, placeholder = "Descripción de estado de avance")),
              column(6, textAreaInput(ns(acciones_id), NULL, placeholder = "Acciones a implementar"))
            )
          )
        )
      }
      salida
    })
    
    data_inputs <- reactive({
      metas <- data_aliEsp()$Metas
      mediosv <- character()
      estado <- character()
      descripcion <- character()
      acciones <- character()
      
      for (i in seq_along(metas)) {
        mediosv_id <- paste0("mediosv", i)
        estado_id <- paste0("estado", i)
        descripcion_id <- paste0("descripcion", i)
        acciones_id <- paste0("acciones", i)
        
        mediosv[i] <- input[[mediosv_id]]
        estado[i] <- input[[estado_id]]
        descripcion[i] <- input[[descripcion_id]]
        acciones[i] <- input[[acciones_id]]
      }
      data <- tibble(Metas = metas,
             `Medios de verificación` = mediosv,
             `Estado de avance` = estado,
             `Descripción del estado de avance` = descripcion,
             `Acciones a implementar` = acciones) %>% 
        filter(Metas != "")
      data_aliEsp() %>% left_join(data)
    })
    
    output$tabla <- renderUI({
      color_matriz <- data_inputs() %>% left_join(color_table, by = c("Componente"="componente"))
      columnas_color <- c("Objetivos Específicos", 
                          "Metas", 
                          "Medios de verificación", 
                          "Estado de avance", 
                          "Descripción del estado de avance",
                          "Acciones a implementar")
      data_inputs() %>% 
        select(-Componente) %>% 
        flextable() %>% 
        autofit() %>%
        theme_vanilla() %>%
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        merge_v(j = "Objetivos Específicos") %>% 
        color(j = columnas_color, 
              color = scales::col_factor(color_matriz$color2, levels = color_matriz$`Objetivos Específicos`), 
              source = "Objetivos Específicos") %>%
        bg(j = columnas_color, 
           bg = scales::col_factor(color_matriz$bg, levels = color_matriz$`Objetivos Específicos`), 
           source = "Objetivos Específicos") %>% 
        htmltools_value()
    })
    
    guardarTablaServer("guardar", data_inputs, "tarea1/monitoreoPEI", nombre_usuario)
    
  })
}

monitoreoPEI_App <- function(){
  ui <- fluidPage(
    monitoreoPEI_UI("myTestId")
  )
  server <- function(input, output, session) {
    nombre_usuario <- reactive("alumno1")
    monitoreoPEI_Server("myTestId", nombre_usuario)
  }
  shinyApp(ui, server)
}

# monitoreoPEI_App()
