aliEspecificoUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(type = "pills",
      tabPanel(
        title = "Objetivos específicos",
        sidebarLayout(
          sidebarPanel(
            tabsetPanel(type = "pills",
              tabPanel(
                title = "Obj. Est. 1",
                textAreaInput(ns("oesp1_1"), NULL, placeholder = "Objetivo Específico 1.1"),
                textAreaInput(ns("oesp1_2"), NULL, placeholder = "Objetivo Específico 1.2"),
                textAreaInput(ns("oesp1_3"), NULL, placeholder = "Objetivo Específico 1.3"),
                textAreaInput(ns("oesp1_4"), NULL, placeholder = "Objetivo Específico 1.4"),
                textAreaInput(ns("oesp1_5"), NULL, placeholder = "Objetivo Específico 1.5")
              ),
              tabPanel(
                title = "Obj. Est. 2",
                textAreaInput(ns("oesp2_1"), NULL, placeholder = "Objetivo Específico 2.1"),
                textAreaInput(ns("oesp2_2"), NULL, placeholder = "Objetivo Específico 2.2"),
                textAreaInput(ns("oesp2_3"), NULL, placeholder = "Objetivo Específico 2.3"),
                textAreaInput(ns("oesp2_4"), NULL, placeholder = "Objetivo Específico 2.4"),
                textAreaInput(ns("oesp2_5"), NULL, placeholder = "Objetivo Específico 2.5")
              ),
              tabPanel(
                title = "Obj. Est. 3",
                textAreaInput(ns("oesp3_1"), NULL, placeholder = "Objetivo Específico 3.1"),
                textAreaInput(ns("oesp3_2"), NULL, placeholder = "Objetivo Específico 3.2"),
                textAreaInput(ns("oesp3_3"), NULL, placeholder = "Objetivo Específico 3.3"),
                textAreaInput(ns("oesp3_4"), NULL, placeholder = "Objetivo Específico 3.4"),
                textAreaInput(ns("oesp3_5"), NULL, placeholder = "Objetivo Específico 3.5")
              )
            )
          ),
          mainPanel(
            style = "overflow-y:scroll; max-height: 600px; position:relative;",
            uiOutput(ns("tabla")),
            guardarTablaUI(ns("guardar_oesp"))
          )
        )
      ),
      tabPanel(
        title = "Metas",
        sidebarLayout(
          sidebarPanel(
            style = "overflow-y:scroll; max-height: 600px; position:relative;",
            uiOutput(ns("metas_input"))
          ),
          mainPanel(
            style = "overflow-y:scroll; max-height: 600px; position:relative;",
            uiOutput(ns("metas_tabla")),
            guardarTablaUI(ns("guardar_metas"))
          )
        )
      )
    )
  )
}

aliEspecificoServer <- function(id, nombre_usuario) {
  moduleServer(id, function(input, output, session) {
    
    aliEst_inicial <- reactive({
      file <- paste0("data/tarea1/aliEstrategico/", nombre_usuario(), ".csv")
      read_csv(file)
    })
    
    color_table <- read_csv("data/color_table.csv")
    
    data_aliEst <- reactive({
      oest1 <- aliEst_inicial() %>% filter(Categoría == "Misión") 
      oest2 <- aliEst_inicial() %>% filter(Categoría == "Visión")
      oest3 <- aliEst_inicial() %>% filter(Categoría == "Valores")
      
      seq_oest1 <- seq_along(oest1$Categoría)
      seq_oest2 <- seq_along(oest2$Categoría)
      seq_oest3 <- seq_along(oest3$Categoría)
      
      oesp_1 <- c(input$oesp1_1, input$oesp1_2, input$oesp1_3, input$oesp1_4, input$oesp1_5)[seq_oest1]
      oesp_2 <- c(input$oesp2_1, input$oesp2_2, input$oesp2_3, input$oesp2_4, input$oesp2_5)[seq_oest2]
      oesp_3 <- c(input$oesp3_1, input$oesp3_2, input$oesp3_3, input$oesp3_4, input$oesp3_5)[seq_oest3]
      
      oest1 <- oest1 %>% mutate("Objetivos Específicos" = oesp_1)
      oest2 <- oest2 %>% mutate("Objetivos Específicos" = oesp_2)
      oest3 <- oest3 %>% mutate("Objetivos Específicos" = oesp_3)
      
      bind_rows(oest1, oest2, oest3)
    })
    
    output$tabla <- renderUI({
    color_matriz <- data_aliEst() %>% left_join(color_table, by = c("Componente"="componente"))
      data_aliEst() %>% 
        select(`Objetivo Estratégico`, `Objetivos Específicos`) %>% 
        flextable() %>% 
        autofit() %>%
        theme_vanilla() %>%
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        color(j = c("Objetivo Estratégico","Objetivos Específicos"), color = scales::col_factor(color_matriz$color2, levels = color_matriz$`Objetivo Estratégico`), source = "Objetivo Estratégico") %>%
        bg(j = c("Objetivo Estratégico", "Objetivos Específicos"), bg = scales::col_factor(color_matriz$bg, levels = color_matriz$`Objetivo Estratégico`), source = "Objetivo Estratégico") %>% 
        htmltools_value()
    })
    
    guardarTablaServer("guardar_oesp", data_aliEst, "tarea1/data_aliEst", nombre_usuario)
    
    output$metas_input <- renderUI({
      ns <- session$ns
      data <- data_aliEst()
      
      oest1 <- data %>% filter(Categoría == "Misión") %>% pull(`Objetivos Específicos`)
      oest2 <- data %>% filter(Categoría == "Visión") %>% pull(`Objetivos Específicos`)
      oest3 <- data %>% filter(Categoría == "Valores") %>% pull(`Objetivos Específicos`)
      
      salida <- tagList()
        
      listado <- list(oest1, oest2, oest3)
      for (elemento in seq_along(listado)) {
        oest <- listado[[elemento]]
        salida[[elemento]] <- tagList()
        for (i in seq_along(oest)) {
          salida[[elemento]][[i]] <- tagList()
          for (j in seq_len(3)) {
            id_meta <- paste0("meta_", elemento, "_", i, "_", j)
            id_year <- paste0("year_", elemento, "_", i, "_", j)
            salida[[elemento]][[i]][[j]] <- tagList(
              fluidRow(
                column(8, textAreaInput(ns(id_meta), NULL, placeholder = paste0("Meta ", j, " del Obj. Específico ", elemento ,".",i))),
                column(4, selectInput(ns(id_year), NULL, choices = c("","Año 1", "Año 2", "Año 3")))
              )
            )
          }
          salida[[elemento]][[i]] <- tagList(
            wellPanel(
              salida[[elemento]][[i]]
            )
          )
        }
      }
      
      tagList(
        fluidRow(
          column(8, tags$h4("Metas")),
          column(4, tags$h4("Año"))
        ),
        salida
      )
    })
    
    data_aliEst_guardada <- reactive({
      file <- paste0("data/tarea1/data_aliEst/", nombre_usuario(), ".csv")
      read_csv(file)
    })
    
    data_metas <- reactive({
      data <- data_aliEst_guardada()
      data_join <- data %>% select(`Objetivos Específicos`, Componente)
      oest1 <- data %>% filter(Categoría == "Misión") %>% pull(`Objetivos Específicos`)
      oest2 <- data %>% filter(Categoría == "Visión") %>% pull(`Objetivos Específicos`)
      oest3 <- data %>% filter(Categoría == "Valores") %>% pull(`Objetivos Específicos`)
      listado <- list(oest1, oest2, oest3)
      listado_data <- list()
      for (oest in seq_along(listado)) {
        listado_oest <- list()
        for (oesp in seq_along(listado[[oest]])) {
          obj_esp <- listado[[oest]][oesp]
          metas <- character()
          year <- character()
          
          for (num in seq_len(3)) {
            id_num_meta <- paste0("meta_", oest, "_", oesp, "_", num)
            id_num_year <- paste0("year_", oest, "_", oesp, "_", num)
            metas[id_num_meta] <- input[[id_num_meta]]
            year[id_num_year] <- input[[id_num_year]]
            
          }
          listado_oest[[oesp]] <- tibble(`Objetivos Específicos` = obj_esp, Metas = metas, Año = year)
        }
        
        listado_data[[oest]] <- do.call(bind_rows, listado_oest)
      }
      do.call(bind_rows, listado_data) %>% left_join(data_join)
    })
    
    output$metas_tabla <- renderUI({
      color_matriz <- data_metas() %>% left_join(color_table, by = c("Componente"="componente"))
      data_metas() %>% 
        select(-Componente) %>% 
        flextable() %>% 
        autofit() %>%
        theme_vanilla() %>%
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        merge_v(j = "Objetivos Específicos") %>% 
        color(j = c("Objetivos Específicos", "Metas", "Año"), 
              color = scales::col_factor(color_matriz$color2, levels = color_matriz$`Objetivos Específicos`), 
              source = "Objetivos Específicos") %>%
        bg(j = c("Objetivos Específicos", "Metas", "Año"), 
           bg = scales::col_factor(color_matriz$bg, levels = color_matriz$`Objetivos Específicos`), 
           source = "Objetivos Específicos") %>% 
        htmltools_value()
    })
    
    guardarTablaServer("guardar_metas", data_metas, "tarea1/aliEspecifico", nombre_usuario)
    
  })
}

aliEspecificoApp <- function(){
  ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "flatly"),
    aliEspecificoUI("myTestId")
  )
  server <- function(input, output, session) {
    nombre_usuario <- reactive("alumno1")
    aliEspecificoServer("myTestId", nombre_usuario)
  }
  shinyApp(ui, server)
}

# aliEspecificoApp()
