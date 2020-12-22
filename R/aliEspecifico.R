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
            uiOutput(ns("tabla"))
          )
        )
      ),
      tabPanel(
        title = "Metas",
        sidebarLayout(
          sidebarPanel(
            uiOutput(ns("metas_input"))
          ),
          mainPanel(
            
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
        color(j = "Objetivo Estratégico", color = scales::col_factor(color_matriz$color2, levels = color_matriz$`Objetivo Estratégico`)) %>%
        bg(j = "Objetivo Estratégico", bg = scales::col_factor(color_matriz$bg, levels = color_matriz$`Objetivo Estratégico`)) %>% 
        color(j = "Objetivos Específicos", color = scales::col_factor(color_matriz$color2, levels = color_matriz$`Objetivos Específicos`)) %>%
        bg(j = "Objetivos Específicos", bg = scales::col_factor(color_matriz$bg, levels = color_matriz$`Objetivos Específicos`)) %>% 
        htmltools_value()
    })
    
    output$metas_input <- renderUI({
      ns <- session$ns
      data <- data_aliEst()
      
      oest1 <- data %>% filter(Categoría == "Misión") %>% pull(`Objetivos Específicos`)
      oest2 <- data %>% filter(Categoría == "Visión") %>% pull(`Objetivos Específicos`)
      oest3 <- data %>% filter(Categoría == "Valores") %>% pull(`Objetivos Específicos`)
      
      salida <- tagList()
        
      for (i in seq_along(oest1)) {
        salida[[i]] <- tagList()
        for (j in seq_len(3)) {
          id_meta <- paste0("meta_1_", i, "_", j)
          id_year <- paste0("year_1_", i, "_", j)
          salida[[i]][[j]] <- tagList(
            fluidRow(
              column(8, textAreaInput(ns(id_meta), paste0("Meta ", j, " del Obj. Específico 1.",i))),
              column(4, selectInput(ns(id_year), "Año", choices = c("Año 1", "Año 2", "Año 3")))
            )
          )
        }
      }
      
      salida
      
      # selectInput(ns("meta_test"), "Seleccione Obj. Específico", choices = data$`Objetivos Específicos`)
    })
  })
}

aliEspecificoApp <- function(){
  ui <- fluidPage(
    aliEspecificoUI("myTestId")
  )
  server <- function(input, output, session) {
    nombre_usuario <- reactive("alumno1")
    aliEspecificoServer("myTestId", nombre_usuario)
  }
  shinyApp(ui, server)
}

aliEspecificoApp()
