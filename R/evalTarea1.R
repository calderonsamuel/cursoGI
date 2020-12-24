evalTarea1UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(3,actionButton(ns("reset"), icon("broom", "fa-1x"))),
          column(3, guardarTablaUI(ns("guardar")))
        ),
        checkboxGroupInput(
          inputId = ns("rubrica1"), 
          label = "Los objetivos estratégicos del PEI presentan",
          choices = c("Alineamiento con misión",
                      "Alineamiento con visión",
                      "Alineamiento con valores institucionales",
                      "Alineamiento con los componentes de gestión")
        ),
        checkboxGroupInput(
          inputId = ns("rubrica2"), 
          label = "Los objetivos específicos del PEI presentan",
          choices = c("Alineamiento con objetivos estratégicos",
                      "Alineamiento con metas institucionales",
                      "Alineamiento con los componentes de gestión",
                      "Coherencia con temporalidad para la ejecución de metas")
         ),
        checkboxGroupInput(
          inputId = ns("rubrica3"),
          label = "El monitoreo del PEI presenta",
          choices = c("Construcción a partir de los objetivos específicos",
                      "Estado de avance en el cumplimiento de metas anuales",
                      "Descripción del estado de avance",
                      "Acciones para implementar a partir del avance")
        ),
        checkboxGroupInput(
          inputId = ns("rubrica4"),
          label = "La evaluación del PEI presenta",
          choices = c("Descripción del cumplimiento de metas anuales",
                      "Descripción del cumplimiento de objetivos estratégicos",
                      "Descripción del cumplimiento de objetivos específicos",
                      "Descripción de la evaluación institucional")
        )
      ),
      mainPanel(
        style = "overflow-y:scroll; max-height: 600px; position:relative;",
          uiOutput(ns("elegir_alumno")),
        tabsetPanel(type = "pills",
          tabPanel(
            title = "Alineamiento estratégico",
            uiOutput(ns("tabla_aliEstrategico"))
          ),
          tabPanel(
            title = "Alineamiento específico",
            uiOutput(ns("tabla_aliEspecifico"))
          ),
          tabPanel(
            title = "Monitoreo del PEI",
            uiOutput(ns("tabla_monitoreoPEI"))
          ),
          tabPanel(
            title = "Evaluación del PEI",
            uiOutput(ns("tabla_evaluacionPEI"))
          )
        )
      )
    )
  )
}

evalTarea1Server <- function(id, choices_alumnos){
  moduleServer(id, function(input, output, session){

    color_table <- read_csv("data/color_table.csv")   
    alumno_elegido <- reactive(input$alumno_elegido)
    
    output$elegir_alumno <- renderUI({
      ns <- session$ns
      selectInput(ns("alumno_elegido"), "Seleccione participante", choices = choices_alumnos())
    })
    
    data_aliEstrategico <- reactive({
      file <- file.path("data", "tarea1", "aliEstrategico", alumno_elegido())
      read_csv(paste0(file, ".csv"))
    })
    
    data_aliEspecifico <- reactive({
      file1 <- file.path("data", "tarea1", "aliEspecifico", alumno_elegido())
      file2 <- paste0("data/tarea1/data_aliEst/", alumno_elegido(), ".csv")
      data1 <- read_csv(paste0(file1, ".csv"))
      data2 <- read_csv(file2)
      
      left_join(data1, data2) %>% 
        select(`Objetivo Estratégico`, `Objetivos Específicos`, Metas, Año, Componente)
    })
    
    data_monitoreoPEI <- reactive({
      file <- paste0("data/tarea1/monitoreoPEI/", alumno_elegido(), ".csv")
      read_csv(file)
    })
    
    data_evaluacionPEI <- reactive({
      file <- paste0("data/tarea1/evaluacionPEI/", alumno_elegido(), ".csv")
      read_csv(file)
    })
    
    output$tabla_aliEstrategico <- renderUI({
      color_matriz <- data_aliEstrategico() %>% left_join(color_table, by = c("Componente"="componente"))
      columnas_color <- c("Componente", "Identidad Institucional", "Objetivo Estratégico")
      data_aliEstrategico() %>%
        flextable() %>%
        autofit() %>%
        merge_v("Categoría") %>%
        theme_vanilla() %>%
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        color(j = columnas_color, 
              color = scales::col_factor(color_matriz$color2, levels = color_matriz$Componente), 
              source = "Componente") %>%
        bg(j = columnas_color, 
           bg = scales::col_factor(color_matriz$bg, levels = color_matriz$Componente),
           source = "Componente") %>% 
        htmltools_value()
    })
    
    output$tabla_aliEspecifico <- renderUI({
      color_matriz <- data_aliEspecifico() %>% left_join(color_table, by = c("Componente"="componente"))
      columnas_color <- c("Objetivo Estratégico", "Objetivos Específicos", "Metas", "Año")
      
      data_aliEspecifico() %>% 
        select(-Componente) %>% 
        flextable() %>% 
        autofit() %>%
        merge_v(c("Objetivo Estratégico", "Objetivos Específicos")) %>%
        theme_vanilla() %>%
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        color(j = columnas_color, 
              color = scales::col_factor(color_matriz$color2, levels = color_matriz$`Objetivos Específicos`), 
              source = "Objetivos Específicos") %>%
        bg(j = columnas_color, 
           bg = scales::col_factor(color_matriz$bg, levels = color_matriz$`Objetivos Específicos`),
           source = "Objetivos Específicos") %>% 
        htmltools_value()
    })
    
    output$tabla_monitoreoPEI <- renderUI({
      color_matriz <- data_monitoreoPEI() %>% left_join(color_table, by = c("Componente"="componente"))
      columnas_color <- data_monitoreoPEI() %>% select(-Componente) %>% names()
        
      data_monitoreoPEI() %>% 
        select(-Componente) %>% 
        flextable() %>% 
        autofit() %>%
        merge_v(c("Objetivos Específicos")) %>%
        theme_vanilla() %>%
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        color(j = columnas_color, 
              color = scales::col_factor(color_matriz$color2, levels = color_matriz$`Objetivos Específicos`), 
              source = "Objetivos Específicos") %>%
        bg(j = columnas_color, 
           bg = scales::col_factor(color_matriz$bg, levels = color_matriz$`Objetivos Específicos`),
           source = "Objetivos Específicos") %>% 
        htmltools_value()
    })
    
    output$tabla_evaluacionPEI <- renderUI({
      color_matriz <- data_evaluacionPEI() %>% left_join(color_table, by = c("Componente"="componente"))
      columnas_color <- data_evaluacionPEI() %>% select(-Componente) %>% names()
      
      data_evaluacionPEI() %>% 
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
    
    reset <- reactive(input$reset)
    observeEvent(reset(), {
      updateCheckboxGroupInput(
        session, 
        "rubrica1", 
        choices = c("Alineamiento con misión",
                    "Alineamiento con visión",
                    "Alineamiento con valores institucionales",
                    "Alineamiento con los componentes de gestión"))
      updateCheckboxGroupInput(
        session, "rubrica2",
        choices = c("Alineamiento con objetivos estratégicos",
                    "Alineamiento con metas institucionales",
                    "Alineamiento con los componentes de gestión",
                    "Coherencia con temporalidad para la ejecución de metas"))
      updateCheckboxGroupInput(
        session, "rubrica3",
        choices = c("Construcción a partir de los objetivos específicos",
                    "Estado de avance en el cumplimiento de metas anuales",
                    "Descripción del estado de avance",
                    "Acciones para implementar a partir del avance"))
      updateCheckboxGroupInput(
        session, "rubrica4",
        choices = c("Descripción del cumplimiento de metas anuales",
                    "Descripción del cumplimiento de objetivos estratégicos",
                    "Descripción del cumplimiento de objetivos específicos",
                    "Descripción de la evaluación institucional"))
    })
    
    data_guardar <- reactive({
      tibble(nombre_alumno = alumno_elegido(),
             rubrica1 = paste(input$rubrica1, collapse = ", "),
             cal_rubrica1 = length(input$rubrica1) + 1,
             rubrica2 = paste(input$rubrica2, collapse = ", "),
             cal_rubrica2 = length(input$rubrica2) + 1,
             rubrica3 = paste(input$rubrica3, collapse = ", "),
             cal_rubrica3 = length(input$rubrica3) + 1,
             rubrica4 = paste(input$rubrica4, collapse = ", "),
             cal_rubrica4 = length(input$rubrica4) + 1,
             total = cal_rubrica1+cal_rubrica2+cal_rubrica3+cal_rubrica4)
    })
    
    guardarTablaServer("guardar", data = data_guardar, nombre_carpeta = "evalTarea1", nombre_usuario = alumno_elegido)
  })
}

evalTarea1App <- function(){
  ui <- fluidPage(
    evalTarea1UI("myTestId")
  )
  server <- function(input, output, session){
    choices_alumnos <- reactive(c("alumno1", "alumno2", "alumno3", "alumno4", "alumno5"))
    evalTarea1Server("myTestId", choices_alumnos)
  }
  shinyApp(ui, server)
}

# evalTarea1App()
