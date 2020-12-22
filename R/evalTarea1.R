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
          uiOutput(ns("elegir_alumno")),
        tabsetPanel(type = "pills",
          tabPanel(
            title = "Alineamiento estratégico",
            uiOutput(ns("tabla_aliEstrategico"))
          ),
          tabPanel(
            title = "Alineamiento específico"
          ),
          tabPanel(
            title = "Monitoreo del PEI"
          ),
          tabPanel(
            title = "Evaluación del PEI"
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
    
    output$tabla_aliEstrategico <- renderUI({
      color_matriz <- data_aliEstrategico() %>% left_join(color_table, by = c("Componente"="componente"))
      
      data_aliEstrategico() %>%
        flextable() %>%
        autofit() %>%
        merge_v("Categoría") %>%
        theme_vanilla() %>%
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        color(j = "Componente", color = scales::col_factor(color_matriz$color2, levels = color_matriz$Componente)) %>%
        bg(j = "Componente", bg = scales::col_factor(color_matriz$bg, levels = color_matriz$Componente)) %>% 
        color(j = "Identidad Institucional", color = scales::col_factor(color_matriz$color2, levels = color_matriz$`Identidad Institucional`)) %>%
        bg(j = "Identidad Institucional", bg = scales::col_factor(color_matriz$bg, levels = color_matriz$`Identidad Institucional`)) %>% 
        color(j = "Objetivo Estratégico", color = scales::col_factor(color_matriz$color2, levels = color_matriz$`Objetivo Estratégico`)) %>%
        bg(j = "Objetivo Estratégico", bg = scales::col_factor(color_matriz$bg, levels = color_matriz$`Objetivo Estratégico`)) %>% 
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
    
    guardarTablaServer("guardar", data = data_aliEstrategico, nombre_carpeta = "evalTarea1", nombre_usuario = alumno_elegido)
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
