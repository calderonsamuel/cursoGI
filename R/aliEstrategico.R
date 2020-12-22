aliEstrategicoUI <- function(id) {
  choices <- read_csv("data/componentes_gestion_list.csv") %>% 
    as.list() %>% 
    map(~.x[!is.na(.x)])
  choices <- c("", choices)
  
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 5,
        tabsetPanel(type = "pills",
          tabPanel(
            title = "Misión",
            tags$h4("Alineamiento de misión institucional"),
            fluidRow(
              column(4, textAreaInput(ns("mision1"), NULL, placeholder = "Parte 1")),
              column(4, selectInput(ns("c_mision1"), "Componente alineado", choices = choices)),
              column(4, textAreaInput(ns("oest1_1"),  "Obj. Estratégico 1 ", placeholder = "Parte 1"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("mision2"), NULL, placeholder = "Parte 2")),
              column(4, selectInput(ns("c_mision2"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest1_2"), NULL, placeholder = "Parte 2"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("mision3"), NULL, placeholder = "Parte 3")),
              column(4, selectInput(ns("c_mision3"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest1_3"), NULL, placeholder = "Parte 3"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("mision4"), NULL, placeholder = "Parte 4")),
              column(4, selectInput(ns("c_mision4"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest1_4"), NULL, placeholder = "Parte 4"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("mision5"), NULL, placeholder = "Parte 5")),
              column(4, selectInput(ns("c_mision5"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest1_5"), NULL, placeholder = "Parte 5"))
            )
          ),
          tabPanel(
            title = "Visión",
            tags$h4("Alineamiento de visión institucional"),
            fluidRow(
              column(4, textAreaInput(ns("vision1"), NULL, placeholder = "Parte 1")),
              column(4, selectInput(ns("c_vision1"), "Componente alineado", choices = choices)),
              column(4, textAreaInput(ns("oest2_1"),  "Obj. Estratégico 2", placeholder = "Parte 1"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("vision2"), NULL, placeholder = "Parte 2")),
              column(4, selectInput(ns("c_vision2"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest2_2"), NULL, placeholder = "Parte 2"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("vision3"), NULL, placeholder = "Parte 3")),
              column(4, selectInput(ns("c_vision3"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest2_3"), NULL, placeholder = "Parte 3"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("vision4"), NULL, placeholder = "Parte 4")),
              column(4, selectInput(ns("c_vision4"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest2_4"), NULL, placeholder = "Parte 4"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("vision5"), NULL, placeholder = "Parte 5")),
              column(4, selectInput(ns("c_vision5"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest2_5"), NULL, placeholder = "Parte 5"))
            )
          ),
          tabPanel(
            title = "Valores",
            tags$h4("Alineamiento de valores institucionales"),
            fluidRow(
              column(4, textAreaInput(ns("valores1"), NULL, placeholder = "Valor institucional 1")),
              column(4, selectInput(ns("c_valores1"), "Componente alineado", choices = choices)),
              column(4, textAreaInput(ns("oest3_1"), "Obj. Estratégico 3",  placeholder = "Parte 1"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("valores2"), NULL, placeholder = "Valor institucional 2")),
              column(4, selectInput(ns("c_valores2"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest3_2"),  NULL, placeholder = "Parte 2"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("valores3"), NULL, placeholder = "Valor institucional 3")),
              column(4, selectInput(ns("c_valores3"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest3_3"),  NULL, placeholder = "Parte 3"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("valores4"), NULL, placeholder = "Valor institucional 4")),
              column(4, selectInput(ns("c_valores4"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest3_4"),  NULL, placeholder = "Parte 4"))
            ),
            fluidRow(
              column(4, textAreaInput(ns("valores5"), NULL, placeholder = "Valor institucional 5")),
              column(4, selectInput(ns("c_valores5"), NULL, choices = choices)),
              column(4, textAreaInput(ns("oest3_5"),  NULL, placeholder = "Parte 5"))
            )
          )
        )
      ),
      mainPanel(width = 7,
        uiOutput(ns("tabla")),
        guardarTablaUI(ns("guardar"))
      )
    )
  )
}

aliEstrategicoServer <- function(id, nombre_usuario){
  moduleServer(id, function(input, output, session){
    
    color_table <- read_csv("data/color_table.csv")
    
    data <- reactive({
      mision <- tribble(
        ~Categoría, ~`Identidad Institucional`, ~Componente, ~`Objetivo Estratégico`,
        "Misión", input$mision1, input$c_mision1, input$oest1_1,
        "Misión", input$mision2, input$c_mision2, input$oest1_2,
        "Misión", input$mision3, input$c_mision3, input$oest1_3,
        "Misión", input$mision4, input$c_mision4, input$oest1_4,
        "Misión", input$mision5, input$c_mision5, input$oest1_5
      )
      
      vision <- tribble(
        ~Categoría, ~`Identidad Institucional`, ~Componente, ~`Objetivo Estratégico`,
        "Visión", input$vision1, input$c_vision1, input$oest2_1,
        "Visión", input$vision2, input$c_vision2, input$oest2_2,
        "Visión", input$vision3, input$c_vision3, input$oest2_3,
        "Visión", input$vision4, input$c_vision4, input$oest2_4,
        "Visión", input$vision5, input$c_vision5, input$oest2_5
      )
      
      valores <- tribble(
        ~Categoría, ~`Identidad Institucional`, ~Componente, ~`Objetivo Estratégico`,
        "Valores", input$valores1, input$c_valores1, input$oest3_1,
        "Valores", input$valores2, input$c_valores2, input$oest3_2,
        "Valores", input$valores3, input$c_valores3, input$oest3_3,
        "Valores", input$valores4, input$c_valores4, input$oest3_4,
        "Valores", input$valores5, input$c_valores5, input$oest3_5
      )
      
      bind_rows(mision, vision, valores) %>% 
        filter(`Identidad Institucional` != "")
    })
    
    color_matriz <- reactive({
      data() %>% 
        left_join(color_table, by = c("Componente"="componente"))
    })
    
    output$tabla <- renderUI({
      validate(need(nrow(data()) != 0,"Ingresar texto en Misión, Visión o Valores"))
      data() %>% 
        flextable() %>%
        autofit() %>%
        merge_v("Categoría") %>%
        theme_vanilla() %>%
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        color(j = "Componente", color = scales::col_factor(color_matriz()$color2, levels = color_matriz()$Componente)) %>%
        bg(j = "Componente", bg = scales::col_factor(color_matriz()$bg, levels = color_matriz()$Componente)) %>% 
        color(j = "Identidad Institucional", color = scales::col_factor(color_matriz()$color2, levels = color_matriz()$`Identidad Institucional`)) %>%
        bg(j = "Identidad Institucional", bg = scales::col_factor(color_matriz()$bg, levels = color_matriz()$`Identidad Institucional`)) %>% 
        color(j = "Objetivo Estratégico", color = scales::col_factor(color_matriz()$color2, levels = color_matriz()$`Objetivo Estratégico`)) %>%
        bg(j = "Objetivo Estratégico", bg = scales::col_factor(color_matriz()$bg, levels = color_matriz()$`Objetivo Estratégico`)) %>% 
        htmltools_value()
    })
    
    guardarTablaServer("guardar", data = data, nombre_carpeta = "tarea1/aliEstrategico", nombre_usuario = nombre_usuario)
  })
}

aliEstrategicoApp <- function(){
  ui <- fluidPage(
    aliEstrategicoUI("myTestId")
  )
  
  server <- function(input, output, session){
    nombre_usuario <- reactive("alumno1")
    aliEstrategicoServer("myTestId", nombre_usuario)
  }
  shinyApp(ui, server)
}

# aliEstrategicoApp()
