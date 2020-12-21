aliEstrategicoUI <- function(id) {
  choices <- read_csv("data/componentes_gestion_list.csv") %>% 
    as.list() %>% 
    map(~.x[!is.na(.x)])
  choices <- c("", choices)
  
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          tabPanel(
            title = "Misión",
            fluidRow(
              column(6, textAreaInput(ns("mision1"), "Ingrese la parte 1 de la Misión")),
              column(6, selectInput(ns("c_mision1"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("mision2"), "Ingrese la parte 2 de la Misión")),
              column(6, selectInput(ns("c_mision2"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("mision3"), "Ingrese la parte 3 de la Misión")),
              column(6, selectInput(ns("c_mision3"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("mision4"), "Ingrese la parte 4 de la Misión")),
              column(6, selectInput(ns("c_mision4"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("mision5"), "Ingrese la parte 5 de la Misión")),
              column(6, selectInput(ns("c_mision5"), "Componente alineado", choices = choices))
            )
          ),
          tabPanel(
            title = "Visión",
            fluidRow(
              column(6, textAreaInput(ns("vision1"), "Ingrese la parte 1 de la Visión")),
              column(6, selectInput(ns("c_vision1"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("vision2"), "Ingrese la parte 2 de la Visión")),
              column(6, selectInput(ns("c_vision2"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("vision3"), "Ingrese la parte 3 de la Visión")),
              column(6, selectInput(ns("c_vision3"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("vision4"), "Ingrese la parte 4 de la Visión")),
              column(6, selectInput(ns("c_vision4"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("vision5"), "Ingrese la parte 5 de la Visión")),
              column(6, selectInput(ns("c_vision5"), "Componente alineado", choices = choices))
            )
          ),
          tabPanel(
            title = "Valores",
            fluidRow(
              column(6, textAreaInput(ns("valores1"), "Ingrese el primer valor institucional")),
              column(6, selectInput(ns("c_valores1"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("valores2"), "Ingrese el segundo valor institucional")),
              column(6, selectInput(ns("c_valores2"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("valores3"), "Ingrese el tercer valor institucional")),
              column(6, selectInput(ns("c_valores3"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("valores4"), "Ingrese el cuarto valor institucional")),
              column(6, selectInput(ns("c_valores4"), "Componente alineado", choices = choices))
            ),
            fluidRow(
              column(6, textAreaInput(ns("valores5"), "Ingrese el quinto valor institucional")),
              column(6, selectInput(ns("c_valores5"), "Componente alineado", choices = choices))
            )
          )
        )
      ),
      mainPanel(
        uiOutput(ns("tabla"))
        # dataTableOutput(ns("tabla"))
      )
    )
  )
}

aliEstrategicoServer <- function(id){
  moduleServer(id, function(input, output, session){
    data <- reactive({
      mision <- tibble(Categoría = "Misión",
                       `Identidad Institucional` = c(input$mision1,
                                                     input$mision2,
                                                     input$mision3,
                                                     input$mision4,
                                                     input$mision5),
                       Componente = c(input$c_mision1,
                                      input$c_mision2,
                                      input$c_mision3,
                                      input$c_mision4,
                                      input$c_mision5))
      vision <- tibble(Categoría = "Visión",
                       `Identidad Institucional` = c(input$vision1,
                                                     input$vision2,
                                                     input$vision3,
                                                     input$vision4,
                                                     input$vision5),
                       Componente = c(input$c_vision1,
                                      input$c_vision2,
                                      input$c_vision3,
                                      input$c_vision4,
                                      input$c_vision5))
      valores <- tibble(Categoría = "Valores",
                       `Identidad Institucional` = c(input$valores1,
                                                     input$valores2,
                                                     input$valores3,
                                                     input$valores4,
                                                     input$valores5),
                       Componente = c(input$c_valores1,
                                      input$c_valores2,
                                      input$c_valores3,
                                      input$c_valores4,
                                      input$c_valores5))
      
      bind_rows(mision, vision, valores) %>% 
        filter(`Identidad Institucional` != "")
    })
    
    output$tabla <- renderUI({
      # validate(need(nrow(data()) != 0,"Ingresar texto en Misión, Visión o Valores"))
      data() %>% 
        flextable() %>%
        autofit() %>%
        merge_v("Categoría") %>%
        theme_vanilla() %>%
        htmltools_value()
    })
  })
}

aliEstrategicoApp <- function(){
  ui <- fluidPage(
    aliEstrategicoUI("myTestId")
  )
  
  server <- function(input, output, session){
    aliEstrategicoServer("myTestId")
  }
  shinyApp(ui, server)
}

# aliEstrategicoApp()