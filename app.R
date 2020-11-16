library(shiny)
library(tidyverse)
library(shinymanager)
library(readxl)
library(DT)

creds <- read_csv("data/credentials.csv")

sheets <- excel_sheets("data/matrices_ejemplo.xlsx")

matriz_ii <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[1])
matriz_oest <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[2])
matriz_cbc <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[3])
matriz_oesp <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[4])
matriz_metas <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[5])


ui <- navbarPage(
  theme = shinythemes::shinytheme("yeti"),
  title = "Curso de Gestión Institucional",
  tabPanel(
      title = "Estudiantes",
      uiOutput("ui_estudiantes")
  ),
  tabPanel(
    title = "Guías",
    uiOutput("ui_guias")
  )
)

ui <- shinymanager::secure_app(ui)

server <- function(input, output, session) {
  # auth start

  res_auth <- secure_server(
    check_credentials = check_credentials(creds)
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  # #auth end
  
  output$tabla_ii <- DT::renderDT({
      matriz_ii
  }, rownames = FALSE, selection = "none")
  
  output$tabla_oest <- DT::renderDT({
      matriz_oest
  }, rownames = FALSE, selection = "single")
  
  selected_oest <- reactive({
      matriz_oest %>% 
          filter(row_number() == input$tabla_oest_rows_selected) %>% 
          pull(`Objetivos estratégicos`)
  })
  
  output$tabla_CBC <- renderDT({
      data <- matriz_cbc
      if(length(input$tabla_oest_rows_selected)>0) {
          data <- matriz_cbc %>% 
              filter(`Objetivos estratégicos` == selected_oest())
      }
      data
      
  }, rownames = FALSE, selection = "none")
  
  filtered_matriz_oesp <- reactive({
    data <- matriz_oesp
    if(length(input$tabla_oest_rows_selected) > 0) {
      data <- matriz_oesp %>% 
        filter(`Objetivos estratégicos` == selected_oest())
    }
    data
  })
  
  output$tabla_oesp <- renderDT(filtered_matriz_oesp(), rownames = FALSE, selection = "single")
  
  selected_oesp <- reactive({
    filtered_matriz_oesp() %>% 
      filter(row_number() == input$tabla_oesp_rows_selected) %>% 
      pull(`Objetivos específico`)
  })
  
  filtered_matriz_metas <- reactive({
    data <- matriz_metas
    if(length(input$tabla_oesp_rows_selected) > 0) {
      data <- matriz_metas %>% 
        filter(`Objetivos específico` == selected_oesp())
    }
    data
  })
  
  output$tabla_metas <- renderDT({
      filtered_matriz_metas()
  }, rownames = FALSE, selection = "none")
  
  matriz_est_ii <- reactive({
    tibble(Identidad = c("Misión", "Visión", "Valores"),
           `Texto ingresado` =  c(input$est_ii_mision, 
                                  input$est_ii_vision, 
                                  str_replace_all(input$est_ii_valores, "\n", "<br>")))
  })
  
  output$tabla_est_ii <- renderDT({
    matriz_est_ii()
  })
  
  output$tabla_est_oest <- renderDT({
    matriz_est_ii() %>% 
      mutate(`Objetivos estratégicos` = c(input$est_oest_1, input$oest_2, input$oest_3))
  })
  
  output$ui_estudiantes <- renderUI({
    validate(need(res_auth$user == "alumno", "Debe ser alumno para ver este contenido"))
    tabsetPanel(
      id = "tabset_estudiantes_matrices",
      tabPanel(
        title = "Identidad institucional",
        sidebarLayout(
          sidebarPanel(
            textAreaInput("est_ii_mision", "Ingrese la misión institucional", resize = "vertical"),
            textAreaInput("est_ii_vision", "Ingrese la visión institucional", resize = "vertical"),
            textAreaInput("est_ii_valores", "Ingrese los valores institucionales", resize = "vertical")
          ),
          mainPanel(
            DTOutput("tabla_est_ii")
          )
        )
      ),
      
      tabPanel(
        title = "Objetivos estratégicos",
        sidebarLayout(
          sidebarPanel(
            textAreaInput("est_oest_1", "Ingrese el objetivo estratégico 1 (Vincular a misión)"),
            textAreaInput("est_oest_2", "Ingrese el objetivo estratégico 2 (Vincular a visión)"),
            textAreaInput("est_oest_3", "Ingrese el objetivo estratégico 3 (Vincular a valores)"),
            
          ),
          mainPanel(
            DTOutput("tabla_est_oest")
          )
        )
      )
      
    )
  })
  
  output$ui_guias <- renderUI({
    validate(need(res_auth$user == "docente", "Debe ser guía para ver este contenido"))
    
    tabsetPanel(
      id = "tabset_guias_matrices",
      tabPanel(
        title = "Identidad institucional",
        sidebarLayout(
          sidebarPanel(
            tags$h3("Aquí se muestran los datos de identidad institucional")
          ),
          mainPanel(
            DTOutput("tabla_ii")
          )
        )
      ),
      tabPanel(
        title = "Objetivos estratégicos",
        sidebarLayout(
          sidebarPanel(
            tags$h3("Aquí se ve el alineamiento de Objetivos estratégicos y datos de identidad institucional")
          ),
          mainPanel(
            DTOutput("tabla_oest")
          )
        )
      ),
      tabPanel(
        title = "CBC",
        sidebarLayout(
          sidebarPanel(
            tags$h3("Aquí se ve el alineamiento de Objetivos estratégicos y CBC")
          ),
          mainPanel(
            DTOutput("tabla_CBC")
          )
        )
      ),
      tabPanel(
        title = "Objetivos específicos",
        sidebarLayout(
          sidebarPanel(
            tags$h3("Aquí se ve el alineamiento de objetivos estratégicos y objetivos específicos")
          ),
          mainPanel(
            DTOutput("tabla_oesp")
          )
        )
      ),
      tabPanel(
        title = "Metas",
        sidebarLayout(
          sidebarPanel(
            tags$h3("Aquí se ven las metas según objetivos específicos")
          ),
          mainPanel(
            DTOutput("tabla_metas")
          )
        )
      )
    )
  })
  
}

shinyApp(ui, server)