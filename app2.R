library(shiny)
library(tidyverse)
library(shinymanager)
library(flextable)

creds <- read_csv("data/credentials.csv")
tipo_usuario <- "alumno"

ui <- fluidPage(
  navbarPage(
    title = "Curso de Gestión Institucional",
    theme = shinythemes::shinytheme("yeti"),
    tabPanel(
      title = "Inicio",
      bienvenidaUI("bienvenida")
    ),
    tabPanel(
      title = "Curso",
      uiOutput("moduloCurso")
    ),
    tabPanel(
      title = "Cronograma",
      cronogramaUI("cronograma")
    )
  )
)

ui <- secure_app(ui)

server <- function(input, output) {
  
  # auth start
  res_auth <- secure_server(
    check_credentials = check_credentials(creds)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  # #auth end
  
  tipo_de_usuario <- reactive(res_auth$tipo)
  nombre_usuario <- reactive(res_auth$user)
  
  bienvenidaServer("bienvenida")
  
  output$moduloCurso <- renderUI(moduloCursoUI("moduloCurso"))
  moduloCursoServer("moduloCurso", tipo_de_usuario)
  
  cronogramaServer("cronograma")
}

# Run the application 
shinyApp(ui = ui, server = server)
