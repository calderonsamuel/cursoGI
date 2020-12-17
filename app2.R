library(shiny)
library(tidyverse)
library(shinymanager)

creds <- read_csv("data/credentials.csv")
tipo_usuario <- "alumno"

ui <- fluidPage(
  navbarPage(
    title = "Curso de Gestión Institucional",
    theme = shinythemes::shinytheme("yeti"),
    tabPanel("Inicio"),
    tabPanel(
      title = "Curso",
      uiOutput("moduloCurso")
    ),
    tabPanel("Calendario")
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
  
  output$moduloCurso <- renderUI(moduloCursoUI("moduloCurso"))
  moduloCursoServer("moduloCurso", tipo_de_usuario)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
