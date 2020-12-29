library(shiny)
library(tidyverse)
library(shinymanager)
library(flextable)
library(bslib)

# my_theme <- bs_theme(bootswatch = "journal", version = 3)
# sass::sass(my_theme, output = "www/mytheme.css")

creds <- read_csv("data/credentials.csv")

ui <- fluidPage(
  navbarPage(
    title = "Curso de Gestión Institucional",
    # theme = shinythemes::shinytheme("journal"),
    theme = "mytheme.css",
    tabPanel(
      title = "Inicio",
      bienvenidaUI("bienvenida")
    ),
    tabPanel(
      title = "Curso",
      uiOutput("moduloCurso")
    )
  )
)

# ui <- secure_app(ui, theme = shinythemes::shinytheme("journal"))
ui <- secure_app(ui, theme = "mytheme.css")
# ui <- secure_app(ui)

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
  choices_alumnos <- reactive({
    if(tipo_de_usuario() == "docente") {
      creds %>% filter(docente_encargado == nombre_usuario()) %>% pull(user)
    } else "alumno1"
  })
  
  bienvenidaServer("bienvenida")
  
  output$moduloCurso <- renderUI(moduloCursoUI("moduloCurso"))
  moduloCursoServer("moduloCurso", tipo_de_usuario, choices_alumnos, nombre_usuario)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
