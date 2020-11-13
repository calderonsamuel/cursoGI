library(shiny)
library(tidyverse)
library(shinymanager)
library(readxl)
library(DT)

sheets <- excel_sheets("data/matrices_ejemplo.xlsx")

matriz_ii <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[1])
matriz_oest <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[2])
matriz_cbc <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[3])
matriz_oesp <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[4])
matriz_metas <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[5])


ui <- navbarPage(
  title = "Curso de Gestión Institucional",
  tabPanel(
      title = "Guías",
      tabsetPanel(
          id = "tabset_matrices",
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
  ),
  tabPanel(
      title = "Estudiantes"
  )
)

server <- function(input, output, session) {
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
  
  output$tabla_oesp <- renderDT({
      data <- matriz_oesp
      if(length(input$tabla_oest_rows_selected) > 0) {
          data <- matriz_oesp %>% 
              filter(`Objetivos estratégicos` == selected_oest())
      }
      data
  }, rownames = FALSE, selection = "single")
  
  output$tabla_metas <- renderDT({
      matriz_metas
  }, rownames = FALSE, selection = "none")
}

shinyApp(ui, server)