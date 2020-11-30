matriz_est_componentesInput <- function(id, choices){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("mision"), 
      label = "¿Qué componentes de gestión se alinean con la misión institucional?", 
      choices = choices, 
      multiple = TRUE),
    selectInput(
      inputId = ns("vision"), 
      label = "¿Qué componentes de gestión se alinean con la visión institucional?", 
      choices = choices, 
      multiple = TRUE),
    selectInput(
      inputId = ns("valores"), 
      label = "¿Qué componentes de gestión se alinean con los valores institucionales??", 
      choices = choices, 
      multiple = TRUE)#,
    # DTOutput(ns("tabla"))
  )
}

tabla_est_componentesOutput <- function(id){
  ns <- NS(id)
  tagList(
      uiOutput(ns("tabla"))
  )
}

matriz_est_componentesServer <- function(id, alumn_id){
  moduleServer(id, function(input, output, session){
    reactive({
      file <- paste0("data/estudiantes_matrices/matriz_ii/", alumn_id(), ".csv")
      data <- read_csv(file)
      data %>%
        mutate(`Componente` = c(paste0(input$mision, collapse = ","),
                                paste0(input$vision, collapse = ","),
                                paste0(input$valores, collapse = ","))
        )
    })
  })
}

tabla_est_componentesServer <- function(id, matriz, color_table){
  moduleServer(id, function(input, output, session){
    output$tabla <- renderUI({
      matriz() %>% 
        select(-user) %>%
        separate_rows(Componente, sep = ",") %>%
        flextable() %>% 
        theme_vanilla() %>% 
        merge_v(j = c("Identidad", "Texto ingresado")) %>% 
        autofit() %>% 
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        bold(part = "header") %>% 
        color(color = "white", part = "header") %>% 
        bg(bg = "gray20", part = "header") %>% 
        color(j = "Componente", color = scales::col_factor(color_table$color2, levels = color_table$componente)) %>% 
        bg(j = "Componente", bg = scales::col_factor(color_table$bg, levels = color_table$componente)) %>% 
        htmltools_value()
        
    })
  })
}

matriz_est_componentesApp <- function(){
  choices <- read_csv("data/componentes_gestion_list.csv") %>% as.list() %>% map(~.x[!is.na(.x)])
  color_table <- read_csv("data/componentes_gestion.csv")
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        matriz_est_componentesInput("my_id", choices)
      ),
      mainPanel(
        tabla_est_componentesOutput("my_table_id")
      )
    )
  )
  server <- function(input, output, session) {
    alumn_id <- reactive("alumno1")
    matriz <- matriz_est_componentesServer("my_id", alumn_id)
    tabla_est_componentesServer("my_table_id", matriz, color_table)
  }
  shinyApp(ui, server)  
}

# library(shiny)
# library(tidyverse)
# library(flextable)
# matriz_est_componentesApp()
