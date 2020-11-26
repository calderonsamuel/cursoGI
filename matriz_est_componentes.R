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
    DTOutput(ns("tabla"))
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

tabla_est_componentesServer <- function(id, matriz){
  moduleServer(id, function(input, output, session){
    output$tabla <- renderDT({
      matriz() %>% 
        select(-user) %>%
        separate_rows(Componente, sep = ",") %>%
        # mutate(Componente = str_squish(Componente)) %>%
        datatable() %>%
        formatStyle('Componente', color = styleEqual(color_table$componente, color_table$color))
    })
  })
}

matriz_est_componentesApp <- function(){
  choices <- read_csv("data/componentes_gestion_list.csv")
  color_table <- read_csv("data/componentes_gestion.csv")
  
  ui <- fluidPage(
    matriz_est_componentesUI("my_id", choices),
    tabla_est_componentesOutput("my_table_id")
  )
  server <- function(input, output, session) {
    alumn_id <- reactive("alumno1")
    matriz <- matriz_est_componentesServer("my_id", alumn_id)
    tabla_est_componentesServer("my_table_id", matriz)
  }
  shinyApp(ui, server)  
}

matriz_est_componentesApp()
