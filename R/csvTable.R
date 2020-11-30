csvTableOutput <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("tabla"))
    # DT::DTOutput(ns("tabla"))
  )
}

csvTableServer <- function(id, alumn_id){
  moduleServer(id, function(input, output, session){
    matriz <- reactive({
      matriz_dir <- id
      file <- paste0("data/estudiantes_matrices/", matriz_dir,"/", alumn_id(), ".csv")
      read_csv(file)
    })
    
    output$tabla <- renderUI({
    # output$tabla <- DT::renderDT({
      matriz() %>% 
        select(-user) %>% 
        # mutate(groupname=Identidad) %>% 
        flextable() %>% 
        autofit() %>% 
        htmltools_value()
        # tab_row_group(group = "Mision", rows = 1, others = "Otros")
    # }, rownames = FALSE, selection = "none")
    })
  })
}

csvTableApp <- function(){
  ui <- fluidPage(
    csvTableOutput("matriz_ii")
  )
  server <- function(input, output, session) {
    alumn_id <- reactive("alumno1")
    csvTableServer("matriz_ii", alumn_id)
  }
  shinyApp(ui, server)  
}

# csvTableApp()

