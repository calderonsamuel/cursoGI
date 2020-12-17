cronogramaUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tabla"))
  )
}

cronogramaServer <- function(id){
  moduleServer(id, function(input, output, session){
    output$tabla <- renderUI({
      data <- readxl::read_excel("data/cronograma.xlsx")
      
      data %>% 
        # mutate(`Actividades a realizar` = str_wrap(`Actividades a realizar`)) %>% 
        relocate(`Actividades a realizar`, .after = Módulo) %>% 
        flextable() %>% 
        merge_v(j = c("Módulo", "Semana")) %>% 
        autofit() %>% 
        theme_vanilla() %>% 
        htmltools_value()
    })
  })
}

cronogramaApp <- function(){
  ui <- fluidPage(
    cronogramaUI("myTestId")
  )
  server <- function(input, output, session){
    cronogramaServer("myTestId")
  }
  shinyApp(ui, server)
}

# cronogramaApp()
