oest_componentesUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("table"))
  )
}

oest_componentesServer <- function(id, matriz, color_table) {
  moduleServer(id, function(input, output, session) {
    color_matriz <- reactive({
      matriz() %>% 
        left_join(color_table, by = c("Componente"="componente"))
    })
    
    output$table <- renderUI({
      matriz() %>% 
        select(-c(num_oest, oest)) %>% 
        flextable() %>% 
        autofit() %>% 
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        bold(part = "header") %>% 
        color(color = "white", part = "header") %>% 
        bg(bg = "gray20", part = "header") %>% 
        color(j = "Componente", color = scales::col_factor(color_matriz()$color2, levels = color_matriz()$Componente)) %>% 
        bg(j = "Componente", bg = scales::col_factor(color_matriz()$bg, levels = color_matriz()$Componente)) %>% 
        color(j = "Objetivos estratégicos", color = scales::col_factor(color_matriz()$color2, levels = color_matriz()$`Objetivos estratégicos`)) %>%
        bg(j = "Objetivos estratégicos", bg = scales::col_factor(color_matriz()$bg, levels = color_matriz()$`Objetivos estratégicos`)) %>%
        htmltools_value()
    })
  })
}

oest_componentesApp <- function(){
  matriz <- reactive(read_csv("data/oest_componentes.csv"))
  color_table <- read_csv("data/componentes_gestion.csv")
  ui <- fluidPage(
    oest_componentesUI("myTestId")
  )
  server <- function(input, output, session) {
    oest_componentesServer("myTestId", matriz, color_table)
  }
  shinyApp(ui, server)
}

# oest_componentesApp()
