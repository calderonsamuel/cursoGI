matriz_monitoreo_peiUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tabla"))
  )
}

matriz_monitoreo_peiServer <- function(id, color_table) {
  moduleServer(id, function(input, output, session) {
      
    output$tabla <- renderUI({
      matriz <- read_csv("data/monitoreo_PEI.csv")
      oesp <- read_csv("data/oesp.csv")
      oest_componentes <- read_csv("data/oest_componentes.csv")
      color_table <- read_csv("data/componentes_gestion.csv")
      
      matriz_color <- matriz %>% 
        left_join(oesp) %>% 
        left_join(oest_componentes) %>%  
        left_join(color_table, by= c("Componente" = "componente")) %>% 
        select(`Objetivos específico`:`Acciones a implementar a partir del avance`, 
               Componente, color2, bg) %>% 
        distinct(`Metas anuales`, .keep_all = TRUE)
      
      matriz %>% 
        flextable() %>% 
        autofit() %>% 
        flextable::theme_vanilla() %>% 
        fontsize(size = 15, part = "body") %>%
        fontsize(size = 16, part = "header") %>% 
        bold(part = "header") %>% 
        color(color = "white", part = "header") %>% 
        bg(bg = "gray20", part = "header") %>% 
        color(color = unique(matriz_color$color2)) %>% 
        bg(bg = unique(matriz_color$bg)) %>% 
        merge_v(j = "Objetivos específico") %>% 
             # color = scales::col_factor(
             #   palette = color_matriz()$color2, 
             #   levels = color_matriz()$`Objetivos estratégicos`)) %>%
        # bg(j = "Objetivos estratégicos", 
           
           # bg = scales::col_factor(
           #   palette = color_matriz()$bg, 
           #   levels = color_matriz()$`Objetivos estratégicos`)) %>%
        htmltools_value()
    })
  })
}

matriz_monitoreo_peiApp <- function(){
  ui <- fluidPage(
    matriz_monitoreo_peiUI("myTestId")
  )
  server <- function(input, output, session) {
    matriz_monitoreo_peiServer("myTestId")
  }
  shinyApp(ui, server)
}

# matriz_monitoreo_peiApp()
