library(shiny)
library(tidyverse)
library(shinymanager)
library(readxl)
library(DT)

creds <- read_csv("data/credentials.csv")
componentes_gestion <- read_csv("data/componentes_gestion_list.csv")
componentes_gestion_color <- read_csv("data/componentes_gestion.csv")

ui <- navbarPage(
  theme = shinythemes::shinytheme("yeti"),
  title = "Curso de Gestión Institucional",
  tabPanel(
      title = "Estudiantes",
      uiOutput("ui_estudiantes")
  ),
  tabPanel(
    title = "Guías",
    uiOutput("ui_guias")
  )
)

ui <- shinymanager::secure_app(ui)

server <- function(input, output, session) {
  # auth start

  res_auth <- secure_server(
    check_credentials = check_credentials(creds)
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  # #auth end
  
  nombre_alumno <- reactive(input$listado_alumnos)
  
  tipo_de_usuario <- reactive(res_auth$tipo)
  
  csvTableServer("matriz_ii", nombre_alumno)
  csvTableServer("matriz_componentes", nombre_alumno)
  csvTableServer("matriz_oest", nombre_alumno)
  
  filtered_matriz_oesp <- reactive({
    data <- matriz_oesp
    if(length(input$tabla_oest_rows_selected) > 0) {
      data <- matriz_oesp %>% 
        filter(`Objetivos estratégicos` == selected_oest())
    }
    data
  })
  
  output$tabla_oesp <- renderDT(filtered_matriz_oesp(), rownames = FALSE, selection = "single")
  
  selected_oesp <- reactive({
    filtered_matriz_oesp() %>% 
      filter(row_number() == input$tabla_oesp_rows_selected) %>% 
      pull(`Objetivos específico`)
  })
  
  filtered_matriz_metas <- reactive({
    data <- matriz_metas
    if(length(input$tabla_oesp_rows_selected) > 0) {
      data <- matriz_metas %>% 
        filter(`Objetivos específico` == selected_oesp())
    }
    data
  })
  
  output$tabla_metas <- renderDT({
      filtered_matriz_metas()
  }, rownames = FALSE, selection = "none")
  
  matriz_est_ii <- reactive({
    tibble(Identidad = c("Misión", "Visión", "Valores"),
           `Texto ingresado` =  c(input$est_ii_mision, 
                                  input$est_ii_vision, 
                                  str_replace_all(input$est_ii_valores, "\n", "<br>")))
  })
  
  output$tabla_est_ii <- renderDT({
    matriz_est_ii()
  })
  
  matriz_est_componentes <- reactive({
    file <- paste0("data/estudiantes_matrices/matriz_ii/", res_auth$user, ".csv")
    data <- read_csv(file)
    data %>% 
      # select(-c(Identidad,`Texto ingresado`)) %>%
      mutate(`Componente` = c(paste0(input$est_componentes_1, collapse = ", "), 
                       paste0(input$est_componentes_2, collapse = ", "), 
                       paste0(input$est_componentes_3, collapse = ", "))
      )
  })
  
  output$tabla_est_componentes <- renderDT({
    matriz_est_componentes() %>% 
      select(-user) %>% 
      separate_rows(Componente, sep = ",") %>%
      mutate(Componente = str_squish(Componente)) %>% 
      datatable() %>% 
      formatStyle('Componente', color = styleEqual(componentes_gestion_color$componente, componentes_gestion_color$color))
  })
  
  matriz_est_oest <- reactive({
    file <- paste0("data/estudiantes_matrices/matriz_ii/", res_auth$user, ".csv")
    data <- read_csv(file)
    data %>% 
      mutate(`Objetivos estratégicos` = c(input$est_oest_1, input$est_oest_2, input$est_oest_3))
  })
  
  output$tabla_est_oest <- renderDT({
    matriz_est_oest() %>% select(-user)
  })
  
  observeEvent(input$est_ii_save, {
    data <- matriz_est_ii() %>% mutate(user = res_auth$user) %>% relocate(user)
    file <- paste0("data/estudiantes_matrices/matriz_ii/", res_auth$user, ".csv")
    write_csv(data, file)
    showNotification("Guardado correctamente", duration = 2, closeButton = FALSE)
    updateTextAreaInput(session, "est_ii_mision", value = "")
    updateTextAreaInput(session, "est_ii_vision", value = "")
    updateTextAreaInput(session, "est_ii_valores", value = "")
    
    updateTabsetPanel(session, "tabset_estudiantes_matrices", selected = "est_oest")
  })
  
  observeEvent(input$est_oest_save, {
    data <- matriz_est_oest() 
    file <- paste0("data/estudiantes_matrices/matriz_oest/", res_auth$user, ".csv")
    write_csv(data, file)
    showNotification("Guardado correctamente", duration = 2, closeButton = FALSE)
    updateTextAreaInput(session, "est_oest_1", value = "")
    updateTextAreaInput(session, "est_oest_2", value = "")
    updateTextAreaInput(session, "est_oest_3", value = "")
    
    updateTabsetPanel(session, "tabset_estudiantes_matrices", selected = "est_componentes")
  })
  
  observeEvent(input$est_componentes_save, {
    data <- matriz_est_componentes() 
    file <- paste0("data/estudiantes_matrices/matriz_componentes/", res_auth$user, ".csv")
    write_csv(data, file)
    showNotification("Guardado correctamente", duration = 2, closeButton = FALSE)
    updateSelectInput(session, "est_componentes_1", value = "")
    updateSelectInput(session, "est_componentes_2", value = "")
    updateSelectInput(session, "est_componentes_3", value = "")
  })
  
  output$ui_estudiantes <- renderUI({
    validate(need(tipo_de_usuario() == "alumno", "Debe ser alumno para ver este contenido"))
    tabsetPanel(
      id = "tabset_estudiantes_matrices",
      tabPanel(
        title = "Identidad institucional", 
        value = "est_ii",
        sidebarLayout(
          sidebarPanel(
            textAreaInput("est_ii_mision", "Ingrese la misión institucional", resize = "vertical"),
            textAreaInput("est_ii_vision", "Ingrese la visión institucional", resize = "vertical"),
            textAreaInput("est_ii_valores", "Ingrese los valores institucionales", resize = "vertical"),
            actionButton("est_ii_save", "Guardar")
          ),
          mainPanel(
            DTOutput("tabla_est_ii")
          )
        )
      ),
      
      tabPanel(
        title = "Componentes",
        value = "est_componentes",
        sidebarLayout(
          sidebarPanel(
            selectInput("est_componentes_1", "¿Qué componentes de gestión se alinean con la misión institucional?", choices = componentes_gestion, multiple = TRUE),
            selectInput("est_componentes_2", "¿Qué componentes de gestión se alinean con la visión institucional?", choices = componentes_gestion, multiple = TRUE),
            selectInput("est_componentes_3", "¿Qué componentes de gestión se alinean con los valores institucionales??", choices = componentes_gestion, multiple = TRUE),
            actionButton("est_componentes_save", "Guardar")
            # TODO: guardar data de cbc
          ),
          mainPanel(
            DTOutput("tabla_est_componentes")
          )
        )
      ),
      
      tabPanel(
        title = "Objetivos estratégicos",
        value = "est_oest",
        sidebarLayout(
          sidebarPanel(
            textAreaInput("est_oest_1", "Ingrese el objetivo estratégico 1 (Vincular a misión)"),
            textAreaInput("est_oest_2", "Ingrese el objetivo estratégico 2 (Vincular a visión)"),
            textAreaInput("est_oest_3", "Ingrese el objetivo estratégico 3 (Vincular a valores)"),
            actionButton("est_oest_save", "Guardar")
            
          ),
          mainPanel(
            DTOutput("tabla_est_oest")
          )
        )
      )
      
      
      
    )
  })
  
  output$ui_guias <- renderUI({
    validate(need(tipo_de_usuario() == "docente", "Debe ser guía para ver este contenido"))
    
    tabsetPanel(
      id = "tabset_guias_matrices",
      tabPanel(
        title = "Alumnos",
        selectInput(
          inputId = "listado_alumnos",
          label = "Seleccione alumno",
          choices = filter(creds, tipo == "alumno")$user,
          selected = "alumno1"
        )
      ),
      tabPanel(
        title = "Identidad institucional",
        sidebarLayout(
          sidebarPanel(
            tags$h3("Aquí se muestran los datos de identidad institucional")
          ),
          mainPanel(
            csvTableOutput("matriz_ii")
          )
        )
      ),
      tabPanel(
        title = "Componentes",
        sidebarLayout(
          sidebarPanel(
            tags$h3("Aquí se ve el alineamiento de Identidad institucional y Componentes de Gestión")
          ),
          mainPanel(
            csvTableOutput("matriz_componentes")
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
            csvTableOutput("matriz_oest")
          )
        )
      )
    )
  })
  
}

shinyApp(ui, server)