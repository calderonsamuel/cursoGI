library(shiny)
library(tidyverse)
library(shinymanager)
library(readxl)
library(DT)

creds <- read_csv("data/credentials.csv")

# sheets <- excel_sheets("data/matrices_ejemplo.xlsx")

# matriz_ii <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[1])
# matriz_oest <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[2])
# matriz_cbc <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[3])
# matriz_oesp <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[4])
# matriz_metas <- read_excel("data/matrices_ejemplo.xlsx", sheet = sheets[5])


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
  
  tipo_de_usuario <- reactive({
    creds %>% 
      filter(user == res_auth$user) %>% 
      pull(tipo)
  })

  # #auth end
  
  matriz_ii <- reactive({
    req(input$listado_alumnos)
    file <- paste0("data/estudiantes_matrices/matriz_ii/", input$listado_alumnos, ".csv")
    read_csv(file)
  })
  
  matriz_oest <- reactive({
    req(input$listado_alumnos)
    file <- paste0("data/estudiantes_matrices/matriz_oest/", input$listado_alumnos, ".csv")
    read_csv(file)
  })
  
  matriz_cbc <- reactive({
    req(input$listado_alumnos)
    file <- paste0("data/estudiantes_matrices/matriz_cbc/", input$listado_alumnos, ".csv")
    read_csv(file)
  })
  
  output$tabla_ii <- DT::renderDT({
      matriz_ii()
  }, rownames = FALSE, selection = "none")
  
  output$tabla_oest <- DT::renderDT({
      matriz_oest()
  }, rownames = FALSE, selection = "single")
  
  # selected_oest <- reactive({
  #     matriz_oest %>% 
  #         filter(row_number() == input$tabla_oest_rows_selected) %>% 
  #         pull(`Objetivos estratégicos`)
  # })
  
  output$tabla_CBC <- renderDT({
      # data <- matriz_cbc
      # if(length(input$tabla_oest_rows_selected)>0) {
      #     data <- matriz_cbc %>% 
      #         filter(`Objetivos estratégicos` == selected_oest())
      # }
      # data
    
    matriz_cbc()
      
  }, rownames = FALSE, selection = "none")
  
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
  
  matriz_est_oest <- reactive({
    file <- paste0("data/estudiantes_matrices/matriz_ii/", res_auth$user, ".csv")
    data <- read_csv(file)
    data %>% 
      mutate(`Objetivos estratégicos` = c(input$est_oest_1, input$est_oest_2, input$est_oest_3))
  })
  
  output$tabla_est_oest <- renderDT({
    matriz_est_oest() %>% select(-user)
  })
  
  matriz_est_cbc <- reactive({
    file <- paste0("data/estudiantes_matrices/matriz_oest/", res_auth$user, ".csv")
    data <- read_csv(file)
    data %>% 
      select(-c(Identidad,`Texto ingresado`)) %>%
      mutate(`CBC` = c(paste0(input$est_cbc_1, collapse = ", "), 
                       paste0(input$est_cbc_2, collapse = ", "), 
                       paste0(input$est_cbc_3, collapse = ", "))
      )
  })
  
  output$tabla_est_cbc <- renderDT({
    matriz_est_cbc() %>% select(-user)
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
    
    updateTabsetPanel(session, "tabset_estudiantes_matrices", selected = "est_cbc")
  })
  
  observeEvent(input$est_cbc_save, {
    data <- matriz_est_cbc() 
    file <- paste0("data/estudiantes_matrices/matriz_cbc/", res_auth$user, ".csv")
    write_csv(data, file)
    showNotification("Guardado correctamente", duration = 2, closeButton = FALSE)
    updateSelectInput(session, "est_cbc_1", value = "")
    updateSelectInput(session, "est_cbc_2", value = "")
    updateSelectInput(session, "est_cbc_3", value = "")
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
      ),
      
      tabPanel(
        title = "CBC",
        value = "est_cbc",
        sidebarLayout(
          sidebarPanel(
            selectInput("est_cbc_1", "¿Qué CBC se alinean con el Objetivo Estratégico 1?", choices = paste("CBC", 1:7), multiple = TRUE),
            selectInput("est_cbc_2", "¿Qué CBC se alinean con el Objetivo Estratégico 2?", choices = paste("CBC", 1:7), multiple = TRUE),
            selectInput("est_cbc_3", "¿Qué CBC se alinean con el Objetivo Estratégico 3?", choices = paste("CBC", 1:7), multiple = TRUE),
            actionButton("est_cbc_save", "Guardar")
            # TODO: guardar data de cbc
          ),
          mainPanel(
            DTOutput("tabla_est_cbc")
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
      ) # ,
      
      # tabPanel(
      #   title = "Objetivos específicos",
      #   sidebarLayout(
      #     sidebarPanel(
      #       tags$h3("Aquí se ve el alineamiento de objetivos estratégicos y objetivos específicos")
      #     ),
      #     mainPanel(
      #       DTOutput("tabla_oesp")
      #     )
      #   )
      # ),
      # tabPanel(
      #   title = "Metas",
      #   sidebarLayout(
      #     sidebarPanel(
      #       tags$h3("Aquí se ven las metas según objetivos específicos")
      #     ),
      #     mainPanel(
      #       DTOutput("tabla_metas")
      #     )
      #   )
      # )
    )
  })
  
}

shinyApp(ui, server)