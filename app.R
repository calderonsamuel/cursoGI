library(shiny)
library(tidyverse)
library(shinymanager)
library(readxl)
library(DT)
library(flextable)

creds <- read_csv("data/credentials.csv")

componentes_gestion <- read_csv("data/componentes_gestion_list.csv") %>% 
  as.list() %>% 
  map(~.x[!is.na(.x)])

# componentes_gestion_color <- read_csv("data/componentes_gestion.csv")
componentes_gestion_color <- read_excel("data/color_table.xlsx")

data_oest_componentes <- reactive(read_csv("data/oest_componentes.csv"))

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
  ),
  navbarMenu(
    title = "Módulos",
    tabPanel("Módulo I"),
    tabPanel("Módulo II"),
    tabPanel("Módulo III")
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
  
  nombre_usuario <- reactive(res_auth$user)
  
  tipo_de_usuario <- reactive(res_auth$tipo)
  
  csvTableServer("matriz_ii", nombre_alumno)
  csvTableServer("matriz_componentes", nombre_alumno)
  # csvTableServer("matriz_oest", nombre_alumno)
  oest_componentesServer("oest_componentes", data_oest_componentes,componentes_gestion_color)
  
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
  
  matriz_est_componentes <- matriz_est_componentesServer("matriz_est_componentes", nombre_usuario)
  tabla_est_componentesServer("tabla_est_componentes", matriz_est_componentes, componentes_gestion_color)
  
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
    # updateSelectInput(session, "est_componentes_1", value = "")
    # updateSelectInput(session, "est_componentes_2", value = "")
    # updateSelectInput(session, "est_componentes_3", value = "")
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
            matriz_est_componentesInput("matriz_est_componentes", componentes_gestion),
            # selectInput("est_componentes_1", "¿Qué componentes de gestión se alinean con la misión institucional?", choices = componentes_gestion, multiple = TRUE),
            # selectInput("est_componentes_2", "¿Qué componentes de gestión se alinean con la visión institucional?", choices = componentes_gestion, multiple = TRUE),
            # selectInput("est_componentes_3", "¿Qué componentes de gestión se alinean con los valores institucionales??", choices = componentes_gestion, multiple = TRUE),
            actionButton("est_componentes_save", "Guardar")
            # TODO: guardar data de cbc
          ),
          mainPanel(
            tabla_est_componentesOutput("tabla_est_componentes")
            # DTOutput("tabla_est_componentes")
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
            checkboxGroupInput(
              inputId = "evaluacion_oest", 
              label = "Los objetivos estratégicos del PEI presentan",
              choices = c("Alineamiento con misión",
                          "Alineamiento con visión",
                          "Alineamiento con valores institucionales",
                          "Alineamiento con componentes de gestión")),
            textInput(
              inputId = "feedback",
              label = "Retroalimentación",
              placeholder = "Ingrese retroalimentación"
            )
          ),
          mainPanel(
            oest_componentesUI("oest_componentes"),
            # csvTableOutput("matriz_oest"),
            tags$br(),
            textOutput("evaluacion_oest_texto")
          )
        )
      ),
      
      tabPanel(
        title = "Monitoreo PEI",
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput(
              inputId = "monitoreo_pei",
              label = "El monitoreo del PEI presenta",
              choices = c("Construcción a partir de los objetivos específicos.",
                          "Estado de avance en el cumplimiento de metas anuales.",
                          "Descripción del estado de avance.",
                          "Acciones para implementar a partir del avance.")
            )
          ),
          mainPanel(
            matriz_monitoreo_peiUI("matriz_monitoreo_pei"),
            textOutput("evaluacion_monitoreo_pei")
          )
        )
      )
    )
  })
  
  output$evaluacion_oest_texto <- renderText({
    cumplidos <- length(input$evaluacion_oest)
    calificacion <- case_when(
      cumplidos == 0 ~ "Malo",
      cumplidos == 1 ~ "Deficiente",
      cumplidos == 2 ~ "Regular",
      cumplidos == 3 ~ "Bueno",
      cumplidos == 4 ~ "Excelente",
      TRUE ~ ""
    )
    paste("El alumno cumplió con", cumplidos, "condiciones, por eso su nota es", calificacion)
  })
  
  output$evaluacion_monitoreo_pei <- renderText({
    cumplidos <- length(input$monitoreo_pei)
    calificacion <- case_when(
      cumplidos == 0 ~ "Malo",
      cumplidos == 1 ~ "Deficiente",
      cumplidos == 2 ~ "Regular",
      cumplidos == 3 ~ "Bueno",
      cumplidos == 4 ~ "Excelente",
      TRUE ~ ""
    )
    paste("El alumno cumplió con", cumplidos, "condiciones, por eso su nota es", calificacion)
  })
  
  matriz_monitoreo_peiServer("matriz_monitoreo_pei", componentes_gestion_color)
  
}

shinyApp(ui, server)