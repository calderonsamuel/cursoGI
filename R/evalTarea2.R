evalTarea2UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = ns("rubrica1"),
          label = "El alineamiento con el RI presenta",
          choices = c("Coherencia con funciones dispuestas en el RI",
                      "Descripción de las competencias del puesto",
                      "Coherencia entre competencias del puesto y objetivos estratégicos",
                      "Jerarquía en la enumeración de los puestos de la institución")
        ),
        checkboxGroupInput(
          inputId = ns("rubrica2"),
          label = "El monitoreo del MPP presenta",
          choices = c("Nivel de avance en el cumplimiento de las funciones del puesto",
                      "Descripción del nivel de avance",
                      "Posibilidad de mejora para el siguiente periodo",
                      "Alineamiento con las funciones del RI")
        ),
        checkboxGroupInput(
          inputId = ns("rubrica3"),
          label = "La evaluación del MPP presenta",
          choices = c("Competencias definidas para las funciones del puesto",
                      "Porcentaje promedio del logro de la competencia",
                      "Porcentaje de cumplimiento el puesto considerando todas las competencias",
                      "Descripción del estado de las competencias: logradas, en proceso y en incio")
        )
      ),
      mainPanel(
        
      )
    )
  )
}

evalTarea2Server <- function(id){
  moduleServer(id, function(input, output, session){
    
  })
}

evalTarea2App <- function(){
  ui <- fluidPage(
    evalTarea2UI("myTestId")
  )
  server <- function(input, output, session){
    evalTarea2Server("myTestId")
  }
  shinyApp(ui, server)
}

# evalTarea2App()
