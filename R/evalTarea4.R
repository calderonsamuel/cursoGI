evalTarea4UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = ns("rubrica1"),
          label = "La implementación y seguimiento de metas del PAT presenta",
          choices = c("Alineamiento con objetivos estratégicos",
                      "Alineamiento con objetivos específicos",
                      "Alineamiento con componentes de gestión",
                      "Actividades propuestas para el cumplimiento de metas anuales")
        ),
        checkboxGroupInput(
          inputId = ns("rubrica2"),
          label = "La ejecución y acompañamiento de actividades del PAT presenta",
          choices = c("Componentes de gestión como criterio organizador",
                      "Responsables de las actividades propuestas para cada meta anual",
                      "Presupuesto destinado para la ejecución de las actividades propuestas",
                      "Cronograma de ejecución de las actividades")
        ),
        checkboxGroupInput(
          inputId = ns("rubrica3"),
          label = "La formulación de indicadores del PAT presenta",
          choices = c("Indicadores cualitativos y cuantitativos para una evaluación integral",
                      "Respeto por la estructura planteada en la guía de acompañamiento",
                      "Instrumento de evaluación propuesto para cada tipo de indicador",
                      "Explicaciones sobre el avance en el cumplimiento de los indicadores")
        ),
        checkboxGroupInput(
          inputId = ns("rubrica4"),
          label = "La evaluación del PAT presenta",
          choices = c("Componentes de gestión como criterio organizador",
                      "Visión integradora del logro de las metas anuales con respecto a cada actividad",
                      "Estado del avance de las metras en el periodo correspondiente",
                      "Propuestas de mejoras para el año siguiente")
        ),
        checkboxGroupInput(
          inputId = ns("rubrica5"),
          label = "Las mejoras al PAT 2021 presentan",
          choices = c("Coherencia con las acciones a implementar a partir del avance planteadas en el PEI",
                      "Coherencia con las posibilidades de mejora al 2021 planteadas en el MPP 2020",
                      "Coherencia con las mejoras al 2021 planteadas por el PAT 2020",
                      "Alineamiento con las demás herramientas de gestión institucional")
        )
      ),
      mainPanel(
        
      )
    )
  )
}

evalTarea4Server <- function(id){
  moduleServer(id, function(input, output, session){
    
  })
}

evalTarea4App <- function(){
  ui <- fluidPage(
    evalTarea4UI("myTestId")
  )
  server <- function(input, output, session){
    evalTarea4Server("myTestId")
  }
  shinyApp(ui, server)
}

# evalTarea4App()
