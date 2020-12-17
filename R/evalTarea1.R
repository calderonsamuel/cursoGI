evalTarea1UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = ns("rubrica1"), 
          label = "Los objetivos estratégicos del PEI presentan",
          choices = c("Alineamiento con misión",
                      "Alineamiento con visión",
                      "Alineamiento con valores institucionales",
                      "Alineamiento con los componentes de gestión")
        ),
        checkboxGroupInput(
          inputId = ns("rubrica2"), 
          label = "Los objetivos específicos del PEI presentan",
          choices = c("Alineamiento con objetivos estratégicos",
                      "Alineamiento con metas institucionales",
                      "Alineamiento con los componentes de gestión",
                      "Coherencia con temporalidad para la ejecución de metas")
         ),
        checkboxGroupInput(
          inputId = ns("rubrica3"),
          label = "El monitoreo del PEI presenta",
          choices = c("Construcción a partir de los objetivos específicos",
                      "Estado de avance en el cumplimiento de metas anuales",
                      "Descripción del estado de avance",
                      "Acciones para implementar a partir del avance"),
        ),
        checkboxGroupInput(
          inputId = ns("rubrica4"),
          label = "La evaluación del PEI presenta",
          choices = c("Descripción del cumplimiento de metas anuales",
                      "Descripción del cumplimiento de objetivos estratégicos",
                      "Descripción del cumplimiento de objetivos específicos",
                      "Descripción de la evaluación institucional"),
        )
      ),
      mainPanel(
        
      )
    )
  )
}

evalTarea1Server <- function(id){
  moduleServer(id, function(input, output, session){
    
  })
}

evalTarea1App <- function(){
  ui <- fluidPage(
    evalTarea1UI("myTestId")
  )
  server <- function(input, output, session){
    evalTarea1Server("myTestId")
  }
  shinyApp(ui, server)
}

evalTarea1App()
