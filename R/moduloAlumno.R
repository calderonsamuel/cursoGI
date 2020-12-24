moduloAlumnoUI <- function(id) {
  ns <- NS(id)
  tagList(
    navlistPanel(
      widths = c(2, 10),
      "Inicio",
      tabPanel(
        title = "Evaluación de entrada",
        tags$h3("¡Bienvenido a la evaluación de entrada!")
        ),
      
      "Modulo I: Proyecto Educativo Institucional",
      "",
      tabPanel(
        title = "Video 1: Evaluación del PEI",
        video1UI("video1")
        ),
      tabPanel(
        title = "Foro 1: Alineamiento y cumplimiento del PEI en la actualidad"
        ),
      tabPanel(
        title = "Tarea 1: Alineamiento de los componentes del PEI",
        tabsetPanel(type = "pills",
          tabPanel(
            title = "Alineamiento estratégico",
            aliEstrategicoUI(ns("aliestrategico"))
          ),
          tabPanel(
            title = "Alineamiento específico",
            aliEspecificoUI(ns("aliespecifico"))
          ),
          tabPanel(
            title = "Monitoreo del PEI",
            monitoreoPEI_UI(ns("monitoreoPEI"))
          ),
          tabPanel(
            title = "Evaluación del PEI",
            evaluacionPEI_UI(ns("evaluacionPEI"))
          )
        )
      ),
      
      "Modulo II: Manual de Perfil de Puesto",
      "",
      tabPanel(
        title = "Video 2: Evaluación del MPP",
        video2UI("video2")
        ),
      tabPanel(
        title = "Foro 2: Alineamiento y cumplimiento de perfiles"
        ),
      tabPanel(
        title = "Tarea 2: Alineamiento de los componentes del MPP",
        tabsetPanel(type = "pills",
          tabPanel(
            title = "Alineamiento con RI"
          ),
          tabPanel(
            title = "Monitoreo del MPP"
          ),
          tabPanel(
            title = "Evaluación del MPP"
          )
        )
      ),
      
      "Modulo III: Plan Anual de Trabajo",
      "",
      tabPanel(
        title = "Video 3: Evaluación del PAT alineado",
        video3UI("video3")
        ),
      tabPanel(
        title = "Foro 3: Importancia de la evaluación operativa para el éxito de la gestión institucional"
        ),
      tabPanel(
        title = "Tarea 3: Evaluación y monitoreo del PAT en coherencia con evaluación del PEI y MPP",
        tabsetPanel(
          tabPanel(
            title = "Alineamiento de act."
          ),
          tabPanel(
            title = "Ejecución y acompañamiento de act."
          ),
          tabPanel(
            title = "Formulación de indicadores"
          ),
          tabPanel(
            title = "Evaluación del PAT"
          )
        )
      ),
      tabPanel(
        title = "Tarea 4: Matrices de alineamiento del PAT 2021",
        tabsetPanel(
          tabPanel(
            title = "Alineamiento de act."
          ),
          tabPanel(
            title = "Ejecución y acompañamiento de act."
          ),
          tabPanel(
            title = "Formulación de indicadores"
          ),
          tabPanel(
            title = "Evaluación del PAT"
          )
        )
      ),
      
      "Cierre",
      tabPanel(
        title = "Evaluación de salida"
        ),
      tabPanel(
        title = "Encuesta de satisfacción"
        )
      )
    )
}

moduloAlumnoServer <- function(id, nombre_usuario) {
  moduleServer(id, function(input, output, session) {
    video1Server("video1")
    video2Server("video2")
    video3Server("video3")
    aliEstrategicoServer("aliestrategico", nombre_usuario)
    aliEspecificoServer("aliespecifico", nombre_usuario)
    monitoreoPEI_Server("monitoreoPEI", nombre_usuario)
    evaluacionPEI_Server("evaluacionPEI", nombre_usuario)
  })
}

moduloAlumnoApp <- function(){
  ui <- fluidPage(
    # theme = bs_theme(version = 3, bootswatch = "flatly"),
    moduloAlumnoUI("myTestId")
  )
  server <- function(input, output, session) {
    nombre_usuario <- reactive("alumno1")
    moduloAlumnoServer("myTestId", nombre_usuario)
  }
  shinyApp(ui, server)
}

# moduloAlumnoApp()
