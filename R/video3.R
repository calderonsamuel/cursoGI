video3UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h1("Evaluación del Plan Anual de Trabajo"),
    tags$p("El siguiente video te mostrará la importancia de la evaluación del Plan Anual de Trabajo y ..."),
    HTML('<iframe width="560" height="315" 
         src="https://www.youtube.com/embed/KlalrOQgSH4" 
         frameborder="0" 
         allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" 
         allowfullscreen></iframe>')
  )
}

video3Server <- function(id){
  moduleServer(id, function(input, output, session){
    
  })
}

video3App <- function(){
  ui <- fluidPage(
    video3UI("myTestId")
  )
  server <- function(input, output, session){
    video3Server("myTestId")
  }
  shinyApp(ui, server)
}

# video3App()