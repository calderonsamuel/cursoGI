video1UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h1("Evaluación del Proyecto Educativo Institucional"),
    tags$p("El siguiente video te mostrará la importancia de la evaluación del PEI y ..."),
    HTML('<iframe width="560" 
              height="315" 
              src="https://www.youtube.com/embed/gOcDSsbWbEk" 
              frameborder="0" 
              allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" 
              allowfullscreen></iframe>')
  )
}

video1Server <- function(id){
  moduleServer(id, function(input, output, session){
    
  })
}

video1App <- function(){
  ui <- fluidPage(
    video1UI("myTestId")
  )
  server <- function(input, output, session){
    video1Server("myTestId")
  }
  shinyApp(ui, server)
}

# video1App()
