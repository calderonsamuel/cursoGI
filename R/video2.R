video2UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h1("Evaluación del Manual de Perfiles de Puesto"),
    tags$p("El siguiente video te mostrará la importancia de la evaluación del Manual de Perfiles de Puesto y ..."),
    HTML('<iframe width="560" height="315" 
         src="https://www.youtube.com/embed/9tvQRniJQy0" 
         frameborder="0" 
         allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" 
         allowfullscreen></iframe>')
  )
}

video2Server <- function(id){
  moduleServer(id, function(input, output, session){
    
  })
}

video2App <- function(){
  ui <- fluidPage(
    video2UI("myTestId")
  )
  server <- function(input, output, session){
    video2Server("myTestId")
  }
  shinyApp(ui, server)
}

# video2App()
