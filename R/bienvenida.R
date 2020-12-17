bienvenidaUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h1("Bienvenidos"),
    tags$p("La Dirección de Servicios de Educación Técnico-Productiva y Superior 
           Tecnológica y Artística (DISERTPA) les da la bienvenida al Curso monitoreo 
           de la gestión institucional, dirigido a directores y responsables de gestión 
           pedagógica de la Educación Superior Tecnológica. 
           Este curso forma parte de la capacitación que la DISERTPA viene implementando 
           con el objetivo de brindar herramientas que permitan fortalecer las competencias 
           y desempeños del directivo."),
    tags$p("El curso tiene como finalidad sustentar el monitoreo y evaluación del Plan 
           Anual de Trabajo, teniendo en cuenta los objetivos, metas y actividades 
           dispuestos a partir de los demás instrumentos de gestión, involucrando 
           a la comunidad educativa y a los grupos de interés, demostrando responsabilidad 
           y compromiso con su función social. Asimismo, busca fortalecer el desempeño de 
           los directivos mediante la aplicación del principio de alineamiento en la 
           elaboración de sus herramientas de gestión educativa, lo que les permitirá 
           orientar su trabajo al logro de resultados de manera eficaz."),
    tags$p("El curso está organizado en las siguientes tres unidades temáticas:"),
    tags$ul(
      tags$li("Proyecto Educativo Institucional"),
      tags$li("Manual de Perfil de Puesto"),
      tags$li("Plan Anual de Trabajo")
    ),
    tags$p("Esperamos que este curso sea una experiencia gratificante y significativa 
           de aprendizaje, de manera que los directores y responsables de gestión pedagógica 
           de la Educación Superior Tecnológica puedan reflexionar sobre las temáticas que 
           se tratarán y participar en todas las actividades propuestas."),
    tags$h4("¡Éxitos en el curso!")
  )
}

bienvenidaServer <- function(id){
  moduleServer(id, function(input, output, session){
    
  })
}

bienvenidaApp <- function(){
  ui <- fluidPage(
    bienvenidaUI("myTestId")
  )
  server <- function(input, output, session){
    bienvenidaServer("myTestId")
  }
  shinyApp(ui, server)
}

bienvenidaApp()
