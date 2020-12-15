library(shiny)
library(tidyverse)
library(shinymanager)
library(keyring)

creds <- read_csv("data/credentials.csv")

key_set("R-shinymanager-key", "llavesupersegura")

create_db(
  credentials_data = creds,
  sqlite_path = "data/credentials.sqlite", # will be created
  passphrase = key_get("R-shinymanager-key", "llavesupersegura")
  # passphrase = "passphrase_wihtout_keyring"
)

ui <- fluidPage(
  tags$h3("Hola"),
  textOutput("usuario")
)

ui <- secure_app(ui, enable_admin = TRUE)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # auth start
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "data/credentials.sqlite",
      passphrase = key_get("R-shinymanager-key", "llavesupersegura")
      # passphrase = "passphrase_wihtout_keyring"
    )
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  # auth end
  
  output$usuario <- renderText(res_auth$tipo)
}

# Run the application 
shinyApp(ui = ui, server = server)