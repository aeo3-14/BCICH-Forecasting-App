library(shiny)

source('ui.R', local = TRUE)
source('server.R')

# Run the application
shinyApp(ui = ui, server = server)
