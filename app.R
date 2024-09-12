#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

source("shiny-app/ui.R")
source("shiny-app/server.R")

# Run the application 
shinyApp(ui = ui, server = server)
