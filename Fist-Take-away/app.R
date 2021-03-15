

library(shiny)
library(tidyverse)
library(shinyjs)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Shiny app"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
      ),
      
      # main panel
      mainPanel(
         
      )
   )
)

# Define server logic 
server <- function(input, output) {
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

