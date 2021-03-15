

library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(plotly)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage("Classification Shiny app",
             theme = shinytheme("superhero")
             ,#theme
             tabPanel("Data Description",
                      fluidPage(
                        mainPanel(
                          tabsetPanel(type = "pills", 
                                      tabPanel("Table", tableOutput("datatable")),
                                      tabPanel("Data Description", tableOutput("data_desc")),
                                      tabPanel("Summary", verbatimTextOutput("summary"))
                                      
                          )
                        ))),
             tabPanel("Preprocess",
                      fluidPage(
                        mainPanel(tabsetPanel(type = "pills",
                                              tabPanel("Detect NA`s", plotOutput("price")),
                                              tabPanel("Imput NA`s", plotOutput("age"))
                                              
                        )
                        
                        ))),
             
             tabPanel("Dynamic Plots",
                      fluidPage(
                        
                        )),
             tabPanel("Classification models",
                      fluidPage(
                        mainPanel(
                          tabsetPanel(type = "pills",
                                      tabPanel("Adaboost", verbatimTextOutput("Modelsummary")),
                                      tabPanel("Random Forest", plotOutput("Modelplot1")),
                                      tabPanel("SVM", plotOutput("Modelplot2"))
                                      
                                      
                          )
                        ))),
             tabPanel("Predict class new data",
                      fluidPage(
                        
                        ),
                        
                        mainPanel(
                          h3("The class of the new data is:"),
                          textOutput("predict")
                        )
                        )
                      )
)
             
             
 # navbarPage
  


# Define server logic 
server <- function(input, output) {
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

