

library(shiny)
library(tidyverse)
library(shinyjs)
library(shinythemes)
library(plotly)
library(DT)
library(reticulate)

devtools::install_git("https://github.com/bernardo-dauria/kaggler.git")
library(readr)
library(kaggler)
kgl_auth(username:"juannmi86",key:"8e8a6dd50edcd417a545f9d8b1c26765")

reticulate::use_python("C:/Users/juanm/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
reticulate::py_config()
kaggle <- import("kaggle")
kaggle$api$authenticate()
kaggle$api$dataset_download_files("mathchi/diabetes-data-set", "diabetes.csv", unzip = T)
kaggle$api$dataset_download_files("jsphyg/weather-dataset-rattle-package", "weatherAUS.csv", unzip = T)

# Define UI 
ui <- fluidPage(
  
  navbarPage("Classification Shiny app",
             theme = shinytheme("superhero")
             , #theme
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

