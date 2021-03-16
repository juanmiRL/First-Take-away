#library(tidyverse)
library(shiny)
library(dplyr)
library(funModeling)
library(DataExplorer)
library(shinyjs)
library(shinythemes)
library(plotly)
library(DT)
library(reticulate)


#devtools::install_git("https://github.com/bernardo-dauria/kaggler.git")

#library(kaggler)
#kgl_auth(username:"juannmi86",key:"8e8a6dd50edcd417a545f9d8b1c26765")

#reticulate::use_python("C:/Users/juanm/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
#reticulate::py_config()
#kaggle <- import("kaggle")
#kaggle$api$authenticate()
#kaggle$api$dataset_download_files("mathchi/diabetes-data-set", "diabetes.csv", unzip = T)
#kaggle$api$dataset_download_files("jsphyg/weather-dataset-rattle-package", "weatherAUS.csv", unzip = T)

library(readr)
data <- read.csv("https://raw.githubusercontent.com/juanmiRL/First-Take-away/main/Fist-Take-away/diabetes.csv")
bank <- read.csv("https://raw.githubusercontent.com/juanmiRL/First-Take-away/main/Fist-Take-away/bank.csv")

# create a function to generate na`s in the data 
generate_na <- function(data){
last_column <- length(data)
target <- data[last_column]
data <- data[,-last_column]
data <- as.data.frame(lapply(data, function(x) 
  "is.na<-"(x, sample(seq(x), floor(length(x) * runif(1, 0, .2))))))
data <- cbind(data,target)
}

# Define UI 
ui <- fluidPage(
  
  navbarPage("Classification Shiny App",
             theme = shinytheme("yeti")
             , #theme
             tabPanel("Data Description",
                      fluidPage(fileInput("file",h3("Upload the file")), 
                                  
                        mainPanel(
                          tabsetPanel(type = "pills", 
                                      tabPanel("Table",br(), DT::dataTableOutput("datatable")),
                                      tabPanel("Data Description",br(), tableOutput("data_desc")),
                                      tabPanel("Summary",br(), tableOutput("summary")),
                                      tabPanel("Numeric Variables",br(), tableOutput("numeric"))
                                      
                                      
                          )
                        ))),
             tabPanel("Preprocess",
                      fluidPage(
                        mainPanel(tabsetPanel(type = "pills",
                                              tabPanel("Transform variable", 
                                                       wellPanel(
                                                             selectInput("select", label = h3("Select feature"), 
                                                                                choices = colnames(data),
                                                                                selected = 1),
                                                             br(),
                                                             checkboxGroupInput("transform",label = h3("Select a type"), choices = list("Factor","Integer","Numeric","Character")),br(),
                                                             actionButton("Gotransform", "Carry out the transform", class = "btn-success"),br()
                                                            
                                                            
                                                       )
                                                                    
                                                       ),
                                              tabPanel("Detect NA`s", plotOutput("plotna")),
                                              tabPanel("Create NA`s",
                                                       br(),
                                                       br(),
                                                       actionButton("na", "Click to generate missing values in the data", 
                                                                    class = "btn-success"),
                                                       br(),
                                                       plotOutput("plotna2")),
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
  
  output$datatable <- DT::renderDT(data)
  output$data_desc <- renderTable(skimr::skim_without_charts(data))
  output$summary <- renderTable(summary(data))
  output$numeric <- renderTable(profiling_num(data))
  
  output$plotna <- renderPlot(plot_missing(data,group = list(Good = 0.05, OK = 0.4, Bad = 0.8, Remove = 1),
                                           missing_only = FALSE,
                                           geom_label_args = list(),
                                           title = "Missing values in the data set",
                                           ggtheme = theme_minimal(),
                                           theme_config = list(legend.position = c("bottom"))))
  
  
  output$plotna2 <- renderPlot(plot_missing(data,group = list(Good = 0.05, OK = 0.4, Bad = 0.8, Remove = 1),
                                           missing_only = FALSE,
                                           geom_label_args = list(),
                                           title = "Missing values in the data set",
                                           ggtheme = theme_minimal(),
                                           theme_config = list(legend.position = c("bottom"))))
  cols_to_change <- reactive({colnames(data)})
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

