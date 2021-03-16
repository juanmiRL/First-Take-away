#library(tidyverse)
library(shiny)
library(mice)
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


library(readr)
data <- read.csv("https://raw.githubusercontent.com/juanmiRL/First-Take-away/main/Fist-Take-away/diabetes.csv")

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
                      fluidPage(fileInput("file",h3("Upload you own file")), 
                                  
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
                                              tabPanel("Detect and create NA`s",
                                                       br(),
                                                       br(),
                                                       p(strong("Note that once you click in the button below the Data Description changes also.")),
                                                       p(strong("You can click as many time as you wish.")),
                                                       actionButton("na", "Click to generate missing values in the data", 
                                                                    class = "btn-success"),
                                                       actionButton("reset", "Click to restore the initial dataset", 
                                                                    class = "btn-success"),
                                                       hr(),
                                                       plotOutput("plotna")),
                                              tabPanel("Impute NA`s",
                                                       br(),
                                                       p(strong("Note that once you click in the button below the Data Description changes also.")),
                                                       wellPanel(
                                                       
                                                       sliderInput("numberk","Number of neighboors in KNN", min = 1,max = 10,value = 5),
                                                       actionButton("imputena", "Click to impute missing values with mice", 
                                                                    class = "btn-success")
                                                       )
                                                       ),
                                              tabPanel("Transform variable", 
                                                       br(),
                                                       p(strong("Note that once you click in the button below the Data Description changes also.")),
                                                       wellPanel(
                                                         selectInput("class_var", label = h3("Select feature"), 
                                                                     choices = colnames(data),
                                                                     selected = 1),
                                                         br(),
                                                         checkboxGroupInput("choose_class",label = h3("Select data type"), 
                                                                            choices = list(Numeric = "Numeric",Factor = "Factor", Character = "Character")),
                                                         br(),
                                                         actionButton("chg_class", "Carry out the transformation", class = "btn-success")
                                                         
                                                         
                                                       )
                                                       
                                              )
                                              
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
  
  output$datatable <- DT::renderDT(v$data)
  output$data_desc <- renderTable(skimr::skim_without_charts(v$data))
  output$summary <- renderTable(summary(v$data))
  output$numeric <- renderTable(profiling_num(v$data))
  
  v <- reactiveValues(data = data)
  
  observeEvent(input$chg_class,{
    if( input$choose_class == "Numeric"){
      v$data[, input$class_var] <- as.numeric(v$data[, input$class_var])
    } else if( input$choose_class == "Factor"){
      v$data[, input$class_var] <- as.factor(v$data[, input$class_var])
    } else if( input$choose_class == "Character"){
      v$data[, input$class_var] <- as.character(v$data[, input$class_var])
    } 
  })
  
  
  observeEvent(input$imputena, {
    v$data <- mice(data, k=input$numberk)
    v$data <- complete(v$data)
  })  
  
  observeEvent(input$na, {
      v$data <- generate_na(data)
  })
  
  observeEvent(input$reset, {
    v$data <- data
  })  
  
  output$plotna <- renderPlot(plot_missing(v$data,group = list(Good = 0.05, Bad = 0.4, Remove = 0.8, Remove = 1),
                                           missing_only = FALSE,
                                           geom_label_args = list(),
                                           title = "Missing values in the data set",
                                           ggtheme = theme_minimal(),
                                           theme_config = list(legend.position = c("bottom"))))
  cols_to_change <- reactive({colnames(data)})
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

