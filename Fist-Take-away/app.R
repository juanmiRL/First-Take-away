library(tidyverse)
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
library(htmlwidgets)
library(caret)



#devtools::install_git("https://github.com/bernardo-dauria/kaggler.git")

library(kaggler)
#kgl_auth("juannmi86","8e8a6dd50edcd417a545f9d8b1c26765")
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
                      fluidPage(
                        a("Please wait, the app is loading.."),
                        fileInput("file",h3("Upload you own file")), 
                                  
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
                                                       wellPanel(
                                                       actionButton("na", "Click to generate missing values in the data", 
                                                                    class = "btn-success"),
                                                       actionButton("reset", "Click to restore the initial dataset", 
                                                                    class = "btn-success")
                                                       ),
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
                        mainPanel(
                          tabsetPanel(type = "pills",
                                      tabPanel("Univariate", 
                                               sidebarLayout(
                                                  sidebarPanel(
                                                    
                                                      selectInput("featureplot", label = h4("Select feature"), 
                                                                  choices = colnames(data),
                                                                  selected = 1),
                                                      selectInput("plot_var", label = h4("Select type of graph"), 
                                                                  choices = list(Histogram = "Histogram", Boxplot = "Boxplot",Density = "Density"),
                                                                  selected = 1),
                                                      selectInput("colors", label = h4("Select color"), 
                                                                  choices = list("SteelBlue","Yellowgreen","Gold","Tomato","Sandybrown","Orange"),
                                                                  selected = 2)
                                                      
                                                      
                                                    
                                                  ),
                                                  mainPanel(plotOutput("uniplot"))
                                               
                                                 )),
                                      tabPanel("Bivariate", 
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   
                                                   selectInput("featureplot2", label = h4("Select x-axis feature"), 
                                                               choices = colnames(data),
                                                               selected = 1),
                                                   selectInput("featureplot3", label = h4("Select y-axis feature"), 
                                                               choices = colnames(data),
                                                               selected = colnames(data)[2]),
                                              
                                                   selectInput("plot_var2", label = h4("Select type of graph"), 
                                                               choices = list( Boxplot = "Boxplot",Scatterplot = "scatterplot"),
                                                               selected = 1),
                                                   selectInput("colors2", label = h4("Select color"), 
                                                               choices = list("SteelBlue","Yellowgreen","Gold","Tomato","Sandybrown","Orange"),
                                                               selected = 2)
                                                   
                                                   
                                                   
                                                 ),
                                                 mainPanel(plotOutput("biplot"))
                                                 
                                               )),
                                      tabPanel("Plots by target", 
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   
                                                   selectInput("target", label = h4("Select target"), 
                                                               choices = colnames(data),
                                                               selected = as.factor(colnames(data)[length(data)])),
                                                   
                                                   selectInput("featureplot4", label = h4("Select x-axis feature"), 
                                                               choices = colnames(data),
                                                               selected = 1),
                                                   
                                              
                                                   selectInput("plot_var4", label = h4("Select type of graph"), 
                                                               choices = list( Boxplot = "Boxplot",Barplot = "barplot"),
                                                               selected = 1)
                                                   
                                                   
                                                   
                                                   
                                                 ),
                                                 mainPanel(plotOutput("biplot2"))
                                                 
                                               )),
                                      tabPanel("Dynamic plots", 
                                               plotOutput("dynaplot"))
                                      
                                      
                          )
                        
                        ))),
             tabPanel("Classification models",
                      fluidPage(
                        mainPanel(
                          tabsetPanel(type = "pills",
                                      tabPanel("Logistic Regression", 
                                               br(),
                                               p(strong("Confussion matrix for the Logistic model")),
                                               br(),
                                                
                                                 verbatimTextOutput("Modelsummary")
                                              
                                                 
                                               ),   
                                               
                                
                                      tabPanel("Random Forest",
                                               br(),
                                               p(strong("Confussion matrix for the Random Forest model")),
                                               br(),
                                               verbatimTextOutput("Modelsummary2")
                                      ),
                                              
                                      tabPanel("Variable importance",
                                               br(),
                                               br(),
                                               p(strong("Variable importance plot for the Logistic Regression model")),
                                               br(),
                                               plotOutput("varimp"),
                                               br(),
                                               p(strong("Variable importance plot for the Random Forest model")),
                                               br(),
                                               plotOutput("varimp2")
                                      
                                      )           
                          
                        )))),
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
  
 
  
  output$uniplot <-  renderPlot(
      if(input$plot_var == "Histogram"){
         ggplot(v$data, aes_string(input$featureplot)) + geom_histogram(fill=input$colors) + ggtitle(paste("Histogram ",input$featureplot)) + theme_minimal()
      } else if (input$plot_var == "Boxplot"){
        ggplot(v$data, aes_string(input$featureplot)) + geom_boxplot(fill=input$colors)+ coord_flip() + ggtitle(paste("Boxplot ",input$featureplot)) + theme_minimal()
      } 
        else if (input$plot_var == "Density"){
        ggplot(v$data, aes_string(input$featureplot)) + geom_density(fill=input$colors,stat = "density",alpha=.2)+ ggtitle(paste("Density plot ",input$featureplot)) + theme_minimal()
      } 
  )
  
  
  output$biplot <-  renderPlot(
      if(input$plot_var2 == "Boxplot"){
         ggplot(v$data, aes_string(x=input$featureplot2,y=input$featureplot3)) +  geom_boxplot(fill=input$colors2) + ggtitle(paste("Boxplot",input$featureplot2,"vs",input$featureplot3)) + theme_minimal()
      } else if (input$plot_var2 == "scatterplot"){
        ggplot(v$data, aes_string(input$featureplot2,input$featureplot3,group=1)) + geom_point(color=input$colors2) + ggtitle(paste("Scatterplot ",input$featureplot2,"vs",input$featureplot3)) + theme_minimal()
      } 
  )
  
  output$biplot2 <-  renderPlot(
      if(input$plot_var4 == "Boxplot"){
         ggplot(v$data, aes_string(x=input$featureplot4,group=input$target,fill=input$target)) +  geom_boxplot() + coord_flip()+ ggtitle(paste("Boxplot",input$featureplot4,"by",input$target)) + theme_minimal()
      } else if (input$plot_var4 == "barplot"){
        ggplot(v$data, aes_string(input$featureplot4,group=input$target,fill=input$target)) + geom_bar() + ggtitle(paste("Barplot ",input$featureplot4,"by",input$target)) + theme_minimal()
      } 
  )
  
  # split data 
  set.seed(100351855)
  split <- caret::createDataPartition(data[,length(data)], p = 0.7, list = FALSE)
  data_train <- data[split,]
  data_train[,length(data_train)] <- as.factor(data_train[,length(data_train)]) 
  data_test <- data[-split,]
  data_test2 <- data_test[,-length(data)]
  target <- data_test[,length(data)]
  name_target <- colnames(data_train)[length(data_train)]
  rf_model <- caret::train(Outcome~., 
                           method = "rf",
                           data = data_train,
                           metric = "Accuracy"
                           )
  logit_model <- caret::train(Outcome~., 
                              method = "glmnet",
                              data = data_train,
                              metric = "Accuracy"
  )
  
  
  
  output$Modelsummary <- renderPrint(caret::confusionMatrix(predict(logit_model,data_test2),as.factor(target)))
  output$Modelsummary2 <- renderPrint(caret::confusionMatrix(predict(rf_model,data_test2),as.factor(target)))
  output$varimp <- renderPlot(plot(caret::varImp(logit_model)))
  output$varimp2 <- renderPlot(plot(caret::varImp(rf_model)))
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

