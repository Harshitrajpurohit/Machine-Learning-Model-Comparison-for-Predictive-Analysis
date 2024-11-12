# Load necessary libraries
library(ggplot2)
library(shiny)
library(shinydashboard)
library(DT)
library(class)
library(kernlab)
library(caTools)
library(e1071)
library(rpart)
library("rpart.plot")

data <- read.csv("D:/Predictive Project/heart_failure_clinical_records_dataset.csv")
data <- data.frame(data)
set.seed(123) 

data$DEATH_EVENT <- as.factor(data$DEATH_EVENT)
train_index <- sample(1:nrow(data), 0.90 * nrow(data))
train_data <- data[train_index,]
test_data <- data[-train_index,]
train_data_label <- data[train_index,12]
test_data_label <- data[-train_index,12]


# KNN
knn_algo <-knn(train_data,test_data,cl=train_data_label,k=12)
knn_tb <-table(knn_algo,test_data_label)
accuracy_fun <- function(x){sum(diag(x)) / sum(rowSums(x)) * 100}
accuracy_knn = accuracy_fun(knn_tb)

# NB
nb_model <- naiveBayes(DEATH_EVENT ~ ., data = train_data)
pred_nb<- predict(nb_model,test_data)
actual = test_data$DEATH_EVENT
confusion_matrix_nb <- table(pred_nb,actual)
accuracy_nb <- sum(pred_nb == actual)/ length(actual)*100

# DT
target <- DEATH_EVENT ~ .
tree <- rpart(target, data = train_data, method = "class")
pred_DT <- predict(tree,test_data,type = "class")
actual = test_data$DEATH_EVENT
confusion_matrix_DT <- table(pred_DT,actual)
accuracy_DT <- sum(pred_DT == actual)/ length(actual)*100

# SVM
svm <- ksvm(DEATH_EVENT ~ ., data = train_data, kernel = "vanilladot")
pred_svm <- predict(svm, test_data)
pred_svm <- factor(pred_svm, levels = levels(test_data$DEATH_EVENT))
confusion_matrix_svm <- table(pred_svm, test_data$DEATH_EVENT)
accuracy_svm <- sum(pred_svm == test_data$DEATH_EVENT) / length(test_data$DEATH_EVENT) *100


# LM
logistic_model <- glm(DEATH_EVENT ~ ., data = train_data, family = binomial)
pred_lm <- predict(logistic_model, newdata = test_data, type = "response")
y_pred <- ifelse(pred_lm > 0.5, 1, 0)
confusion_mtx_lm <- table(test_data$DEATH_EVENT, y_pred)
accuracy_lm <- sum(diag(confusion_mtx_lm)) / sum(confusion_mtx_lm) * 100



ui<- dashboardPage(
  dashboardHeader(title = "ML Algorithms"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Comparision", tabName = "Comparision"),
      menuItem("Data",tabName = "Table")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Comparision",
              fluidRow(
                infoBox("Total Data",h3(nrow(data)),icon = icon("database")),
                valueBox(h4("KNN Accuracy"),sprintf("%.2f%%",accuracy_knn),color = "green" ,icon = icon("database")),
                valueBox(h4("Naive Bayes"),sprintf("%.2f%%",accuracy_nb),color = "red" ,icon = icon("database")),
                valueBox(h4("Decision Tree"),sprintf("%.2f%%",accuracy_DT),color = "red" ,icon = icon("database")),
                valueBox(h4("SVM Accuracy"),sprintf("%.2f%%",accuracy_svm),color = "yellow" ,icon = icon("database")),
                valueBox(h4("Logistic Regression"),sprintf("%.2f%%",accuracy_lm),color = "green" ,icon = icon("database"))
                
              ),
              fluidRow(
                box(
                  title = "Predictive Model Accuracy Comparison",status = "primary",solidHeader = T,background = "aqua",
                  width = 12,
                  height = 520,
                  plotOutput("barplot", height = 450)
                )

              ),
              fluidRow(
                box(
                  title = "KNN: Actual vs Predicted Death Event",status = "primary",solidHeader = T,background = "aqua",
                  plotOutput("KNN_predict")
                ),
                box(
                  title = "Decision Tree plot",status = "primary",solidHeader = T,background = "aqua",
                  plotOutput("DT_plot")
                )
              )
      ),
      tabItem(tabName = "Table",
              h2("Complete Table"),
              br(),
            dataTableOutput("Heart_data_table"),
      )
    )
    
  )
)

server <- shinyServer(function(input,output){
  output$barplot <- renderPlot({
    accuracy <- data.frame(
      Model = c("KNN", "Decision Tree", "Logistic Regression", "Naive Bayes", "SVM"),
      Accuracy = c(accuracy_knn, accuracy_DT, accuracy_lm, accuracy_nb, accuracy_svm)
    )
    barplot(accuracy$Accuracy, names.arg = accuracy$Model, 
            col = c("#fd7f6f", "#7eb0d5", "#b2e061", "#bd7ebe", "#ffb55a"), border = "black",
            xlab = "Model", ylab = "Accuracy",
            ylim = c(0, 100))
  })
  
  output$DT_plot <- renderPlot({
    rpart.plot(tree)
  })
  
  output$KNN_predict <-renderPlot({
    df <- data.frame(Actual = actual, Predicted = pred_DT)
    ggplot(df) +
      geom_point(aes(x = Actual, y = Predicted, size = Actual), color = 'blue') + 
      geom_point(aes(x = Actual, y = Actual, size = Predicted), color = 'red') +  
      geom_abline(slope = 1, intercept = 0, color = 'black', linetype = "dashed") +
      labs(x = "Actual Values", 
           y = "Predicted Values") +
      theme_minimal()
    
    
    
  })
  
  output$Heart_data_table <- renderDataTable(
    data
  )

})

shinyApp(ui=ui,server = server)
