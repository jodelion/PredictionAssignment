setwd("C:/Users/P358774/Coursera/Prediction Assignment")

library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(corrplot)
library(rattle)
library(randomForest)
library(RColorBrewer)
library(e1071)
library(gbm)

set.seed(100)

url_train <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_quiz  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

data_train <- read.csv(url(url_train), strip.white = TRUE, na.strings = c("NA",""))
data_quiz  <- read.csv(url(url_quiz),  strip.white = TRUE, na.strings = c("NA",""))

in_train  <- createDataPartition(data_train$classe, p=0.75, list=FALSE)
train_set <- data_train[ in_train, ]
test_set  <- data_train[-in_train, ]

nzv_var <- nearZeroVar(train_set)
train_set <- train_set[ , -nzv_var]
test_set  <- test_set [ , -nzv_var]

na_var <- sapply(train_set, function(x) mean(is.na(x))) > 0.95
train_set <- train_set[ , na_var == FALSE]
test_set  <- test_set [ , na_var == FALSE]

train_set <- train_set[ , -(1:5)]
test_set  <- test_set [ , -(1:5)]

# Correlation Analysis
corr_matrix <- cor(train_set[ , -54])
corrplot(corr_matrix, order = "FPC", method = "circle", type = "full",
         tl.cex = 0.7, tl.col = rgb(0, 0, 0))

# Decision Tree Model
set.seed(100)
fit_decision_tree <- rpart(classe ~ ., data = train_set, method="class")
fancyRpartPlot(fit_decision_tree)

predict_decision_tree <- predict(fit_decision_tree, newdata = test_set, type="class")
conf_matrix_decision_tree <- confusionMatrix(predict_decision_tree, test_set$classe)
conf_matrix_decision_tree

plot(conf_matrix_decision_tree$table, col = conf_matrix_decision_tree$byClass, 
     main = paste("Decision Tree Model - Accuracy =",
                  round(conf_matrix_decision_tree$overall['Accuracy'], 3)))

# Generalized Boosted Model
set.seed(100)
ctrl_GBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
fit_GBM  <- train(classe ~ ., data = train_set, method = "gbm",
                  trControl = ctrl_GBM, verbose = FALSE)
fit_GBM$finalModel

predict_GBM <- predict(fit_GBM, newdata = test_set)
conf_matrix_GBM <- confusionMatrix(predict_GBM, test_set$classe)
conf_matrix_GBM

plot(conf_matrix_GBM$table, col = conf_matrix_GBM$byClass, 
     main = paste("Generalized Boosted Model - Accuracy =", round(conf_matrix_GBM$overall['Accuracy'], 3)))

# Random Forest Model
set.seed(100)
ctrl_RF <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
fit_RF  <- train(classe ~ ., data = train_set, method = "rf",
                 trControl = ctrl_RF, verbose = FALSE)
fit_RF$finalModel

predict_RF <- predict(fit_RF, newdata = test_set)
conf_matrix_RF <- confusionMatrix(predict_RF, test_set$classe)
conf_matrix_RF

plot(conf_matrix_RF$table, col = conf_matrix_RF$byClass, 
     main = paste("Random Forest Model - Accuracy =", round(conf_matrix_RF$overall['Accuracy'], 3)))

# Random Forest Model applied to testing data set
predict_quiz <- predict(fit_RF, newdata = data_quiz)
predict_quiz







