# Prediction Assignment Writeup 

## 1 Assignment 
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.


## 2 Loading and cleaning the data set
We first set the working directory and load the required R packages

```r
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
```

```r
Loaded gbm 2.1.5
Warning message:
package 'gbm' was built under R version 3.6.1
```

The next step is to load the data from the source stated above and to split the training data set into two parts (75% for creating the models, 25% for validation). The testing data set is not touched and will only be used to generate the answers for the quiz. 

```r
url_train <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_quiz  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

data_train <- read.csv(url(url_train), strip.white = TRUE, na.strings = c("NA",""))
data_quiz  <- read.csv(url(url_quiz),  strip.white = TRUE, na.strings = c("NA",""))

in_train  <- createDataPartition(data_train$classe, p=0.75, list=FALSE)
train_set <- data_train[ in_train, ]
test_set  <- data_train[-in_train, ]
dim(train_set)
```

```r
[1] 14718   160
```

```r
dim(test_set)
```

```r
[1] 4904  160
```

The data sets have 160 variables (columns). The first step is to use the function nearZeroVar to identify Near Zero Variance predictors and to remove them.

```r
nzv_var <- nearZeroVar(train_set)
train_set <- train_set[ , -nzv_var]
test_set  <- test_set [ , -nzv_var]
dim(train_set)
```

```r
[1] 14718   120


```r
dim(test_set)
```

```r
[1] 4904  120
```

The second step is to remove variables which are mostly NAs (threshold = 0.95).

```r
na_var <- sapply(train_set, function(x) mean(is.na(x))) > 0.95
train_set <- train_set[ , na_var == FALSE]
test_set  <- test_set [ , na_var == FALSE]
dim(train_set)
```

```r
[1] 14718    59
```

```
dim(test_set)
```

```r
[1] 4904   59
``` 

The last step is to remove the columns which are only used for identification (1-5).

```r
train_set <- train_set[ , -(1:5)]
test_set  <- test_set [ , -(1:5)]
dim(train_set)
```

```r
[1] 14718    54
```

```r
dim(test_set)
```

```r
[1] 4904   54
```

## 3 Correlation analysis

In order to understand the relation between the varibales better, a correlation analysis is performed. The method is circle (standard) and for order FPC (first principal component) is chosen. 

´´´r
corr_matrix <- cor(train_set[ , -54])
corrplot(corr_matrix, order = "FPC", method = "circle", type = "full",
         tl.cex = 0.7, tl.col = rgb(0, 0, 0))
´´´
<img src="Rplot.png"
     alt="Markdown Monster icon"
     style="float: left; margin-right: 10px;" />




## 4 Model building

### 4.1 Decision Tree Model

### 4.2 Generalized Boosted Model

### 4.3 Random Forest Model

## 5 Applying Random Forest to quiz data