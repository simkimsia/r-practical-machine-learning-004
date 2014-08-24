## Project requirements
## You should create a report describing how you built your model, how you used cross validation,
## what you think the expected out of sample error is, and why you made the choices you did.
## You will also use your prediction model to predict 20 different test cases.

## Objectives
## 1. Your submission should consist of a link to a Github repo with your R markdown and compiled HTML
## file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number
## of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages
## branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).

## 2. You should also apply your machine learning algorithm to the 20 test cases available in the test data
## above. Please submit your predictions in appropriate format to the programming assignment for automated grading.
## See the programming assignment for additional details.

## use the packages required
library(caret)
library(data.table)
library(doMC)
library(ggplot2)
library(knitr)
library(randomForest)
library(xtable)

## registering the multicore parallel 
## backend with the foreach
## see http://www.inside-r.org/packages/cran/domc/docs/registerDoMC
registerDoMC(cores = 4)

## download function to download
## the training and testing data files
download_files <- function() {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv")
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv")
}

## read function to read
## the training and testing data files
## while setting all (divide by zero) values to empty strings
read_file <- function(file) {
  fread(file, na.strings=c("#DIV/0!", ""))
}

## extract the raw data from the files into dataframe
training_data <- read_file("pml-training.csv")
testing_data <- read_file("pml-testing.csv")

set.seed(13)

## extract the na columns
na_cols <- training_data[,sapply(.SD, function(x) any(is.na(x)))]

## function to drop unwanted columns
drop_cols <- function(x) {
  x[,!c("V1", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window"),with=F]
}

## function to transform features where needed
transform_features <- function(x) {
  x[,classe:=factor(classe)]
}

## we only attempt on columns that have valid values
training_features <- drop_cols(training_data[,eval(names(which(na_cols == F))),with=F])

## generate the predictions
generate_predictions <- function(x) {
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

## this is where we submit the predictions
submit_predictions <- function(x, validation) {
  training_data <- createDataPartition(x$classe, p=.60, list=FALSE)
  train <- x[training_data[,1]]
  model.rf <- train(y=as.factor(train$classe), x=train[,!"classe",with=F], tuneGrid=data.frame(mtry=3), trControl=trainControl(method="none"), method="parRF")  
  generate_predictions(predict(model.rf, newdata=drop_cols(validation[,eval(names(which(na_cols == F))[-60]),with=F])))
}