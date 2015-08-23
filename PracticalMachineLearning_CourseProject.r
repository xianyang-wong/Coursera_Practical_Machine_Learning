trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

pml_train <- read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings=c("NA","#DIV/0!",""))
pml_test <- read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

setwd("C:/Users/Xianyang/Desktop/Coursera/Practical Machine Learning")

pml_train <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
pml_test <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

dim(pml_train)
dim(pml_test)

head(pml_train)
head(pml_test)

#remove NA columns in datasets
pml_train <- pml_train[, colSums(is.na(pml_train)) == 0] 
pml_test <- pml_test[, colSums(is.na(pml_test)) == 0] 

#Exploratory Data Analysis reveals that the first 7 fields of the data are dimensional, and may not be pertinent to the prediction model.
pml_train <- pml_train[-c(1:7)]
pml_test <- pml_test[-c(1:7)]

#check if both train and test data sets have the same column names
all.equal(colnames(pml_train[1:length(pml_train)-1]),colnames(pml_test[1:length(pml_test)-1]))

str(pml_train)
str(pml_test)

for(i in c(1:(ncol(pml_train)-1))) {
  pml_train[,i] = as.numeric(as.character(pml_train[,i]))
  pml_test[,i] = as.numeric(as.character(pml_test[,i]))
}





library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

set.seed(901206) 
inTrain <- createDataPartition(pml_train$classe, p=0.70, list=F)
data_train <- pml_train[inTrain, ]
data_cv <- pml_train[-inTrain, ]

control <- trainControl(method="cv", 5)
#rpart.grid <- expand.grid(cp=seq(0,1,by=0.01))
#modelTree2 <- train(classe ~ ., data=data_train, method="rpart", trControl=control, tuneGrid=rpart.grid)

modelTree <- train(classe ~ ., data=data_train, method="rpart", trControl=control)
modelTree

ggplot(modelTree)

predictmodelTree <- predict(modelTree, data_cv)
confusionMatrix(predictmodelTree, data_cv$classe)

modelTree_accuracy <- postResample(predictmodelTree, data_cv$classe)
modelTree_accuracy 
modelTree_oose <- 1 - as.numeric(confusionMatrix(data_cv$classe, predictmodelTree)$overall[1])
modelTree_oose


modelForest <- train(classe ~ ., data=data_train, method="rf", trControl=control)
modelForest

ggplot(modelForest)

predictmodelForest <- predict(modelForest, data_cv)
confusionMatrix(predictmodelForest, data_cv$classe)

modelForest_accuracy <- postResample(predictmodelForest, data_cv$classe)
modelForest_accuracy 
modelForest_oose <- 1 - as.numeric(confusionMatrix(data_cv$classe, predictmodelForest)$overall[1])
modelForest_oose


result <- predict(modelForest, pml_test[, -length(names(pml_test))])
result

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(result)
