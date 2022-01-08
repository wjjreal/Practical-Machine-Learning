---
title: "Predict the manner"
author: "Jingjing Wang"
date: "2021/12/31"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Predict the manner

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 

The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 

The 1st step is the pull and cleanup the data.
```{r message=FALSE}
training <- read.csv("pml-training.csv",na.strings=c("NA","#DIV/0!", ""),stringsAsFactors=TRUE)
testing <- read.csv("pml-testing.csv",na.strings=c("NA","#DIV/0!", ""),stringsAsFactors=TRUE)
dim(training)
dim(testing)
training <- training[ , colSums(is.na(training)) == 0]
testing <- testing[ , colSums(is.na(testing)) == 0]
dim(training)
dim(testing)
library(tidyverse)
df_list <- list(training, testing)
cols <- reduce(df_list, .f = ~ intersect(colnames(.x), colnames(.y)))
training <- cbind(training[,cols],training[,"classe"])
colnames(training)[60]<-"classe"
testing <- cbind(testing[,cols],testing[,"problem_id"])
colnames(testing)[60]<-"problem_id"
training <- training[-c(1:7)]
testing <- testing[-c(1:7)]
```

For cross validation purpose, from the original training data, randomly select 70% for model training and select 30% for testing. The models will be verified with testing data, and select the most accurate one for prediction.

```{r message=FALSE}
set.seed(1000)
library(caret)
inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
trainds <- training[inTrain,]
testds <- training[-inTrain,]
```

Using different models to train data, and cross validation to verify the accuracy and out of sample error.

Decision Tree:
```{r message=FALSE}
library(rattle)
mod1 <- train(classe ~ ., method="rpart",data=trainds)
fancyRpartPlot(mod1$finalModel)
pred1 <- predict(mod1,newdata=testds)
confusionMatrix(pred1, testds$classe)
```
The accuracy is 50.4% and out of sample error is 49.6%, try the next one.

Random Forest:
```{r message=FALSE}
library(parallel)
library(doParallel)
cluster <- makeCluster(5)
registerDoParallel(cluster)
fitControl <- trainControl(method = "cv",number = 5,allowParallel = TRUE)
system.time(mod2 <- train(classe ~ ., method="rf",data=trainds,prox=T,trControl = fitControl))
stopCluster(cluster)
registerDoSEQ()
pred2 <- predict(mod2,newdata=testds)
confusionMatrix(pred2, testds$classe)
```
The accuracy is 99.25% and out of sample error is 0.75%. The result is very good. Try the next one to see if any chance better.

Linear Discriminant Analysis:
```{r message=FALSE}
mod3 <- train(classe ~ ., method="lda",data=trainds)
pred3 <- predict(mod3,newdata=testds)
confusionMatrix(pred3, testds$classe)
```
The accuracy is 70.98% and out of sample error is 29.02%, not good as random forest.


From the test result, random tree is the best model.
The predict of testing data is as below.
```{r message=FALSE}
pred4 <- predict(mod2,newdata=testing)
pred4
```

