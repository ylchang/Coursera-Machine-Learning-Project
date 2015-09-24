setwd("C:/Users/13360/Desktop/coursera/Machine_Learning/Project")
trainingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(trainingUrl, destfile = "training.csv")
download.file(testingUrl, destfile = "testing.csv")
dateDownloaded <- date()

training <- read.table("pml-training.csv", sep = ",", header = TRUE)
testing <- read.table("pml-testing.csv", sep = ",", header = TRUE)

dim(training)

## Keep only the numeric lists in the data frame. Remove columns with 
## 90% or more entries are NA's

NAthres <- 90

nums <- sapply(training, is.numeric)
trainNum <- training[,nums]
countNA <- apply(trainNum, 2, function(x) {length(which(is.na(x)))/length(x)*100})
trainNum <- trainNum[, countNA < NAthres]
trainNum <- cbind(trainNum,training[,160])
colnames(trainNum)[ncol(trainNum)] <- "Classe"

nums <- sapply(testing, is.numeric)
testNum <- testing[,nums]
countNA <- apply(testNum, 2, function(x) {length(which(is.na(x)))/length(x)*100})
testNum <- testNum[, countNA < NAthres]
testNum <- cbind(testNum,testing[,160])
colnames(testNum)[ncol(testNum)] <- "Classe"

## Too many predictors (160). Preprocess the data with 
## Principal Components Analysis to reduct the number of predictors.

preProc <- preProcess(trainNum[,-ncol(trainNum)],method = "pca",pcaComp = 10)
trainPC <- predict(preProc,trainNum[,-ncol(trainNum)])

## ##create 10 folds cross validation
##folds <- createFolds(y = trainNum$classe,k = 10, list = TRUE, returnTrain = FALSE)
##sapply(folds,length)Based on the PC, fit a model and find the confusion matrix

modelFit <- train(trainNum$classe ~., method = "glm", data = trainPC)


