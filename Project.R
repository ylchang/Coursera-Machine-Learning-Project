##setwd("/Users/yu-lichang/Desktop/coursera/Machine_Learning/Project")
trainingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(trainingUrl, destfile = "training.csv")
download.file(testingUrl, destfile = "testing.csv")
dateDownloaded <- date()

training <- read.table("training.csv", sep = ",", header = TRUE)
testing <- read.table("testing.csv", sep = ",", header = TRUE)

dim(training)

## Keep only the numeric lists in the data frame. Remove columns with 
## 90% or more entries are NA's

NAthres <- 90

nums <- sapply(training, is.numeric)
trainNum <- training[,nums]
countNA <- apply(trainNum, 2, function(x) {length(which(is.na(x)))/length(x)*100})
trainNum <- trainNum[, countNA < NAthres]
trainNum <- subset(trainNum, select = -c(raw_timestamp_part_1, raw_timestamp_part_2, num_window))
trainNum <- cbind(trainNum[,2:ncol(trainNum)],training$classe)
##trainNum <- cbind(training$user_name,trainNum[,2:ncol(trainNum)],training$classe)
##colnames(trainNum)[1] <- "user_name"
colnames(trainNum)[ncol(trainNum)] <- "classe"

nums <- sapply(testing, is.numeric)
testNum <- testing[,nums]
countNA <- apply(testNum, 2, function(x) {length(which(is.na(x)))/length(x)*100})
testNum <- testNum[, countNA < NAthres]
testNum <- subset(testNum, select = -c(raw_timestamp_part_1, raw_timestamp_part_2, num_window))
##testNum <- cbind(testNum[,2:ncol(trainNum)])
##testNum <- cbind(testing$user_name,testNum[,2:ncol(trainNum)])
##colnames(testNum)[1] <- "user_name"

## Partition training set and testing test for the trainNum data
set.seed(998)
inTrain <- createDataPartition(y=trainNum$classe, p = 0.6, list = FALSE)
trainTrain <- trainNum[inTrain,]
testTrain <- trainNum[-inTrain,]

## Use PCA to reduce number of variables but still retain the information
M <- abs(cor(trainTrain[,-ncol(trainTrain)]))
diag(M)<- 0.0
which(M>0.8, arr.ind = T)

## Fit a model with rpart method. Variable X is excluded, knowing that
## variable Classe is sorted according to X.

preProc <- preProcess(trainTrain[,-ncol(trainTrain)],method = "pca", pcaComp = 10)
trainPC <- predict(preProc,trainTrain[,-ncol(trainTrain)])

modFit <- train(trainTrain$classe ~ ., data = trainPC, method = "rf")
modFit$times

## Cross Validation by using the Testing set testTrain
testPC <- predict(preProc, testTrain[,-ncol(testTrain)])
confusionMatrix(testTrain$classe,predict(modFit,testPC))

## To improve the accuracy, directly use RF to fit the model for 
## the training set trainTrain
modFitRF <- train(trainTrain$classe ~., data = TrainTrain, method = "rf")
modFitRF$times
confusionMatrix(testTrain$classe,predict(modFitRF,testTrain))

## Use 3-fold CV and repeat 3 times to fit a model
fitControl <- trainControl(## 3-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated 3 times
  repeats = 6)

set.seed(825)

gbmFit1 <- train(classe ~ ., data = trainTrain,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

confusionMatrix(trainTrain$classe,predict(gbmFit1,trainTrain))
confusionMatrix(testTrain$classe,predict(gbmFit1,testTrain))

trellis.par.set(caretTheme())
plot(gbmFit1)

resamps <- resamples(list(RF = modFitRF,
                          CVRF = modFit))


