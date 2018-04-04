# Load Libraries
library(scales)

# Load Data
Duplicate <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\Weekly_Indicator_Duplicate3.csv")
Duplicate$Date <- as.Date(Duplicate$Date,format = "%m/%d/%Y")

# Descriptive Statistics
summary(Duplicate)

# Time Series Visualizations


n <- length(Weekly.Indicator[,1])
Weekly.Training <- Weekly.Indicator[1:(0.8*n),]
Weekly.Test <- Weekly.Indicator[(0.8*n+1):(n+1),]

# linear regression
fit.lm <- lm(FTSE ~. -Date -Return, data=Weekly.Training)
summary(fit.lm)
predict.1 <- predict(fit.lm, Weekly.Test)

# random forest
library(randomForest)
fit.rf <- randomForest(FTSE ~. -Date -Return, data=Weekly.Training)
predict.2 <- predict(fit.rf, Weekly.Test)

# boosting
library(gbm)
fit.boosting <- gbm(FTSE ~. -Date -Return, data=Weekly.Training)
predict.3 <- predict(fit.boosting, Weekly.Test,n.trees = 100)

# svm
library(e1071)
fit.svm <- svm(FTSE ~. -Date -Return, data=Weekly.Training)
predict.4 <- predict(fit.svm, Weekly.Test)

# nnet
library(nnet)
fit.nnet <- nnet(FTSE/20000 ~. -Date -Return, data=Weekly.Training,size=5)
predict.5 <- predict(fit.nnet, Weekly.Test)*20000

plot(Weekly.Test$Date,Weekly.Test$FTSE,type = "l")
lines(Weekly.Test$Date,predict.1,col='blue')
lines(Weekly.Test$Date,predict.2,col='red')
lines(Weekly.Test$Date,predict.3,col='purple')
lines(Weekly.Test$Date,predict.4,col='orange')
lines(Weekly.Test$Date,predict.5,col='light blue')

## RMSE Calculation for linear model

library(hydroGOF)

#Calculate RMSE 
rmse(predict.1,Weekly.Test$FTSE)
rmse(predict.2,Weekly.Test$FTSE)
rmse(predict.3,Weekly.Test$FTSE)
rmse(predict.4,Weekly.Test$FTSE)
rmse(as.numeric(predict.5),Weekly.Test$FTSE)

#===========================================================#

Weekly.Indicator <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\Weekly_Indicator_Duplicate.csv")
Weekly.Indicator$Date <- as.Date(Weekly.Indicator$Date,format = "%m/%d/%Y")

n <- length(Weekly.Indicator[,1])
Weekly.Training <- Weekly.Indicator[1:(0.8*n),]
Weekly.Test <- Weekly.Indicator[(0.8*n+1):(n+1),]

# linear regression
fit.lm <- lm(FTSE ~. -Date -Return, data=Weekly.Training)
summary(fit.lm)
predict.1 <- predict(fit.lm, Weekly.Test)

# random forest
library(randomForest)
fit.rf <- randomForest(FTSE ~. -Date -Return, data=Weekly.Training)
predict.2 <- predict(fit.rf, Weekly.Test)

# boosting
library(gbm)
fit.boosting <- gbm(FTSE ~. -Date -Return, data=Weekly.Training)
predict.3 <- predict(fit.boosting, Weekly.Test,n.trees = 100)

# svm
library(e1071)
fit.svm <- svm(FTSE ~. -Date -Return, data=Weekly.Training)
predict.4 <- predict(fit.svm, Weekly.Test)

# nnet
library(nnet)
fit.nnet <- nnet(FTSE/20000 ~. -Date -Return, data=Weekly.Training,size=5)
predict.5 <- predict(fit.nnet, Weekly.Test)*20000

plot(Weekly.Test$Date,Weekly.Test$FTSE,type = "l")
lines(Weekly.Test$Date,predict.1,col='blue')
lines(Weekly.Test$Date,predict.2,col='red')
lines(Weekly.Test$Date,predict.3,col='purple')
lines(Weekly.Test$Date,predict.4,col='orange')
lines(Weekly.Test$Date,predict.5,col='light blue')

## RMSE Calculation for linear model

library(hydroGOF)

#Calculate RMSE 
rmse(predict.1,Weekly.Test$FTSE)
rmse(predict.2,Weekly.Test$FTSE)
rmse(predict.3,Weekly.Test$FTSE)
rmse(predict.4,Weekly.Test$FTSE)
rmse(as.numeric(predict.5),Weekly.Test$FTSE)

#==========================================================#

Weekly.Indicator <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\Weekly_Indicator_WeekScale.csv")
Weekly.Indicator$Date <- as.Date(Weekly.Indicator$Date,format = "%m/%d/%Y")

n <- length(Weekly.Indicator[,1])
Weekly.Training <- Weekly.Indicator[1:(0.8*n),]
Weekly.Test <- Weekly.Indicator[(0.8*n+1):(n+1),]

# linear regression
fit.lm <- lm(FTSE ~. -Date -Return, data=Weekly.Training)
summary(fit.lm)
predict.1 <- predict(fit.lm, Weekly.Test)

# random forest
library(randomForest)
fit.rf <- randomForest(FTSE ~. -Date -Return, data=Weekly.Training)
predict.2 <- predict(fit.rf, Weekly.Test)

# boosting
library(gbm)
fit.boosting <- gbm(FTSE ~. -Date -Return, data=Weekly.Training)
predict.3 <- predict(fit.boosting, Weekly.Test,n.trees = 100)

# svm
library(e1071)
fit.svm <- svm(FTSE ~. -Date -Return, data=Weekly.Training)
predict.4 <- predict(fit.svm, Weekly.Test)

# nnet
library(nnet)
fit.nnet <- nnet(FTSE/20000 ~. -Date -Return, data=Weekly.Training,size=5)
predict.5 <- predict(fit.nnet, Weekly.Test)*20000

plot(Weekly.Test$Date,Weekly.Test$FTSE,type = "l")
lines(Weekly.Test$Date,predict.1,col='blue')
lines(Weekly.Test$Date,predict.2,col='red')
lines(Weekly.Test$Date,predict.3,col='purple')
lines(Weekly.Test$Date,predict.4,col='orange')
lines(Weekly.Test$Date,predict.5,col='light blue')

## RMSE Calculation for linear model

library(hydroGOF)

#Calculate RMSE 
rmse(predict.1,Weekly.Test$FTSE)
rmse(predict.2,Weekly.Test$FTSE)
rmse(predict.3,Weekly.Test$FTSE)
rmse(predict.4,Weekly.Test$FTSE)
rmse(as.numeric(predict.5),Weekly.Test$FTSE)