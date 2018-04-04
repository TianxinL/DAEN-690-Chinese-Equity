# Load Libraries
library(scales)
library(ggplot2)
library(lubridate)
library(MASS)
library(randomForest)
library(gbm)
library(e1071)
library(hydroGOF)

# Load Data
Duplicate <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\Weekly_Indicator_Duplicate3.csv")
Duplicate$Date <- as.Date(Duplicate$Date,format = "%m/%d/%Y")

# Descriptive Statistics
summary(Duplicate)

# Time Series Visualizations
ggplot(data = Duplicate,aes(x = Date)) +
  geom_line(aes(y = Survey.OECD),colour = "dark green") +
  labs(title="Consumer Survey of OECD Time Series Plot") +
  labs(x="Date", y="Index")

ggplot(data = Duplicate,aes(x = Date)) +
  geom_line(aes(y = Export),colour = "dark green") +
  geom_line(aes(y = Import),colour = "dark red") +
  labs(title="Export and Import Time Series Plot") +
  labs(x="Date", y="Amount")

ggplot(data = Duplicate,aes(x = Date)) +
  geom_line(aes(y = PMI.Manu),colour = "dark green") +
  geom_line(aes(y = PMI.NonManu),colour = "dark red") +
  labs(title="Manufacture and Non Manufacture PMI Time Series Plot") +
  labs(x="Date", y="Index")

ggplot(data = Duplicate,aes(x = Date)) +
  geom_line(aes(y = M0),colour = "dark green") +
  labs(title="Fund Flow of China Time Series Plot") +
  labs(x="Date", y="Amount")

ggplot(data = Duplicate,aes(x = Date)) +
  geom_line(aes(y = Policy.Uncertainty),colour = "dark green") +
  labs(title="Policy Uncertainty Index Time Series Plot") +
  labs(x="Date", y="Index")

ggplot(data = Duplicate,aes(x = Date)) +
  geom_line(aes(y = rescale(Building.Sale)),colour = "dark green") +
  geom_line(aes(y = rescale(Building.Sale.Rate)),colour = "dark red") +
  labs(title="Total Sale of Building Sold Time Series Plot") +
  labs(x="Date", y="Index")

ggplot(data = Duplicate,aes(x = Date)) +
  geom_line(aes(y = CPI),colour = "dark green") +
  geom_line(aes(y = CPI.Residence),colour = "dark red") +
  geom_line(aes(y = CPI.Household),colour = "orange") +
  geom_line(aes(y = CPI.Transportation),colour = "purple") +
  geom_line(aes(y = CPI.Education),colour = "pink") +
  labs(title="CPI related Indicators Time Series Plot") +
  labs(x="Date", y="Index")

# Create Week number
Duplicate$Week.number <- as.factor(as.character(lubridate::isoweek(ymd(Duplicate$Date))))

# Dataset Split
n <- length(Duplicate[,1])
Duplicate.Training <- Duplicate[1:(n-1),]
Duplicate.Test <- Duplicate[n,]

# linear regression
fit.lm <- lm(FTSE ~. -Date -Direction, data=Duplicate.Training)
summary(fit.lm)
# stepwise selection
fit.step <- stepAIC(fit.lm, direction="backward")
summary(fit.step)
predict.1 <- predict(fit.step, Duplicate.Test)

# random forest
fit.rf <- randomForest(FTSE ~. -Date -Direction, data=Duplicate.Training)
predict.2 <- predict(fit.rf, Duplicate.Test)

# boosting
fit.boosting <- gbm(FTSE ~. -Date -Direction, data=Duplicate.Training)
predict.3 <- predict(fit.boosting, Duplicate.Test, n.trees = 100)

# svm
fit.svm <- svm(FTSE ~. -Date -Direction, data=Duplicate.Training)
predict.4 <- predict(fit.svm, Duplicate.Test)

# Compare prediction and obs
plot(Duplicate.Test$Date,Duplicate.Test$FTSE,type = "o", pch=19)
points(Duplicate.Test$Date,predict.1, pch=19, col='blue')
points(Duplicate.Test$Date,predict.2, pch=19, col='red')
points(Duplicate.Test$Date,predict.3, pch=19, col='purple')
points(Duplicate.Test$Date,predict.4, pch=19, col='orange')

# RMSE for evaluation
#Calculate RMSE 
rmse(predict.1, Duplicate.Test$FTSE)
rmse(predict.2, Duplicate.Test$FTSE)
rmse(predict.3, Duplicate.Test$FTSE)
rmse(predict.4, Duplicate.Test$FTSE)