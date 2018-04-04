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
WeekScale <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\Weekly_Indicator_WeekScale3.csv")
WeekScale$Date <- as.Date(WeekScale$Date,format = "%m/%d/%Y")

# Descriptive Statistics
summary(WeekScale)

# Time Series Visualizations
ggplot(data = WeekScale,aes(x = Date)) +
  geom_line(aes(y = Total.Share.Price),colour = "dark green") +
  labs(title="Consumer Survey of OECD Time Series Plot") +
  labs(x="Date", y="Index")

ggplot(data = WeekScale,aes(x = Date)) +
  geom_line(aes(y = rescale(SSE.10D.MA)),colour = "dark green") +
  geom_line(aes(y = rescale(SSECBI.10D.MA)),colour = "dark red") +
  labs(title="Manufacture and Non Manufacture PMI Time Series Plot") +
  labs(x="Date", y="Index")

# Create Week number
WeekScale$Week.number <- as.factor(as.character(lubridate::isoweek(ymd(WeekScale$Date))))

# Dataset Split
n <- length(WeekScale[,1])
WeekScale.Training <- WeekScale[1:(n-1),]
WeekScale.Test <- WeekScale[n,]

# linear regression
fit.lm <- lm(FTSE ~. -Date -Direction, data=WeekScale.Training)
summary(fit.lm)
# stepwise selection
fit.step <- stepAIC(fit.lm, direction="backward")
summary(fit.step)
predict.1 <- predict(fit.step, WeekScale.Test)

# random forest
fit.rf <- randomForest(FTSE ~. -Date -Direction, data=WeekScale.Training)
predict.2 <- predict(fit.rf, WeekScale.Test)

# boosting
fit.boosting <- gbm(FTSE ~. -Date -Direction, data=WeekScale.Training)
predict.3 <- predict(fit.boosting, WeekScale.Test, n.trees = 100)

# svm
fit.svm <- svm(FTSE ~. -Date -Direction, data=WeekScale.Training)
predict.4 <- predict(fit.svm, WeekScale.Test)

# Compare prediction and obs
plot(WeekScale.Test$Date,WeekScale.Test$FTSE,type = "o", pch=19)
points(WeekScale.Test$Date,predict.1, pch=19, col='blue')
points(WeekScale.Test$Date,predict.2, pch=19, col='red')
points(WeekScale.Test$Date,predict.3, pch=19, col='purple')
points(WeekScale.Test$Date,predict.4, pch=19, col='orange')

# RMSE for evaluation
#Calculate RMSE 
rmse(predict.1, WeekScale.Test$FTSE)
rmse(predict.2, WeekScale.Test$FTSE)
rmse(predict.3, WeekScale.Test$FTSE)
rmse(predict.4, WeekScale.Test$FTSE)
