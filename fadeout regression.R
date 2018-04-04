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
FadeOut <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\Weekly_Indicator_FadeOut3.csv")
FadeOut$Date <- as.Date(FadeOut$Date,format = "%m/%d/%Y")

# Descriptive Statistics
summary(FadeOut)

# Time Series Visualizations
ggplot(data = FadeOut,aes(x = Date)) +
  geom_line(aes(y = Survey.OECD),colour = "dark green") +
  labs(title="Consumer Survey of OECD Time Series Plot") +
  labs(x="Date", y="Index")

ggplot(data = FadeOut,aes(x = Date)) +
  geom_line(aes(y = Export),colour = "dark green") +
  geom_line(aes(y = Import),colour = "dark red") +
  labs(title="Export and Import Time Series Plot") +
  labs(x="Date", y="Amount")

ggplot(data = FadeOut,aes(x = Date)) +
  geom_line(aes(y = rescale(SSE.10D.MA)),colour = "dark green") +
  geom_line(aes(y = rescale(SSECBI.10D.MA)),colour = "dark red") +
  labs(title="Manufacture and Non Manufacture PMI Time Series Plot") +
  labs(x="Date", y="Index")

# Create Week number
FadeOut$Week.number <- as.factor(as.character(lubridate::isoweek(ymd(FadeOut$Date))))

# Dataset Split
n <- length(FadeOut[,1])
FadeOut.Training <- FadeOut[1:(n-1),]
FadeOut.Test <- FadeOut[n,]

# linear regression
fit.lm <- lm(FTSE ~. -Date -Direction, data=FadeOut.Training)
summary(fit.lm)
# stepwise selection
fit.step <- stepAIC(fit.lm, direction="backward")
summary(fit.step)
predict.1 <- predict(fit.step, FadeOut.Test)

# random forest
fit.rf <- randomForest(FTSE ~. -Date -Direction, data=FadeOut.Training)
predict.2 <- predict(fit.rf, FadeOut.Test)

# boosting
fit.boosting <- gbm(FTSE ~. -Date -Direction, data=FadeOut.Training)
predict.3 <- predict(fit.boosting, FadeOut.Test, n.trees = 100)

# svm
fit.svm <- svm(FTSE ~. -Date -Direction, data=FadeOut.Training)
predict.4 <- predict(fit.svm, FadeOut.Test)

# Compare prediction and obs
plot(FadeOut.Test$Date,FadeOut.Test$FTSE,type = "o", pch=19)
points(FadeOut.Test$Date,predict.1, pch=19, col='blue')
points(FadeOut.Test$Date,predict.2, pch=19, col='red')
points(FadeOut.Test$Date,predict.3, pch=19, col='purple')
points(FadeOut.Test$Date,predict.4, pch=19, col='orange')

# RMSE for evaluation
#Calculate RMSE 
rmse(predict.1, FadeOut.Test$FTSE)
rmse(predict.2, FadeOut.Test$FTSE)
rmse(predict.3, FadeOut.Test$FTSE)
rmse(predict.4, FadeOut.Test$FTSE)
