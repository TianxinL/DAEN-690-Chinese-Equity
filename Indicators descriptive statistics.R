# load data
Daily.Indicator <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Indicator China\\DAILY_INDICATORS.csv")
Daily.Indicator$Date <- as.Date(Daily.Indicator$Date, format="%m/%d/%Y")
str(Daily.Indicator)

head(Daily.Indicator,10)
summary(Daily.Indicator[,2:8])

library(ggplot2)
library(scales)

ggplot(data = Daily.Indicator,aes(x = Date)) +
  geom_line(aes(y = rescale(Daily.Indicator[,2])),colour = "dark green") + 
  geom_line(aes(y = rescale(Daily.Indicator[,3])),colour = "dark blue") +
  geom_line(aes(y = rescale(Daily.Indicator[,8])),colour = "orange") +
  labs(title="China Daily Indicators (Rescaled) Time Series Plot") +
  labs(x="Date", y="Index (Rescaled)")

table(Daily.Indicator[,4:6])

# load data
Monthly.Indicator <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Indicator China\\MONTHLY_INDICATORS.csv")
Monthly.Indicator$OBS_DATE <- as.Date(Monthly.Indicator$OBS_DATE, format="%m/%d/%Y")
Monthly.Indicator <- Monthly.Indicator[,-59]
str(Monthly.Indicator)

head(Monthly.Indicator,5)
summary(Monthly.Indicator[,-1])

library(scales)

Manufact.Indicator <- Monthly.Indicator[,c(1,20:33)]
NonManufact.Indicator <- Monthly.Indicator[,c(1,34:43)]
CPI.Indicator <- Monthly.Indicator[,c(1,44:52)]
Sales.Indicator <- data.frame(Monthly.Indicator$OBS_DATE,rescale(Monthly.Indicator$TSALE_AMOUNT),
                             rescale(Monthly.Indicator$TSALE_RATE),rescale(Monthly.Indicator$TSC_COM_ACCU_YUA),
                             rescale(Monthly.Indicator$TSCRB_COM_ACCU_RAT),rescale(Monthly.Indicator$TSC_FOR_ACCU_YUA),
                             rescale(Monthly.Indicator$TSCRB_FOR_ACCU_RAT))
M.Indicator <- data.frame(Monthly.Indicator$OBS_DATE,Monthly.Indicator$M0,
                          Monthly.Indicator$M1,Monthly.Indicator$M2,Monthly.Indicator$M3)
ConsumerOpinion.Indicator <- data.frame(Monthly.Indicator$OBS_DATE,Monthly.Indicator$COS_EC,
                                  Monthly.Indicator$COS_OECD)

par(mfrow=c(2,4))
plot(Monthly.Indicator$OBS_DATE,Monthly.Indicator$CHIEPUINDXM,type = "l",main = "Economic Policy Uncertainty Index", xlab = "Date", ylab="")
matplot(Manufact.Indicator[,-1],type="l",lty = 1, main = "Manufact indicators",xlab = "",ylab = "")
matplot(NonManufact.Indicator[,-1],type="l",lty = 1, main = "Non-Manufact indicators",xlab = "",ylab = "")
matplot(CPI.Indicator[,-1],type="l",lty = 1, main = "CPI related indicators",xlab = "",ylab = "")
matplot(Sales.Indicator[,-1],type="l",lty = 1, main = "Sales related indicators",xlab = "",ylab = "")
matplot(M.Indicator[,-1],type="l",lty = 1, main = "M indicators",xlab = "",ylab = "")
matplot(ConsumerOpinion.Indicator[,-1],type="l",lty = 1, main = "Consumer Opition indicators",xlab = "",ylab = "")
matplot(Monthly.Indicator$INT_RATES,type="l",lty = 1, main = "Interest Rate",xlab = "",ylab = "")

# load data
Quarterly.Indicator <- read.csv("C:\\Users\\Tianxin.Liu\\Downloads\\QUARTERLY_INDICATORS.csv")
Quarterly.Indicator$OBS_DATE <- as.Date(Quarterly.Indicator$OBS_DATE, format="%m/%d/%Y")
Quarterly.Indicator <- Quarterly.Indicator[,-2]
str(Quarterly.Indicator)

head(Quarterly.Indicator,5)
summary(Quarterly.Indicator[,-1])

ggplot(data = Quarterly.Indicator,aes(x = OBS_DATE)) +
  geom_line(aes(y = rescale(Quarterly.Indicator[,2])),colour = "orange") + 
  geom_line(aes(y = rescale(Quarterly.Indicator[,3])),colour = "red") +
  geom_line(aes(y = rescale(Quarterly.Indicator[,4])),colour = "dark green") +
  geom_line(aes(y = rescale(Quarterly.Indicator[,5])),colour = "blue") + 
  geom_line(aes(y = rescale(Quarterly.Indicator[,6])),colour = "dark green") +
  geom_line(aes(y = rescale(Quarterly.Indicator[,7])),colour = "dark blue") +
  geom_line(aes(y = rescale(Quarterly.Indicator[,8])),colour = "dark green") + 
  geom_line(aes(y = rescale(Quarterly.Indicator[,9])),colour = "light blue") +
  geom_line(aes(y = rescale(Quarterly.Indicator[,10])),colour = "dark green") +
  geom_line(aes(y = rescale(Quarterly.Indicator[,11])),colour = "purple") +
  labs(title="China Quarterly Indicators (Rescaled) Time Series Plot") +
  labs(x="Date", y="Index (Rescaled)")

CPI <- read.csv("C:\\Users\\Tianxin.Liu\\Downloads\\CPI monthly data.csv")
matplot(CPI[,-c(1,2)], type="l",lty = 1, main = "CPI related Indicators",xlab = "",ylab = "")

library(corrplot)
CPI.cor <- cor(CPI[,-c(1,2)])
corrplot(CPI.cor,method="number")

matplot(Manufact.Indicator[-61,-c(1,10)],type="l",lty = 1, main = "Manufact indicators",xlab = "",ylab = "")
Manufact.cor <- cor(Manufact.Indicator[-61,-c(1,10)])
corrplot(Manufact.cor,method="number")

