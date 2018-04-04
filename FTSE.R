FTSE <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\Weekly_Indicator_Spline.csv")
FTSE$Date <- as.Date(FTSE$Date, format="%m/%d/%Y")

head(FTSE$FTSE,5)
summary(FTSE$FTSE)

library(ggplot2)
library(scales)

ggplot(data = FTSE,aes(x = Date)) +
  geom_line(aes(y = FTSE),colour = "dark green") +
  labs(title="FTSE China 50 Index Time Series Plot") +
  labs(x="Date", y="Adj. Close Price")


FTSE.ts <- ts(FTSE$FTSE,start = c(2013,2,8),frequency = 52)
plot(decompose(FTSE.ts))

dec <- decompose(FTSE.ts)

seasonal <- dec$seasonal
seasonal <- as.numeric(seasonal)
write.csv(data.frame(rescale(seasonal)),"C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\ETFs\\weights.csv")
