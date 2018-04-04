# Cubic Interpolation
# Load DAta
mydata <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\QUARTERLY_WEEKLY_INDICATORS.csv")

# Load Library
library(zoo)

# Data Subset
mydata1 <- mydata[,1:3]
mydata2 <- mydata[,4:6]

# Quarter in year-month-day format
mydata1$qvar = as.Date(mydata$Lag.Quarter1,format = "%m/%d/%Y")
mydata2$qvar = as.Date(mydata$Lag.Quarter2,format = "%m/%d/%Y")

# Creating a daily sequence for the quarterly range
weekly1 = seq(mydata1$qvar[1], tail(mydata1$qvar,1), by="week")

# US GDP
gdp.US = mydata1[c("qvar","GDP.US")]

# Cubic interpolation using spline()
gdp2.US = data.frame(qvar=weekly1, gdp2=spline(gdp.US, method="fmm", xout=weekly1)$y)

# Creating a daily sequence for the quarterly range
weekly2 = seq(mydata2$qvar[1], tail(mydata2$qvar,1), by="week")

# China GDP
gdp.Ch = mydata2[c("qvar","GDP.China")]

# Cubic interpolation using spline()
gdp2.Ch = data.frame(qvar=weekly2, gdp2=spline(gdp.Ch, method="fmm", xout=weekly2)$y)

# GDP Combination
GDP.Weekly <- data.frame(gdp2.US$qvar[-1],gdp2.US$gdp2[-1],gdp2.Ch$gdp2[-275])

# column rename
colnames(GDP.Weekly) <- c("Date","GDP.US","GDP.China")

# correlation check
cor(GDP.Weekly[,-1])

plot(gdp.Ch$qvar[1:10],gdp.Ch$GDP.China[1:10],type="o",pch=19,xlab="Date",ylab="GDP.China")
lines(gdp2.Ch$qvar,gdp2.Ch$gdp2,col="red")
