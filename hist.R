mydata <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\Weekly_Indicator_Spline.csv")

library(ggplot2)
library(scales)
hist(mydata$Survey.OECD)


# Add a Normal Curve (Thanks to Peter Dalgaard)
x <- mydata$ETF.Volatility 
h<-hist(x, breaks=20, col="light green", xlab="ETF Volatility Index", 
        main="Histogram with Normal Curve",cex.main=1) 
xfit<-seq(min(x),max(x),length=50) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=1)

# Add a Normal Curve (Thanks to Peter Dalgaard)
x <- log(mydata$ETF.Volatility) 
h<-hist(x, breaks=20, col="light green", xlab="ETF Volatility Index", 
        main="Log-Transformation with Normal Curve",cex.main=1) 
xfit<-seq(min(x),max(x),length=50) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=1)

#logarithm
