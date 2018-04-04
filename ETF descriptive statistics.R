# load file
ETF <- read.csv("C:\\Users\\Tianxin.Liu\\Downloads\\ETFs.csv", stringsAsFactors = FALSE)
ETF$Date <- as.Date(ETF$Date, format = "%m/%d/%Y")

str(ETF)
head(ETF,5)

library(ggplot2)

ggplot(data = ETF,aes(x = Date)) +
  geom_line(aes(y = FXI),colour = "dark green") + 
  geom_line(aes(y = YINN),colour = "dark blue") +
  geom_line(aes(y = XPP),colour = "dark gray") +
  geom_line(aes(y = YANG),colour = "dark red") +
  geom_line(aes(y = FXP),colour = "yellow") +
  geom_line(aes(y = YXI),colour = "purple") +
  labs(title="ETFs Time Series Plot") +
  labs(x="Date", y="Adj. Close Price")

# Descriptive Statistics
# 252 Trading days per year
length(ETF[,1])/5

# Create Return and Return Rate

n <- length(ETF[,1])

Return <- data.frame(ETF$Date[-n], -diff(ETF$FXI), -diff(ETF$YINN),-diff(ETF$XPP) ,
                     -diff(ETF$YANG), -diff(ETF$FXP), -diff(ETF$YXI))
colnames(Return) <- c("Date","FXI", "YINN","XPP","YANG","FXP","YXI")


FXI <- rep(0,(n-1))
Return.Rate <- data.frame(Return$Date,FXI)
for(i in 1:(n-1)){
  Return.Rate$FXI[i] = round((ETF$FXI[i] - ETF$FXI[i+1])/ETF$FXI[i+1],2)
}
for(i in 1:(n-1)){
  Return.Rate$YINN[i] = round((ETF$YINN[i] - ETF$YINN[i+1])/ETF$YINN[i+1],2)
}
for(i in 1:(n-1)){
  Return.Rate$XPP[i] = round((ETF$XPP[i] - ETF$XPP[i+1])/ETF$XPP[i+1],2)
}
for(i in 1:(n-1)){
  Return.Rate$YANG[i] = round((ETF$YANG[i] - ETF$YANG[i+1])/ETF$YANG[i+1],2)
}
for(i in 1:(n-1)){
  Return.Rate$FXP[i] = round((ETF$FXP[i] - ETF$FXP[i+1])/ETF$FXP[i+1],2)
}
for(i in 1:(n-1)){
  Return.Rate$YXI[i] = round((ETF$YXI[i] - ETF$YXI[i+1])/ETF$YXI[i+1],2)
}

# histogram for return
par(mfrow=c(2,3))
x.1 <- Return[,2]
h.1 <- hist(x.1, breaks=15, col="light blue", xlab="Daily Return", main="FXI") 
xfit.1<-seq(min(x.1),max(x.1),length=40) 
yfit.1<-dnorm(xfit.1,mean=mean(x.1),sd=sd(x.1)) 
yfit.1 <- yfit.1*diff(h.1$mids[1:2])*length(x.1) 
lines(xfit.1, yfit.1, col="blue", lwd=2)

x.2 <- Return[,3]
h.2 <- hist(x.2, breaks=15, col="light green", xlab="Daily Return", main="YINN") 
xfit.2<-seq(min(x.2),max(x.2),length=40) 
yfit.2<-dnorm(xfit.2,mean=mean(x.2),sd=sd(x.2)) 
yfit.2 <- yfit.2*diff(h.2$mids[1:2])*length(x.2) 
lines(xfit.2, yfit.2, col="dark green", lwd=2)

x.3 <- Return[,4]
h.3 <- hist(x.3, breaks=15, col="pink", xlab="Daily Return", main="XPP") 
xfit.3<-seq(min(x.3),max(x.3),length=40) 
yfit.3<-dnorm(xfit.3,mean=mean(x.3),sd=sd(x.3)) 
yfit.3 <- yfit.3*diff(h.3$mids[1:2])*length(x.3) 
lines(xfit.3, yfit.3, col="dark red", lwd=2)

x.4 <- Return[,5]
h.4 <- hist(x.4, breaks=15, col="yellow", xlab="Daily Return", main="YANG") 
xfit.4<-seq(min(x.4),max(x.4),length=40) 
yfit.4<-dnorm(xfit.4,mean=mean(x.4),sd=sd(x.4)) 
yfit.4 <- yfit.4*diff(h.4$mids[1:2])*length(x.4) 
lines(xfit.4, yfit.4, col="orange", lwd=2)

x.5 <- Return[,6]
h.5 <- hist(x.5, breaks=15, col="plum2", xlab="Daily Return", main="FXP") 
xfit.5<-seq(min(x.5),max(x.5),length=40) 
yfit.5<-dnorm(xfit.5,mean=mean(x.5),sd=sd(x.5)) 
yfit.5 <- yfit.5*diff(h.5$mids[1:2])*length(x.5) 
lines(xfit.5, yfit.5, col="purple", lwd=2)

x.6 <- Return[,7]
h.6 <- hist(x.6, breaks=15, col="grey", xlab="Daily Return", main="YXI") 
xfit.6<-seq(min(x.6),max(x.6),length=40) 
yfit.6<-dnorm(xfit.6,mean=mean(x.6),sd=sd(x.6)) 
yfit.6 <- yfit.6*diff(h.6$mids[1:2])*length(x.6) 
lines(xfit.6, yfit.6, col="black", lwd=2)