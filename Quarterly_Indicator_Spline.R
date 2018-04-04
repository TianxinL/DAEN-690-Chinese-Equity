# Quarterly to Weekly
# Load DAta
mydata <- read.csv("C:\\Users\\Tianxin.Liu\\Downloads\\QUARTERLY_INDICATORS.csv")

# Load Library
# install.packages("zoo",dependencies = T)
library(zoo)

# Data Subset
mydata1 <- na.omit(mydata[,2:5])

# Quarter in year-month-day format
mydata1$qvar = as.Date(mydata1$New.Date,format = "%m/%d/%Y")

# Creating a daily sequence for the quarterly range
Daily1 = seq(mydata1$qvar[1], tail(mydata1$qvar,1), by="day")


# Cubic interpolation using spline()
Prime = data.frame(qvar=Daily1, prime=spline(mydata1[c("qvar","PRIME_Industry_Value")], method="fmm", xout=Daily1)$y)
Second = data.frame(qvar=Daily1, second=spline(mydata1[c("qvar","Second_Ind_Value")], method="fmm", xout=Daily1)$y)
Tertiary = data.frame(qvar=Daily1, tertiary=spline(mydata1[c("qvar","Tertiary_Ind_Value")], method="fmm", xout=Daily1)$y)

# Load DAta
weekly <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\Weekly_Indicator_Duplicate2.csv")
Date <- as.Date(weekly$Date,format = "%m/%d/%Y")

New.Weekly <- data.frame(Date)
New.Weekly$Prime.Ind.Value <- Prime$prime[match(New.Weekly$Date,Prime$qvar)]
New.Weekly$Second.Ind.Value <- Second$second[match(New.Weekly$Date,Second$qvar)]
New.Weekly$Tertiary.Ind.Value <- Tertiary$tertiary[match(New.Weekly$Date,Tertiary$qvar)]

write.csv(New.Weekly,"C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\Quarterly_Indicator_Spline.csv", row.names = FALSE)