Monthly.Indicator <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\New_Monthly_INDICATORS.csv")
str(Monthly.Indicator)

library(corrplot)

corrplot(cor(Monthly.Indicator[,4:6]),method = "number")

corrplot(cor(Monthly.Indicator[,9:10]),method = "number")

corrplot(cor(na.omit(Monthly.Indicator[,14:15])),method = "number")

corrplot(cor(Monthly.Indicator[,24:27]),method = "number")

corrplot(cor(na.omit(Monthly.Indicator[,27:32])),method = "number")
