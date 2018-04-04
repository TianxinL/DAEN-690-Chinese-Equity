library(corrplot)

stock <- read.csv("C:\\Users\\Tianxin.Liu\\Downloads\\Stocks2.csv")

library(RColorBrewer)

corrplot(cor(stock[, -1]),
         title = "Correlation Plot",
         method = "color", outline = T,
         addgrid.col = "darkgray", order="hclust",
         addrect = 4, rect.col = "black",
         rect.lwd = 5,cl.pos = "b",
         number.digits = 2, number.cex = 0.75)
corrplot(cor(stock[,-c(1,2)]), method = "color", outline = T, 
         addgrid.col = "black", order="hclust", addrect = 2,
         rect.col = "red", rect.lwd = 5,cl.pos = "b",
         tl.col = "indianred4", tl.cex = 1, cl.cex = 1, 
         addCoef.col = "white", number.digits = 2,
         number.cex = 2, 
         col = colorRampPalette(c("darkred","white","darkblue"))(100))

library(car)


mod <- lm(FTSE ~. -Date, data=stock)
vif(mod)
