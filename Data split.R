# data seperation
# install.packages("lubridate",dependencies = T)
library(lubridate)
final <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\final_normalized.csv")
final$week.number <- as.factor(as.character(lubridate::isoweek(ymd(as.Date(final$date,format="%m/%d/%Y")))))
Final <- final[,-1]

# 4-year and 1-year
# continous date
n <- length(Final[,1])
round(0.8*n,0);round(0.2*n,0)
training1 <- Final[1:(round(0.8*n,0)-1),]
testing1 <- Final[(round(0.8*n,0)+1):n,]

# randomly 80/20 separation
set.seed(2018)
smp_size <- round(0.8*n,0)
train_ind <- sample(n, size = smp_size)
training2 <- Final[train_ind, ]
testing2 <- Final[-train_ind, ]

# 10-fold cross validation
# install.packages("cvTools", dependencies = T)
library(cvTools) #run the above line if you don't have this library
k <- 10
folds <- cvFolds(n, K=k)
Final$cv <- rep(0,n)

for(i in 1:k){
  training3 <- Final[folds$subsets[folds$which != i], ] #Set the training set
  testing3 <- Final[folds$subsets[folds$which == i], ] #Set the testing set
  
  newlm <- lm(ftse~.,data=training3) #Get your new linear model (just fit on the train data)
  newpred <- predict(newlm,newdata=testing3) #Get the predicitons for the testing set (from the model just fit on the train data)
  
  Final[folds$subsets[folds$which == i], ]$cv <- newpred #Put the hold out prediction in the data set for later use
}

training3 <- training3[,-ncol(training3)]
testing3 <- testing3[,-ncol(testing3)]

write.csv(training1, "C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\training1.csv", row.names = F)
write.csv(training2, "C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\training2.csv", row.names = F)
write.csv(training3, "C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\training3.csv", row.names = F)
write.csv(testing1, "C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\testing1.csv", row.names = F)
write.csv(testing2, "C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\testing2.csv", row.names = F)
write.csv(testing3, "C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\testing3.csv", row.names = F)
