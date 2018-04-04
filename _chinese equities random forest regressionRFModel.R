library(randomForest) #For Model
library(Metrics) #for RMSE Function
###############################################

## Set working directory to where the csv files are
##using setwd() #####

####  4 year and 1 year seperation data #########

dataset_training1 = read.csv("training1.csv")
dataset_testing1 = read.csv("testing1.csv")

rf_Model1 = randomForest(ftse ~ ., data=dataset_training1, ntree = 500,
                                       importance=TRUE)
jpeg('rf_Model1_ErrorRate.jpg')
plot(rf_Model1) #Shows Error Rate
dev.off()

jpeg('rf_Model1_VarImp.jpg')
varImpPlot(rf_Model1) #Variable Importance Plot
dev.off()


rf_Pred1 = predict(rf_Model1,dataset_testing1)

RMSE_Model1 = rmse(dataset_testing1$ftse,rf_Pred1) #Compare it with other model's RMSE to assess performance
(RMSE_Model1)

#######################################

########## 80-20 Split Data ########## 
#### Best Model since RMSE is the least (RMSE = Root Mean Square Error) 

dataset_training2 = read.csv("training2.csv")
dataset_testing2 = read.csv("testing2.csv")

rf_Model2 = randomForest(ftse ~ ., data=dataset_training2, ntree = 500,
                         importance=TRUE)

jpeg('rf_Model2_ErrorRate.jpg')
plot(rf_Model2) #Shows Error Rate  
dev.off()

jpeg('rf_Model2_VarImp.jpg')
varImpPlot(rf_Model2) #Variable Importance Plot
dev.off()

rf_Pred2 = predict(rf_Model2,dataset_testing2)

RMSE_Model2 = rmse(dataset_testing2$ftse,rf_Pred2)
(RMSE_Model2)
##########################################

######## 10 fold cross validation ########
### Getting very close to the Best Model. Constantly varying RMSE #####

dataset_training3 = read.csv("training3.csv")
dataset_testing3 = read.csv("testing3.csv")

rf_Model3 = randomForest(ftse ~ ., data=dataset_training3, ntree = 500,
                         importance=TRUE)

jpeg('rf_Model3_ErrorRate.jpg')
plot(rf_Model3) #Shows Error Rate
dev.off()

jpeg('rf_Model3_VarImp.jpg')
varImpPlot(rf_Model3) #Variable Importance Plot
dev.off()

rf_Pred3 = predict(rf_Model3,dataset_testing3)

RMSE_Model3 = rmse(dataset_testing3$ftse,rf_Pred3)
(RMSE_Model3)

