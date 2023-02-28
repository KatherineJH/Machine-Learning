##############################################################
# setting my working directory
setwd("D:/OneDrive/Documents/DM&ML/TABA/Datasets/")

Data <- read.csv("Diabetes_012_health_indicators_BRFSS2015.csv", header=T, na.strings=c(""), stringsAsFactors = T)

# (2)Pre-processing
## sampling data due to the raw data size in the original file 
set.seed(123)
sample <- sample(nrow(Data), 0.1*nrow(Data))
Data <- Data[sample, ]

# make this data set as Binomial 
Data$Diabetes_012[Data$Diabetes_012 != 0] <- 1
# change 1st column name as Diabetes_status
colnames(Data)[1] ="Diabetes_status"

# set the labels with names easy catching 
Data$Diabetes_status <- factor(Data$Diabetes_status, 
                               levels = c("0","1"), 
                               labels = c("No", "Yes"))
# Data normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
## Normalize all variables except one factor label(Dependent/predicted variable).
Diabetes.norm <- as.data.frame(lapply(Data[2:22], normalize))
summary(Diabetes.norm[c("GenHlth", "MentHlth", "PhysHlth")])
## column-wise bindibg
Data <- cbind(Data$Diabetes_status, Diabetes.norm)
## rename(data, " new name" = "name to be changed")
library(dplyr)
Data <- rename(Data, "Diabetes_status" = "Data$Diabetes_status")
str(Data)


# Split data into training data / test data 
set.seed(123)
train <- sample(nrow(Data), 0.7*nrow(Data))
diabetes.train <- Data[train, ]
diabetes.test <- Data[-train, ]

##########################################################
library(class)
library(gmodels)
# str(diabetes.train)
# str(diabetes.test)
diabetes.knn.pred1 <- knn(train = diabetes.train[,2:22], test = diabetes.test[,2:22], cl = diabetes.train[,1], k=20)
CrossTable(x = diabetes.test[,1], y = diabetes.knn.pred1, prop.chisq=FALSE)
table(diabetes.test$Diabetes_status, diabetes.knn.pred1,
      dnn=c("Actual", "Predicted"))
mean(diabetes.test$Diabetes_status == diabetes.knn.pred1) # Predict Accuracy >> 83.76%(TN: 133)

##########################################################
# Try Multiple k values
accuracy <- function(actual, predicted) {
  mean(actual == predicted)
} # return 0 and 1


knn.to.try <- seq(1,by=2, len=50)
acc.knn = rep(x = 0, times = length(knn.to.try))

for(i in seq_along(knn.to.try)) {
  diabetes.knn.pred2 <- knn(train = diabetes.train[,2:22], 
                     test = diabetes.test[,2:22], 
                     cl = diabetes.train[,1], 
                     k = knn.to.try[i])
  acc.knn[i] <- accuracy(diabetes.test[,1], diabetes.knn.pred2)
  if (i == 21){
    CrossTable(x = diabetes.test[,1], y = diabetes.knn.pred2, prop.chisq=FALSE)
  }
}

# the best k-value
which(acc.knn == max(acc.knn))

##########################################################################
# plot accuracy vs choice of k
plot(acc.knn, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
       xlab = "k, number of neighbors", ylab = "classification accuracy",
       main = "Accuracy vs Neighbors")
# add lines indicating k with best accuracy
abline(v = which(acc.knn == max(acc.knn)), col = "darkorange", lwd = 1.5)
# add line for max accuracy seen
abline(h = max(acc.knn), col = "grey", lty = 2)

###############################################################################

# Run knn model again with the best k-value found above
diabetes.knn.pred3 <- knn(train = diabetes.train[,2:22], test = diabetes.test[,2:22], cl = diabetes.train[,1], k=14)
CrossTable(x = diabetes.test[,1], y = diabetes.knn.pred3, prop.chisq=FALSE)
table(diabetes.test$Diabetes_status, diabetes.knn.pred3,
      dnn=c("Actual", "Predicted"))
mean(diabetes.test$Diabetes_status == diabetes.knn.pred3) # Predict Accuracy >> increased to 83.77% and TN improved to 174

###########################################################
################### Using caret library  ##################
###########################################################

set.seed(123)
half <- sample(nrow(diabetes.train), 0.5*nrow(diabetes.train))
diabetes.train2 <- diabetes.train[half, ]

half <- sample(nrow(diabetes.test), 0.5*nrow(diabetes.test))
diabetes.test2 <- diabetes.test[half, ]

# train with caret package
library(caret)
set.seed(123) 
#10-fold cross validation: 5times repeat controller 
ctrl <- trainControl(method="repeatedcv", number = 10, repeats=5) 
# training
knnFit <- train(Diabetes_status ~ ., 
                  data = diabetes.train2, method ='knn',
                  trControl = ctrl,
                  preProcess = c("center", "scale"),
                  tuneLength=20) #  If tuneLength = 20, 20 candidate models will be created.
knnFit

# prediction with test date and accuracy with Statistics 
knnPredict <- predict(knnFit, newdata = diabetes.test) # predict value with test data
confusionMatrix(knnPredict, diabetes.test$Diabetes_status) #Confusion Matrix and Statistics

