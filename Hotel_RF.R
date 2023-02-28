

######################################################
##############   Hotel Satisfaction   ################
######################################################

setwd("D:/OneDrive/Documents/DM&ML/TABA/Datasets/")

# https://rap0d.github.io/study/2019/11/23/r_050_SVM/

data <- read.csv("Europe Hotel Booking Satisfaction Score.csv", header=T, na.strings=c(""), stringsAsFactors = T)
# Check na or null value
print(length(data[is.na(data)])) # 0
sum(!complete.cases(data)) # 0
# data<- data[complete.cases(data), ] 
str(data)
nrow(data)
# Delete an id column and assign it in the new variable.
hotel <- data[-1] 
# change name of values because it is too long.
hotel$satisfaction <- factor(hotel$satisfaction, 
                             levels = c("neutral or dissatisfied","satisfied"), 
                             labels = c("No", "Yes"))

hotel$Age <- cut(hotel$Age,
                 breaks = c(0, 10, 20, 30, 40, 50, 60, 70, max(hotel$Age)),
                 labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "71+"))
## as.numeric
hotel <- cbind(lapply(hotel[-16], function(x) as.numeric(as.factor(x))), hotel[16])
str(hotel)


# Sampling data due to the raw data size which has 103,904 rows.
sample <- sample(nrow(hotel), 0.1*nrow(hotel))
hotel <- hotel[sample, ]
# training / test
train <- sample(nrow(hotel), 0.7*nrow(hotel))
hotel.train <- hotel[train, ]
hotel.test <- hotel[-train, ]
nrow(hotel.train) # 7272
nrow(hotel.test) # 3118
# Both samples have similar amount of positives and negatives
prop.table(table(hotel.train$satisfaction)) # probablity of positive in train data
prop.table(table(hotel.test$satisfaction)) # probablity of positive in train data
# str(hotel.train)

##################################################################
###############              Random Forest            ############
##################################################################

# install.packages("randomForest")
library(randomForest)
set.seed(123)
hotel.forest <- randomForest(satisfaction ~ ., data = hotel.train,
                               na.action = na.roughfix, 
                               importance=TRUE) 

hotel.forest
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3 (3 selected out of 15 for node splits) Normally root of the number of independent variables are set as number of splits.
# user can take parameters such as ntree, mtry and so on.
# OOB estimate of  error rate: 6.83%
# 
# Confusion matrix:
#       No  Yes class.error
# No  3926  225  0.05420381
# Yes  272 2849  0.08715155

# type = "prob">> show the predicted results with probability
hotel.forest.pred <- predict(hotel.forest, newdata = hotel.test,
                               type = "prob")
head(hotel.forest.pred)

# type = "response">> show the predicted results by groups
hotel.forest.pred <- predict(hotel.forest, newdata = hotel.test,
                               type = "response")
head(hotel.forest.pred)


table(hotel.test$satisfaction, hotel.forest.pred,
      dnn = c("Actual", "Predicted"))
mean(hotel.test$satisfaction == hotel.forest.pred, na.rm = TRUE) # 0.9371392
# Randomforest exclude data if there is na value. 
#         Predicted
# Actual     No  Yes
#     No  1690   83
#     Yes  113  1232

##################################################################
###############       Draw a Cluster plot Forest      ############
##################################################################

library(cluster)
clusplot(x=na.omit(hotel.test[, -16]), clus = na.omit(hotel.forest.pred),
         color=TRUE, shade=TRUE, labels = 4, lines = 0,
         main = "Random Forest Classification from hotel satisfaction Dataset")

##################################################################
###############           predict.all = TRUE          ############
##################################################################
hotel.forest.predAll <- predict(hotel.forest, newdata = hotel.test,
                                  predict.all = TRUE)  # predict.all: Can see the distribution of individual case results how they are classified by each decision tree.
str(hotel.forest.predAll)
# aggregate : [1:3118] 
# individual: [1:3118, 1:500] 3118 == 3118 >> means no na values. Decision trees are 500. 

# Check 6th index how it's predicted
hotel.forest.predAll$individual[6, ] 
table(hotel.forest.predAll$individual[6, ]) # see 500 trees
# majority of trees out of 500 classified as 'no'
# Let's see if it is really no.
na.omit(hotel.forest.predAll$aggregate)[6]

######################################################
###############      varImpPlot          #############
######################################################
# The advantage of random forest is that you can see the degree to which predictors contribute to the model.Important indicators can be obtained with importance = TRUE, which was specified when generating the random forest model.
# (1) varImpPlot() -> plot
varImpPlot(hotel.forest, pch=21, color="black", bg="red",
           pt.cex = 1.2,
           main = "Variable Importance for hotel Satisfaction Classification")

# (2) importance() -> table
importance(hotel.forest)
importance(hotel.forest, type = 1)
importance(hotel.forest, type = 2)

######################################################
###############         heatmap          #############
######################################################
# install.packages("gplots")
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(hotel.forest)[, 1:2]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace = "none", key = FALSE,
          margins = c(10, 7), cexRow=1.5, cexCol=1.2, colRow = c("green4", "maroon"),
          main = "Variable Importance\n for hotel Satisfaction Classification")

######################################################
###############       Re-modeling        #############
######################################################
# give the selected parameters based on Gini
set.seed(123)
hotel.forest2 <- randomForest(satisfaction ~ Hotel.wifi.service +Type.of.Travel +
                                Type.Of.Booking + Common.Room.entertainment + Stay.comfort+
                                Cleanliness+ Age + Hotel.location + Departure.Arrival..convenience +
                                Checkin.Checkout.service + Ease.of.Online.booking + Other.service,
                              data = hotel.train,
                              na.action = na.roughfix,
                              importance=TRUE) 
hotel.forest2
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3
# OOB estimate of  error rate: 6.52%
# 
# Confusion matrix:
#       No  Yes  class.error
# No  3941  210  0.05059022
# Yes  264 2857  0.08458827

hotel.forest2.pred <- predict(hotel.forest2, newdata = hotel.test,
                             type = "prob")

hotel.forest2.pred <- predict(hotel.forest2, newdata = hotel.test,
                             type = "response")

table(hotel.test$satisfaction, hotel.forest2.pred,
      dnn = c("Actual", "Predicted"))
#         Predicted
# Actual   No  Yes
#     No  1688   87
#     Yes   86 1257


clusplot(x=na.omit(hotel.test[, -16]), clus = na.omit(hotel.forest2.pred),
         color=TRUE, shade=TRUE, labels = 4, lines = 0,
         main = "Random Forest Classification from hotel satisfaction Dataset")

