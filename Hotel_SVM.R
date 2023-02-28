

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

hotel$Gender <- as.numeric(hotel$Gender)
# hotel$Gender <-  ifelse(hotel$Gender =="Male",0, 1)

# There are not many columns to be standardized or normalized therefore I binned the age values.
# Age(Binning)
hotel$Age <- cut(hotel$Age,
                 breaks = c(0, 10, 20, 30, 40, 50, 60, 70, max(hotel$Age)),
                 labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "71+"))
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

######################################################
############## Support Vector Machine ################
######################################################
library(e1071) # for svm and tunning function
set.seed(123)

hotel.svm <- svm(satisfaction ~ ., data = hotel.train,
                   kernel="linear", #kernel="linear" is for linear hyperplane.
                   cost=1, # default is 1 and it should be bigger than 0.
                   scale = FALSE) # scale: not need scale for the data, this time
summary(hotel.svm) # Number of Support Vectors:  2836( 1419  1417 )
# Number of Classes:  2 
# Levels: No Yes

hotel.svm.pred <- predict(hotel.svm, newdata = hotel.test)
head(hotel.svm.pred)
table(hotel.test$satisfaction, hotel.svm.pred,
      dnn = c("Actual", "Predicted"))

# Just in case if the output has any NA values.
mean(hotel.test$satisfaction == hotel.svm.pred)
table(na.omit(hotel.test)$satisfaction, hotel.svm.pred, dnn = c("Actual", "Predicted"))
mean(na.omit(hotel.test)$satisfaction == hotel.svm.pred)

##################################################
################      Tunning      ###############
######### < 10-fold cross validation > ###########
##################################################

set.seed(123)
hotel.svm.tuned <- tune.svm(satisfaction~., data = hotel.train,
                            gamma = 10^(-2:2), 
                            cost=2^(-2:2)) 
# gamma: determine the hyperplane that divides the data.
# As gamma grows, more support vectors are selected, resulting in a more complex model (curved classification boundaries) >> It can be said that error values may increase when new data come in.
# gamma must be set to a value greater than 0.
# 5(-2:2)*5(-2:2) is equal to 25 >>  25 models 
# tune.svm(): cost is a parameter to find the best cost to prevent overfitting and gamma means the slope of the hyperplane.

summary(hotel.svm.tuned)
# Parameter tuning of ‘svm’:
# - sampling method: 10-fold cross validation 
# - best parameters: gamma    cost
#                    0.1       4
hotel.svm.tuned$best.model$gamma # 0.1
hotel.svm.tuned$best.model$cost # 4

#############################################
###########      Re-runnging      ###########
#############################################
# re-run the model with the best gamma and the best cost 
set.seed(123)
hotel.svm2 <- svm(satisfaction ~., data = hotel.train,
                   # kernel="linear",
                   gamma= 0.1, cost=4)
summary(hotel.svm2)
hotel.svm2.pred <- predict(hotel.svm2, hotel.test)

table(hotel.test$satisfaction, hotel.svm2.pred,
      dnn = c("Actual", "Predict"))
mean(hotel.test$satisfaction == hotel.svm2.pred) # improved 84.60% >> to 92.14%

# Just in case if the output has any NA values.
table(na.omit(hotel.test)$satisfaction, hotel.svm2.pred, dnn = c("Actual", "Predicted"))
mean(na.omit(hotel.test)$satisfaction == hotel.svm2.pred) 

#############################################
###########      Draw ggplot      ###########
#############################################

###########    hotel.svm (1)    #############
library(ggplot2)
hotel.mds <- data.frame(cmdscale(dist(hotel.train[, -16])))
ggplot(hotel.mds, aes(x=X1, y=X2)) + 
  geom_point(aes(color=hotel.train[, 16],
                 shape=hotel.train[, 16]), size=2) +
  geom_point(data = hotel.mds[hotel.svm$index, ],
             color="dimgray", shape=21,
             stroke=1.0, size=5) +
  labs(color="Species", shape="Species", x="Dimension 1", y="Dimension 2",
       title="SVM(1) Classification from Hotel Satisfaction Dataset") +
  theme(plot.title = element_text(face="bold"))
##############################################

###########     hotel.svm (2)   ##############
library(ggplot2)
hotel.mds2 <- hotel.mds
ggplot(hotel.mds2, aes(x=X1, y=X2)) + 
  geom_point(aes(color=hotel.train[, 16],
                 shape=hotel.train[, 16]), size=2) +
  geom_point(data = hotel.mds2[hotel.svm2$index, ],
             color="dimgray", shape=21,
             stroke=1.0, size=5) +
  labs(color="Species", shape="Species", x="Dimension 1", y="Dimension 2",
       title="SVM(2) Classification from Hotel Satisfaction Dataset") +
  theme(plot.title = element_text(face="bold"))

###############################################
# tpr = tp / (tp + fn)
# 0.8581921 = 1519/(1519+251) 
# fpr = fp / (fp + tn)
# 0.1810089 = 244/(244+1104)
# fnr = fn / (fn + tp)
# 0.1418079 = 251/(251+1519)
# tnr = tn / (tn + fp)
# 0.8189911 = 1104/(1104+244)
# 
# ###############################################
# tpr = tp / (tp + fn)
# 0.9350649 = 1656/(1656+115) 
# fpr = fp / (fp + tn)
# 0.07943578 = 107/(107+1240)
# fnr = fn / (fn + tp)
# 0.06493506 = 115/(115+1656)
# tnr = tn / (tn + fp)
# 0.9205642 = 1240/(1240+107)



