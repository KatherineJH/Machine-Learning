setwd("D:/OneDrive/Documents/DM&ML/TABA/Datasets/")
# NULL은 초기화되지 않은 값.
# NA은 아직 모른다(Not Available): 할당하지 않은 값, 결측치.


Data <- read.csv("heart_disease_health_indicators_BRFSS2015.csv", header=T, na.strings=c(""), stringsAsFactors = T)

# # 결측치 없음
# print(length(Data[is.na(Data)])) 
# sum(!complete.cases(Data)) 

str(Data)
# 1. label이 될 컬럼을 factor로 변환한다.
Data$HeartDiseaseorAttack <- factor(Data$HeartDiseaseorAttack,
                                    levels = c("0", "1"),
                                    labels = c("no", "yes"))

mean(Data$HeartDiseaseorAttack == "no") # 0.9058144 음성
mean(Data$HeartDiseaseorAttack == "yes") # 0.09418559 양성
sum(!complete.cases(Data))

# 2. 데이터를 정규화 한다.
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
## factor인 label을 제외하고 normalize한다.
hd_norm <- as.data.frame(lapply(Data[2:22], normalize))
summary(hd_norm[c("GenHlth", "MentHlth", "PhysHlth")])
## 다시 열을 합친다.
Data <- cbind(Data$HeartDiseaseorAttack, hd_norm)
## 이름 바꿔준다
#rename(테이블이름, "바꿀 이름" = "원래 이름")
library(dplyr)
Data <- rename(Data, "HeartDiseaseorAttack" = "Data$HeartDiseaseorAttack")

# 3. factor 치환
Data$HighBP <- as.factor(Data$HighBP)
Data$HighChol <- as.factor(Data$HighChol)
Data$CholCheck <- as.factor(Data$CholCheck)
Data$Smoker <- as.factor(Data$Smoker)
Data$Stroke <- as.factor(Data$Stroke)
Data$Diabetes <- as.factor(Data$Diabetes)
Data$PhysActivity <- as.factor(Data$PhysActivity)
Data$Fruits <- as.factor(Data$Fruits)
Data$Veggies <- as.factor(Data$Veggies)
Data$HvyAlcoholConsump <- as.factor(Data$HvyAlcoholConsump)
Data$AnyHealthcare <- as.factor(Data$AnyHealthcare)
Data$NoDocbcCost <- as.factor(Data$NoDocbcCost)
Data$DiffWalk <- as.factor(Data$DiffWalk)
Data$Sex <- as.factor(Data$Sex)

# levels(Data$HighBP)<-c("low","high")
# levels(Data$HighChol)<-c("low","high")
# levels(Data$CholCheck)<-c("no","yes")
# levels(Data$Smoker)<-c("no","yes")
# levels(Data$Stroke)<-c("no","yes")
# levels(Data$Diabetes)<-c("no","type1", "type 2")
# levels(Data$PhysActivity)<-c("no","yes")
# levels(Data$Fruits)<-c("no","yes")
# levels(Data$Veggies)<-c("no","yes")
# levels(Data$HvyAlcoholConsump)<-c("no","yes")
# levels(Data$AnyHealthcare)<-c("no","yes")
# levels(Data$NoDocbcCost)<-c("no","yes")
# levels(Data$DiffWalk)<-c("no","yes")
# levels(Data$Sex)<-c("Male","Female")

str(Data)

sample <- sample(nrow(Data), 0.05*nrow(Data))
Data <- Data[sample, ]

# training data / test data 쪼개기
set.seed(123)
train <- sample(nrow(Data), 0.7*nrow(Data))
hd.train <- Data[train, ]
hd.test <- Data[-train, ]

str(hd.train)
str(hd.test)

table(hd.train$HeartDiseaseorAttack)
table(hd.test$HeartDiseaseorAttack)
# 잘 나뉘어졌음
prop.table(table(hd.train$HeartDiseaseorAttack))
prop.table(table(hd.test$HeartDiseaseorAttack))


###############################################
#################   KNN    ####################
###############################################
str(hd.train)
str(hd.test)
sum(!complete.cases(hd.train))
sum(!complete.cases(hd.test))

library(class)
library(gmodels)
hd_knn_pred <- knn(train = hd.train[,2:22], test = hd.test[,2:22], cl = hd.train[,1], k=20)
CrossTable(x = hd.test[,1], y = hd_knn_pred, prop.chisq=FALSE) # 양성을 그닥 잘 분류하지는 못함


# Try Multiple k values
accuracy = function(actual, predicted) {
  mean(actual == predicted)
} # return 0 and 1


k_to_try = seq(1,by=2, len=50)
acc_k = rep(x = 0, times = length(k_to_try))

for(i in seq_along(k_to_try)) {
  hd_test_pred = knn(train = hd.train[,2:22], 
                     test = hd.test[,2:22], 
                     cl = hd.train[,1], 
                     k = k_to_try[i])
  acc_k[i] = accuracy(hd.test[,1], hd_test_pred)
  if (i == 21){
    CrossTable(x = hd.test[,1], y = hd_test_pred, prop.chisq=FALSE)
  }
}

# plot accuracy vs choice of k
plot(acc_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification accuracy",
     main = "Accuracy vs Neighbors")
# add lines indicating k with best accuracy
abline(v = which(acc_k == max(acc_k)), col = "darkorange", lwd = 1.5)
# add line for max accuracy seen
abline(h = max(acc_k), col = "grey", lty = 2)

hd_knn_pred <- knn(train = hd.train[,2:22], test = hd.test[,2:22], cl = hd.train[,1], k=9)
CrossTable(x = hd.test[,1], y = hd_knn_pred, prop.chisq=FALSE) # 여전히 양성을 그닥 잘 분류하지는 못함. 낫다고 할 수 있으나, 실제 상황이라면 전혀 좋은 결과라고 말하기 어렵다.


###############################################
# scale로 전부 다시 돌리기
###############################################
setwd("D:/OneDrive/Documents/DM&ML/TABA/Datasets/")
Data <- read.csv("heart_disease_health_indicators_BRFSS2015.csv", header=T, na.strings=c(""), stringsAsFactors = T)


str(Data)
Data$HeartDiseaseorAttack <- factor(Data$HeartDiseaseorAttack,
                                    levels = c("0", "1"),
                                    labels = c("no", "yes"))

mean(Data$HeartDiseaseorAttack == "no") # 0.9058144 음성
mean(Data$HeartDiseaseorAttack == "yes") # 0.09418559 양성
sum(!complete.cases(Data))

# 2. 데이터를 표준화 한다.
hd_sd <- as.data.frame(lapply(Data[2:22], scale))
## 다시 열을 합친다.
Data <- cbind(Data$HeartDiseaseorAttack, hd_norm)
## 이름 바꿔준다
library(dplyr)
Data <- rename(Data, "HeartDiseaseorAttack" = "Data$HeartDiseaseorAttack")

# 3. factor 치환
Data$HighBP <- as.factor(Data$HighBP)
Data$HighChol <- as.factor(Data$HighChol)
Data$CholCheck <- as.factor(Data$CholCheck)
Data$Smoker <- as.factor(Data$Smoker)
Data$Stroke <- as.factor(Data$Stroke)
Data$Diabetes <- as.factor(Data$Diabetes)
Data$PhysActivity <- as.factor(Data$PhysActivity)
Data$Fruits <- as.factor(Data$Fruits)
Data$Veggies <- as.factor(Data$Veggies)
Data$HvyAlcoholConsump <- as.factor(Data$HvyAlcoholConsump)
Data$AnyHealthcare <- as.factor(Data$AnyHealthcare)
Data$NoDocbcCost <- as.factor(Data$NoDocbcCost)
Data$DiffWalk <- as.factor(Data$DiffWalk)
Data$Sex <- as.factor(Data$Sex)

str(Data)

sample <- sample(nrow(Data), 0.05*nrow(Data))
Data <- Data[sample, ]

# training data / test data 쪼개기
set.seed(123)
train <- sample(nrow(Data), 0.7*nrow(Data))
hd.train <- Data[train, ]
hd.test <- Data[-train, ]

str(hd.train)
str(hd.test)

table(hd.train$HeartDiseaseorAttack)
table(hd.test$HeartDiseaseorAttack)
# 잘 나뉘어졌음
prop.table(table(hd.train$HeartDiseaseorAttack))
prop.table(table(hd.test$HeartDiseaseorAttack))

# 모델 돌리기
library(class)
library(gmodels)
hd_knn_pred <- knn(train = hd.train[,2:22], test = hd.test[,2:22], cl = hd.train[,1], k=20)
CrossTable(x = hd.test[,1], y = hd_knn_pred, prop.chisq=FALSE) # 양성을 그닥 잘 분류하지는 못함


# Try Multiple k values
accuracy = function(actual, predicted) {
  mean(actual == predicted)
} # return 0 and 1


k_to_try = seq(1,by=2, len=50)
acc_k = rep(x = 0, times = length(k_to_try))

for(i in seq_along(k_to_try)) {
  hd_test_pred = knn(train = hd.train[,2:22], 
                     test = hd.test[,2:22], 
                     cl = hd.train[,1], 
                     k = k_to_try[i])
  acc_k[i] = accuracy(hd.test[,1], hd_test_pred)
  if (i == 21){
    CrossTable(x = hd.test[,1], y = hd_test_pred, prop.chisq=FALSE)
  }
}

# plot accuracy vs choice of k
plot(acc_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification accuracy",
     main = "Accuracy vs Neighbors")
# add lines indicating k with best accuracy
abline(v = which(acc_k == max(acc_k)), col = "darkorange", lwd = 1.5)
# add line for max accuracy seen
abline(h = max(acc_k), col = "grey", lty = 2)

hd_knn_pred <- knn(train = hd.train[,2:22], test = hd.test[,2:22], cl = hd.train[,1], k=10)
CrossTable(x = hd.test[,1], y = hd_knn_pred, prop.chisq=FALSE) # 여전히 양성을 그닥 잘 분류하지는 못함. 낫다고 할 수 있으나, 실제 상황이라면 전혀 좋은 결과라고 말하기 어렵다.

# 
# ###############################################
# ###########    KNN     ########################
# ###############################################
# str(hd.train)
# str(hd.test)
# sum(!complete.cases(hd.train))
# sum(!complete.cases(hd.test))
# 
# library(class)
# library(gmodels)
# hd_knn_pred <- knn(train = hd.train[,2:22], test = hd.test[,2:22], cl = hd.train[,1], k=20)
# CrossTable(x = hd.test[,1], y = hd_knn_pred, prop.chisq=FALSE) # 양성을 그닥 잘 분류하지는 못함
# 
# 
# # Try Multiple k values
# accuracy = function(actual, predicted) {
#   mean(actual == predicted)
# } # return 0 and 1
# 
# 
# k_to_try = seq(1,by=2, len=50)
# acc_k = rep(x = 0, times = length(k_to_try))
# 
# for(i in seq_along(k_to_try)) {
#   hd_test_pred = knn(train = hd.train[,2:22], 
#                      test = hd.test[,2:22], 
#                      cl = hd.train[,1], 
#                      k = k_to_try[i])
#   acc_k[i] = accuracy(hd.test[,1], hd_test_pred)
#   if (i == 21){
#     CrossTable(x = hd.test[,1], y = hd_test_pred, prop.chisq=FALSE)
#   }
# }
# 
# # plot accuracy vs choice of k
# plot(acc_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
#      xlab = "k, number of neighbors", ylab = "classification accuracy",
#      main = "Accuracy vs Neighbors")
# # add lines indicating k with best accuracy
# abline(v = which(acc_k == max(acc_k)), col = "darkorange", lwd = 1.5)
# # add line for max accuracy seen
# abline(h = max(acc_k), col = "grey", lty = 2)
# 
# hd_knn_pred <- knn(train = hd.train[,2:22], test = hd.test[,2:22], cl = hd.train[,1], k=9)
# CrossTable(x = hd.test[,1], y = hd_knn_pred, prop.chisq=FALSE) # 여전히 양성을 그닥 잘 분류하지는 못함. 낫다고 할 수 있으나, 실제 상황이라면 전혀 좋은 결과라고 말하기 어렵다.
# 
# 
# 
# #Scale ... 와，，，　더　최악임．．．
# hd_sd <- as.data.frame(lapply(Data[2:22], scale))
# summary(hd_sd[c("GenHlth", "MentHlth", "PhysHlth")])
# 
# ## 다시 열을 합친다.
# Data <- cbind(Data$HeartDiseaseorAttack, hd_norm)
# ## 이름 바꿔준다
# #rename(테이블이름, "바꿀 이름" = "원래 이름")
# library(dplyr)
# Data <- rename(Data, "HeartDiseaseorAttack" = "Data$HeartDiseaseorAttack")
# 
# 
# sample <- sample(nrow(Data), 0.05*nrow(Data))
# Data <- Data[sample, ]
# 
# # training data / test data 쪼개기
# set.seed(123)
# train <- sample(nrow(Data), 0.7*nrow(Data))
# hd.train <- Data[train, ]
# hd.test <- Data[-train, ]
# 
# hd_knn_pred <- knn(train = hd.train[,2:22], test = hd.test[,2:22], cl = hd.train[,1], k=21)
# 
# CrossTable(x = hd.test[,1], y = hd_knn_pred, prop.chisq=FALSE) # 양성을 그닥 잘 분류하지는 못



# library(e1071)
# set.seed(123)
# hd.svm <- svm(HeartDiseaseorAttack ~ ., data = hd.train,
#                 kernel="linear", #kernel의 디폴트 값은 RBF(방사형 기저함수) 함수를 사용한다.
#                 # 여기서는 고차원 mapping이 필요 없음. 차원의 확장 없이 주어진 예측 변수 만으로 초평면을 탐색 -> Linear 사용.
#                 cost=1, # 0보다 큰 값을 입력해야 한다. 디폴트 값은 1
#                 # >> 숫자가 커지면, 오분류를 심각하게 받아들인다 >> 패널티가 커진다 
#                 # >> 많은 데이터를 충족시킬 수 있는 복잡한 분류 경계선을 생성 
#                 # >> 훈련 데이터에 대해서는 오분류가 작이지고 정확도가 올라가지만 과대적합으로 인해, 새로운 케이스에서는 분류 성능이 떨어지게 된다.
#                 # >> cost가 작은 경우, 단순한 분류 경계선을 만들고 과소적합 가능성이 발생하게 된다. 
#                 scale = FALSE) # scale: 데이터 표준화 할 지 말지 여부.
# summary(hd.svm) # Number of Support Vectors:  2106( 1311 795 )
# 
# table(hd.test$HeartDiseaseorAttack)
# 
# hd.svm.pred <- predict(hd.svm, newdata = hd.test)
# head(hd.svm.pred)
# table(hd.test$HeartDiseaseorAttack, hd.svm.pred,
#       dnn=c("Actual", "Predicted"))
# mean(hd.test$HeartDiseaseorAttack == hd.svm.pred)
# 
# # 이번에는 cost 값을 변화시켜, 서포트벡터와 분류 경계선 및 마진에 미치는 영향을 비교 해보자.
# set.seed(123)
# hd.svm2 <- svm(HeartDiseaseorAttack~., data = hd.train,
#                  kernel="linear", cost=100, scale = FALSE)
# summary(hd.svm2) # Support Vectors의 개수가 2488로 늘었다... 좋은거야...?
# # 실제 vs 예측
# hd.svm2.pred <- predict(hd.svm2, newdata = hd.test)
# table(hd.test$HeartDiseaseorAttack, hd.svm2.pred,
#       dnn = c("Actual", "Predicted"))             ## 하...ㅅㅂ.... 왜 모조리 0으로 예측되는거지...ㅠㅠ
# mean(hd.test$HeartDiseaseorAttack == hd.svm2.pred) # 




################################
######## 아몰랑 아몰랑##########
################################

library(randomForest)
set.seed(123)
heart.forest <- randomForest(HeartDiseaseorAttack ~ ., data = hd.train,
                                na.action = na.roughfix, # na.action: 결측값 처리 방법 지정.
                                # na.roughfix: 결측값이 숫자라면, 해당 열의 중위수. 명목형 변수라면, 최빈도 범주값.
                                importance=TRUE) # 중요한 변수를 *로 알려줌


heart.forest

# type = "prob">> 예측 결과를 확률로 보여줌
heart.forest.pred <- predict(heart.forest, newdata = hd.test,
                                type = "prob")
head(heart.forest.pred)

# type = "response">> 예측 결과를 예측 범주/집단으로 보여줌
heart.forest.pred <- predict(heart.forest, newdata = hd.test,
                                type = "response")
head(heart.forest.pred)

table(hd.test$HeartDiseaseorAttack, heart.forest.pred,
      dnn = c("Actual", "Predicted"))
mean(hd.test$HeartDiseaseorAttack == heart.forest.pred, na.rm = TRUE) # 90.59%
# 랜덤포레스트에서 결측값이 포함된 케이스는 분류대상에서 제외한다.

