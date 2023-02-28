#####################
### Random Forest ###
##################### 



# 랜덤 포레스트
# - 앙상블(ensemble) 학습 절차에 따라 다수의 의사결정 나무를 생성한 후, 개별 의사결정 나무의 예측 결과를 종합하여 데이터를 분류
# - 배깅(bagging, bootstrap, aggregating)의 기본 원리에 무작위적인 예측 변수 선택 과정을 결합
# - Decision Tree 모델의 다양성을 증가시켜, 과적합 가능성을 줄여주고 일반화 가능성을 향상.
## 분석 절차
# - bootstrap 표본을 바탕으로 다수의 Decision Tree 생성
#   >> 훈련 데이터로부터 복원 추출 방식으로 N개의 케이스를 반복 추출하여 분석에 사용할 Decision Tree 들을 생성
#   >> 개별 Decision Tree를 생성할 때 M개의 변수 중 노드 분할에 사용할 m개의 변수를 선택(이 때 변수의 개수 m은 모든 노드에 대해 동일하게 적용)
# - 개별 Decision Tree는 포함된 케이스들이 속한 범주의 상대적 빈도에 따라 최종 노드의 귀속 집단이 결정
# - 새로운 케이스를 분류할 때는 모든 의사결정 나무로부터 분류결과를 집계하여 가장 많이 분류된 집단을 최종 귀속 집단으로 결정.

setwd("D:/OneDrive/Documents/DM&ML/TABA/Datasets/")
# NULL은 초기화되지 않은 값.
# NA은 아직 모른다(Not Available): 할당하지 않은 값, 결측치.


Data <- read.csv("Diabetes_012_health_indicators_BRFSS2015.csv", header=T, na.strings=c(""), stringsAsFactors = T)

# re-sampling the original date due to the data size with 253,680 rows
nrow(Data)
sample <- sample(nrow(Data), 0.1*nrow(Data))
Data <- Data[sample, ]
nrow(Data)
sum(is.null(Data))
sum(is.na(Data))

# make this data set as Binomial 
Data$Diabetes_012[Data$Diabetes_012 != 0] <- 1
# change 1st column name as Diabetes_status
colnames(Data)[1] ="Diabetes_status"

# set the labels with names easy catching 
Data$Diabetes_status <- factor(Data$Diabetes_status, 
                               levels = c("0","1"), 
                               labels = c("No", "Yes"))
Data$Sex <- as.factor(Data$Sex)
Data$HighBP <- as.factor(Data$HighBP)
Data$HighChol <- as.factor(Data$HighChol)
Data$CholCheck <- as.factor(Data$CholCheck)
Data$Smoker <- as.factor(Data$Smoker)
Data$Stroke <- as.factor(Data$Stroke)
Data$HeartDiseaseorAttack <- as.factor(Data$HeartDiseaseorAttack)
Data$PhysActivity <- as.factor(Data$PhysActivity)
Data$Fruits <- as.factor(Data$Fruits)
Data$Veggies <- as.factor(Data$Veggies)
Data$HvyAlcoholConsump <- as.factor(Data$HvyAlcoholConsump)
Data$AnyHealthcare <- as.factor(Data$AnyHealthcare)
Data$NoDocbcCost <- as.factor(Data$NoDocbcCost)
Data$DiffWalk <- as.factor(Data$DiffWalk)
# Data$Sex <- as.factor(Data$Sex)
# below data are already binned in the original data set >> as.factor
Data$Age <- as.factor(Data$Age)
Data$Education <- as.factor(Data$Education)
Data$Income <- as.factor(Data$Income)
Data$GenHlth <- as.factor(Data$GenHlth)
# (1)EDA
## Status vs Gender
table(Data$Diabetes_status, Data$Sex)
## Status vs Age
boxplot(Age~Diabetes_status,data=Data, main="Status vs Age",
        xlab="Diabetes_status", ylab="Age")
library(ggplot2)
ggplot(Data, aes(x = Age, y = BMI, color = factor(Diabetes_status))) +
  geom_point(aes(shape = factor(Diabetes_status)))

# (2)Pre-processing
str(Data)

# Split data into training data / test data 
set.seed(123)
train <- sample(nrow(Data), 0.7*nrow(Data))
diabetes.train <- Data[train, ]
diabetes.test <- Data[-train, ]

nrow(diabetes.train) # 17757 sample
nrow(diabetes.test) # 7611 sample

####################################################
#######            랜덤 포레스트          ##########
####################################################

# install.packages("randomForest")
library(randomForest)
set.seed(123)
diabetes.forest <- randomForest(Diabetes_status ~ ., data = diabetes.train,
                          na.action = na.roughfix, 
                          importance=TRUE) 
# na.action: 결측값 처리 방법 지정.
# na.roughfix: 결측값이 숫자라면, 해당 열의 중위수. 명목형 변수라면, 최빈도 범주값.
#  importance=TRUE: 중요한 변수를 *로 알려줌
diabetes.forest
# Call:
#   randomForest(formula = Diabetes_status ~ ., data = diabetes.train,      importance = TRUE, na.action = na.roughfix) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 4
# 
# OOB estimate of  error rate: 15.66%
# Confusion matrix:
#   No Yes class.error
# No  14549 373  0.02499665
# Yes  2408 427  0.84938272
# 해석: 예측 변수 21개 가운데, 노드 분할을 위해 선택된 4개의 IV(예측변수)
# 보통 예측 변수 개수에 루트를 씌운 값을, IV예측변수) 개수로 선택한다. Root 21 is equal to 4.582
# ntree, mtry 라는 parameter로 내가 직접 정해줄 수도 있다.


# 랜덤포레스트는 bootstrapping에 의해 반복 추출된 표본을 바탕으로, 의사결정 나무를 생성.
# bootstrapping은 복원추출 방식으로 표본에 포함된 케이스들을 선택 >> 일부 케이스들은 모델을 생성할 때 사용하는 부분에 포함되지 않을 수도 있다.
# 예측 모델을 구축할 때 사용하지 않은 케이스들을 OOB(out of bag) 데이터라고 한다.
# OOB 데이터는 예측 모델 구축 시, 사용되지 않은 데이터 >> OOB 데이터를 대상으로 분류작업을 수행하면 테스트 데이터를 이용하여 분류 작업을 하는 것과 비슷한 효과를 낼 수 있다.
# OOB data를 통해 분류 작업을 수행, 얻어진 잘못 분류된 비율: OOB estimate error라고 한다. 
# OOB estimate of  error rate: 14.98% >> test data를 통해 얻어진 error와 비슷하다고 보면 된다.
# 별도의 test data가 없어도 이걸로 설명해도 괜찮다.

# type = "prob">> 예측 결과를 확률로 보여줌
diabetes.forest.pred <- predict(diabetes.forest, newdata = diabetes.test,
                          type = "prob")
head(diabetes.forest.pred)

# type = "response">> 예측 결과를 예측 범주/집단으로 보여줌
diabetes.forest.pred <- predict(diabetes.forest, newdata = diabetes.test,
                          type = "response")
head(diabetes.forest.pred)

table(diabetes.test$Diabetes_status, diabetes.forest.pred,
      dnn = c("Actual", "Predicted"))
mean(diabetes.test$Diabetes_status == diabetes.forest.pred, na.rm = TRUE) # 0.848246
# 랜덤포레스트에서 결측값이 포함된 케이스는 분류대상에서 제외한다.
#           Predicted
# Actual     No  Yes
#      No  6268  147
#      Yes 1008  188
# 그래서 test data는 총 개인데, 개 만 대상이 되어있는 것이다.

library(cluster)
clusplot(x=na.omit(diabetes.test[, -1]), clus = na.omit(diabetes.forest.pred),
         color=TRUE, shade=TRUE, labels = 4, lines = 0,
         main = "Random Forest Classification from Diabetes Dataset")

diabetes.forest.predAll <- predict(diabetes.forest, newdata = diabetes.test,
                             predict.all = TRUE)  # predict.all: 개별 케이스들이 각 의사결정 나무에 의해
# 어떻게 분류 되는지 결과의 분포를 확인할 수 있다. 
str(diabetes.forest.predAll)

# $ aggregate : [1:210] 총 테스트 데이터는 210(예측 결과가 210)
# $ individual: [1:204, 1:500] 결측치 빼고 204(판정 결과가 204, 6개는 결측치로 처리), 의사결정 나무는 500개

diabetes.forest.predAll$individual[6, ] # 예를 들어 6번째 인덱스를 확인하는건데,
table(diabetes.forest.predAll$individual[6, ]) # 500개의 나무를 확인한다.
# benign  malignant                       # 500개가 benign를 좀 더 많다고 예측했으니까, benign일 것이다.
# 274       226 
# 확인 해보자 
na.omit(diabetes.forest.predAll$aggregate)[6] # benign 

apply(diabetes.forest.predAll$individual[21:25, ], 1, table)
#            53  55  57  61  63
# benign     50   4 105 140  90
# malignant 450 496 395 360 410
# 이거 겠네 mal mal mal mal mal
# 확인 해보자
na.omit(diabetes.forest.predAll$aggregate)[21:25]
#      53        55        57        61        63 
# malignant malignant malignant malignant malignant 

# IV(예측 변수)들이 모델에 기여하는 정도를 알 수 있다는 것이,
# 랜덤 포레스트의 장점이다.
# 일부 IV만 선택되어 노드 분할에 사용되기 때문에, IV 선택이 예측/성능에 영향을 미칠 수 있다ㅏ.
# 어떤 변수가 중요한지 평가할 수 있다.
# 지표를 얻기 위해 예측 모델을 생성할 때, inportance = TRUE 지정하면 된다.
# 두 가지 방법이 있는데,
# (1) varImpPlot() 함수
varImpPlot(diabetes.forest, pch=21, color="black", bg="red",
           pt.cex = 1.2,
           main = "Variable Importance for Diabetes Classification")
# Based on 지니계수, 선택된 parameters.
# GenHlth + BMI + Age + Income + DiffWalk + HighChol + PhysHlth + HighBP + HeartDiseaseorAttack + Stroke + Education + MentHlth + Smoker
# (2) importance() 함수로 평가 지표 테이블로 확인한다.
importance(diabetes.forest)
importance(diabetes.forest, type = 1)
importance(diabetes.forest, type = 2)

# install.packages("gplots")
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(diabetes.forest)[, 1:2]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace = "none", key = FALSE,
          margins = c(10, 7), cexRow=1.5, cexCol=1.2, colRow = c("green4", "maroon"),
          main = "Variable Importance\n for Diabetes Classification")
####################################################
#############  Parameters 바꿔서 다시하기 ##########
####################################################
# install.packages("randomForest")
library(randomForest)
set.seed(123)
diabetes.forest <- randomForest(Diabetes_status ~ HighBP + HighChol +
                                  BMI + Sex + Age + HeartDiseaseorAttack +
                                  PhysActivity + Smoker + Stroke + Income
                                , data = diabetes.train,
                                na.action = na.roughfix, 
                                importance=TRUE) 
diabetes.forest

# type = "prob">> 예측 결과를 확률로 보여줌
diabetes.forest.pred <- predict(diabetes.forest, newdata = diabetes.test,
                                type = "prob")
head(diabetes.forest.pred)

# type = "response">> 예측 결과를 예측 범주/집단으로 보여줌
diabetes.forest.pred <- predict(diabetes.forest, newdata = diabetes.test,
                                type = "response")
head(diabetes.forest.pred)

table(diabetes.test$Diabetes_status, diabetes.forest.pred,
      dnn = c("Actual", "Predicted"))
mean(diabetes.test$Diabetes_status == diabetes.forest.pred, na.rm = TRUE) #  0.8478518

library(cluster)
clusplot(x=na.omit(diabetes.test[, -1]), clus = na.omit(diabetes.forest.pred),
         color=TRUE, shade=TRUE, labels = 4, lines = 0,
         main = "Random Forest Classification from Diabetes Dataset")

diabetes.forest.predAll <- predict(diabetes.forest, newdata = diabetes.test,
                                   predict.all = TRUE)  # predict.all: 개별 
str(diabetes.forest.predAll)

diabetes.forest.predAll$individual[6, ] # 예를 들어 6번째 인덱스를 확인하는건데,
table(diabetes.forest.predAll$individual[6, ]) # 500개의 나무를 확인한다.
na.omit(diabetes.forest.predAll$aggregate)[6] # benign 
apply(diabetes.forest.predAll$individual[21:25, ], 1, table)
na.omit(diabetes.forest.predAll$aggregate)[21:25]

# (1) varImpPlot() 함수
varImpPlot(diabetes.forest, pch=21, color="black", bg="red",
           pt.cex = 1.2,
           main = "Variable Importance for Diabetes Classification")

# (2) importance() 함수로 평가 지표 테이블로 확인한다.
importance(diabetes.forest)
importance(diabetes.forest, type = 1)
importance(diabetes.forest, type = 2)

# install.packages("gplots")
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(diabetes.forest)[, 1:2]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace = "none", key = FALSE,
          margins = c(10, 7), cexRow=1.5, cexCol=1.2, colRow = c("green4", "maroon"),
          main = "Variable Importance\n for Diabetes Classification")


####################################################
#############  아래것들은 뭐였더라...???  ##########
####################################################
# data <- cbind(lapply(data.entry[-10], function(x) as.numeric(as.character(x))), data[10]) # 10번 col 빼고 as.numeric 돌림

# # install.packages("randomForest")
# library(randomForest)
# set.seed(123)
# diabetes.forest <- randomForest(Diabetes_status ~ ., data = diabetes.train,
#                           na.action = na.roughfix, # na.action: 결측값 처리 방법 지정.
#                           # na.roughfix: 결측값이 숫자라면, 해당 열의 중위수. 명목형 변수라면, 최빈도 범주값.
#                           importance=TRUE) # 중요한 변수를 *로 알려줌
# 
# 
# diabetes.forest
# 
# # type = "prob">> 예측 결과를 확률로 보여줌
# diabetes.forest.pred <- predict(diabetes.forest, newdata = diabetes.test,
#                           type = "prob")
# head(diabetes.forest.pred)
# 
# # type = "response">> 예측 결과를 예측 범주/집단으로 보여줌
# diabetes.forest.pred <- predict(diabetes.forest, newdata = diabetes.test,
#                           type = "response")
# head(diabetes.forest.pred)
# 
# table(diabetes.test$Diabetes_status, diabetes.forest.pred,
#       dnn = c("Actual", "Predicted"))
# mean(diabetes.test$Diabetes_status == diabetes.forest.pred, na.rm = TRUE) # 84.41%
# # 랜덤포레스트에서 결측값이 포함된 케이스는 분류대상에서 제외한다.
# 
# ## Plot 그리기
# library(cluster)
# clusplot(x=na.omit(diabetes.test), clus = na.omit(diabetes.forest.pred),
#          color=TRUE, shade=TRUE, labels = 4, lines = 0,
#          main = "Random Forest Classification from Diabetes Dataset")
# 
# # predict.all: 개별 케이스들이 각 의사결정 나무에 의해
# # 어떻게 분류 되는지 결과의 분포를 확인할 수 있다. 
# diabetes.forest.predAll <- predict(diabetes.forest, newdata = diabetes.test,
#                              predict.all = TRUE)  
# str(diabetes.forest.predAll)
# # $ aggregate : [1:3806] 총 테스트 데이터는 3806(예측 결과가 3806)
# # $ individual: [1:3806, 1:500] 결측치 빼고 3806(판정 결과가 3806, 결측치는 없음), 의사결정 나무는 500개
# 
# # 예를 들어 6번째 인덱스를 확인하는건데,
# diabetes.forest.predAll$individual[6, ] # 500개의 나무를 확인한다.
# table(diabetes.forest.predAll$individual[6, ]) # 500개의 나무를 확인한다.
# #   0   1                       # 500개가 0을(당뇨 아님) 좀 더 많다고 예측했으니까, 0일 것이다.
# # 497   3 
# # 확인 해보자 
# na.omit(diabetes.forest.predAll$aggregate)[6] # 0  
# 
# apply(diabetes.forest.predAll$individual[21:25, ], 1, table)
# na.omit(diabetes.forest.predAll$aggregate)[21:25]
# 
# # IV(예측 변수)들이 모델에 기여하는 정도를 알 수 있다는 것이,
# # 랜덤 포레스트의 장점이다.
# # 일부 IV만 선택되어 노드 분할에 사용되기 때문에, IV 선택이 예측/성능에 영향을 미칠 수 있다ㅏ.
# # 어떤 변수가 중요한지 평가할 수 있다.
# # 지표를 얻기 위해 예측 모델을 생성할 때, inportance = TRUE 지정하면 된다.
# # 두 가지 방법이 있는데,
# # (1) varImpPlot() 함수
# varImpPlot(diabetes.forest, pch=21, color="black", bg="red",
#            pt.cex = 1.2,
#            main = "Variable Importance for Diabetes Classification")
# # (2) importance() 함수로 평가 지표 테이블로 확인한다.
# importance(diabetes.forest)
# importance(diabetes.forest, type = 1)
# importance(diabetes.forest, type = 2)
# 
# 
# # install.packages("gplots")
# library(gplots)
# library(RColorBrewer)
# heatmap.2(t(importance(diabetes.forest)[, 1:2]),
#           col=brewer.pal(9, "Blues"),
#           dend="none", trace = "none", key = FALSE,
#           margins = c(10, 7), cexRow=1.5, cexCol=1.2, colRow = c("green4", "maroon"),
#           main = "Variable Importance\n for Diabetes Classification")
# 
# 
# 
# 
# ############################################
# diabetes.forest_para <- randomForest(Diabetes_status ~ 
#                                   GenHlth +HighBP +  BMI + Age + Income + PhysHlth + Education +
#                                   CholCheck +HighChol + HeartDiseaseorAttack +                       
#                                   GenHlth + DiffWalk + MentHlth + Stroke, 
#                                 data = diabetes.train,
#                                 na.action = na.roughfix, # na.action: 결측값 처리 방법 지정.
#                                 # na.roughfix: 결측값이 숫자라면, 해당 열의 중위수. 명목형 변수라면, 최빈도 범주값.
#                                 importance=TRUE) # 중요한 변수를 *로 알려줌
# 
# diabetes.forest_para.pred <- predict(diabetes.forest_para, newdata = diabetes.test,
#                                 type = "prob")
# head(diabetes.forest_para.pred)
# 
# # type = "response">> 예측 결과를 예측 범주/집단으로 보여줌
# diabetes.forest_para.pred <- predict(diabetes.forest_para, newdata = diabetes.test,
#                                 type = "response")
# head(diabetes.forest_para.pred)
# 
# table(diabetes.test$Diabetes_status, diabetes.forest_para.pred,
#       dnn = c("Actual", "Predicted"))
# mean(diabetes.test$Diabetes_status == diabetes.forest_para.pred, na.rm = TRUE) # 84.65%
# # 랜덤포레스트에서 결측값이 포함된 케이스는 분류대상에서 제외한다.
# 
# # (1) varImpPlot() 함수
# varImpPlot(diabetes.forest_para, pch=21, color="black", bg="red",
#            pt.cex = 1.2,
#            main = "Variable Importance for Diabetes Classification")
# # (2) importance() 함수로 평가 지표 테이블로 확인한다.
# importance(diabetes.forest_para)
# importance(diabetes.forest_para, type = 1)
# importance(diabetes.forest_para, type = 2)
# 
# 
# # install.packages("gplots")
# library(gplots)
# library(RColorBrewer)
# heatmap.2(t(importance(diabetes.forest_para)[, 1:2]),
#           col=brewer.pal(9, "Blues"),
#           dend="none", trace = "none", key = FALSE,
#           margins = c(10, 7), cexRow=1.5, cexCol=1.2, colRow = c("green4", "maroon"),
#           main = "Variable Importance\n for Diabetes Classification")
# 
# 
# 
nrow(diabetes.test) # 7611 sample