setwd("D:/OneDrive/Documents/DM&ML/TABA/Datasets/")
# NULL은 초기화되지 않은 값.
# NA은 아직 모른다(Not Available): 할당하지 않은 값, 결측치.

train <- read.csv("Airline_train.csv", header=T, na.strings=c(""), stringsAsFactors = T)
# 결측치 처리
print(length(train[is.na(train)])) # 310
sum(!complete.cases(train)) # 310
train<- train[complete.cases(train), ]
# 샘플링
sample <- sample(nrow(train), 0.05*nrow(train))
train <- train[sample, ]

test <- read.csv("Airline_test.csv", header=T, na.strings=c(""), stringsAsFactors = T)
# 결측치 처리
print(length(test[is.na(test)])) # 83
sum(!complete.cases(test)) # 83
test<- test[complete.cases(test), ]
# 샘플링
sample <- sample(nrow(test), 0.05*nrow(test))
test <- test[sample, ]

nrow(train)
nrow(test)

# Data$Diabetes_012[Data$Diabetes_012 > 0] <- 1
str(train)
str(test)

airline.train <- train[-1, -2] # 아이디 열 삭제
airline.test <- test[-1, -2]   # 아이디 열 삭제

str(airline.train)
str(airline.test)


#######################################
#########    Random Forest    #########
#######################################

# install.packages("randomForest")
library(randomForest)
set.seed(123)
airline.forest <- randomForest(satisfaction ~ ., data = airline.train,
                               na.action = na.roughfix, # na.action: 결측값 처리 방법 지정.
                               # na.roughfix: 결측값이 숫자라면, 해당 열의 중위수. 명목형 변수라면, 최빈도 범주값.
                               importance=TRUE) # 중요한 변수를 *로 알려줌
# No. of variables tried at each split: 3
# 해석: 예측 변수 9개 가운데, 노드 분할을 위해 선택된 3개의 IV(예측변수)
# 보통 예측 변수 개수에 루트를 씌운 값을, IV예측변수) 개수로 선택한다.
# ntree, mtry 라는 parameter로 내가 직접 정해줄 수도 있다.
airline.forest

# type = "prob">> 예측 결과를 확률로 보여줌
airline.forest.pred <- predict(airline.forest, newdata = airline.test,
                               type = "prob")
head(airline.forest.pred)

# type = "response">> 예측 결과를 예측 범주/집단으로 보여줌
airline.forest.pred <- predict(airline.forest, newdata = airline.test,
                               type = "response")
head(airline.forest.pred)


table(airline.test$satisfaction, airline.forest.pred,
      dnn = c("Actual", "Predicted"))
mean(airline.test$satisfaction == airline.forest.pred, na.rm = TRUE) # 0.9466357
# 랜덤포레스트에서 결측값이 포함된 케이스는 분류대상에서 제외한다.


library(cluster)
clusplot(x=na.omit(airline.test[, -24]), clus = na.omit(airline.forest.pred),
         color=TRUE, shade=TRUE, labels = 4, lines = 0,
         main = "Random Forest Classification from Airline satisfaction Dataset")

airline.forest.predAll <- predict(airline.forest, newdata = airline.test,
                                  predict.all = TRUE)  # predict.all: 개별 케이스들이 각 의사결정 나무에 의해
# 어떻게 분류 되는지 결과의 분포를 확인할 수 있다. 
str(airline.forest.predAll)
# aggregate : [1:1293] 총 테스트 데이터는 1293
# individual: [1:1293, 1:500] 1293 == 1293 >> 결측치 없음. 의사결정 나무는 500개.

airline.forest.predAll$individual[6, ] # 예를 들어 6번째 인덱스를 확인하는건데,
table(airline.forest.predAll$individual[6, ]) # 500개의 나무를 확인한다.
# 500개 중 대다수의 나무가 unstisfaction이라고 하니까, neutral or dissatisfied이다.
# 확인 해보자 
na.omit(airline.forest.predAll$aggregate)[6] # neutral or dissatisfied 

apply(airline.forest.predAll$individual[21:25, ], 1, table)
# 확인 해보자 
na.omit(airline.forest.predAll$aggregate)[21:25]


# IV(예측 변수)들이 모델에 기여하는 정도를 알 수 있다는 것이,
# 랜덤 포레스트의 장점이다.
# 일부 IV만 선택되어 노드 분할에 사용되기 때문에, IV 선택이 예측/성능에 영향을 미칠 수 있다.
# 어떤 변수가 중요한지 평가할 수 있다.
# 지표를 얻기 위해 예측 모델을 생성할 때, importance = TRUE 지정하면 된다.
# 두 가지 방법이 있는데,
# (1) varImpPlot() 함수
varImpPlot(airline.forest, pch=21, color="black", bg="red",
           pt.cex = 1.2,
           main = "Variable Importance for Airline Satisfaction Classification")

# (2) importance() 함수로 평가 지표 테이블로 확인한다.
importance(airline.forest)
importance(airline.forest, type = 1)
importance(airline.forest, type = 2)
# MeanDecreaseAccuracy와 지니 계수 중 뭘 기준으로 좋은 parameter를 선택하는건지 찾아봐야 함.

# install.packages("gplots")
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(airline.forest)[, 1:2]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace = "none", key = FALSE,
          margins = c(10, 7), cexRow=1.5, cexCol=1.2, colRow = c("green4", "maroon"),
          main = "Variable Importance\n for Airline Satisfaction Classification")