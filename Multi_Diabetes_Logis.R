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

# set the labels with names easy catching 
Data$Diabetes_012 <- factor(Data$Diabetes_012, 
                               levels = c("0","1","2"), 
                               labels = c("No", "Type1", "Type2"))
Data$Sex <- factor(Data$Sex, 
                   levels = c("0","1"), 
                   labels = c("Male", "Female"))

# (1)EDA
## Status vs Gender
table(Data$Diabetes_012, Data$Sex)
## Status vs Age
boxplot(Age~Diabetes_012,data=Data, main="Status vs Age",
        xlab="Diabetes_status", ylab="Age")
library(ggplot2)
ggplot(Data, aes(x = Age, y = BMI, color = factor(Diabetes_012))) +
  geom_point(aes(shape = factor(Diabetes_012)))

# (2)Pre-processing
str(Data)
# start with Y(predicted/independent variable), as.factor() for binary variables
# Data$Diabetes_status <- as.factor(Data$Diabetes_status)
# binary types below
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
# Now, most of the variables are categorical but BMI, MentHlth and PhysHlth. 
str(Data)

# Split data into training data / test data 
set.seed(123)
train <- sample(nrow(Data), 0.7*nrow(Data))
diabetes.train <- Data[train, ]
diabetes.test <- Data[-train, ]

nrow(diabetes.train) # 17757 sample
nrow(diabetes.test) # 7611 sample

##############################################################
#########                   방법 1)                   ########
##############################################################
library(VGAM)

diabetes.mlogit_1 <- vglm(Diabetes_012 ~ ., family=multinomial(), 
                   data=diabetes.train)
# <g-1>개의 다항 로지스틱 회귀모델이 생성 >> 2개의 회귀 모델이 생성된다.
summary(diabetes.mlogit_1)
# 각 2개 씩의 회귀 계수가 나타난다.
# (1) ln{P(Diabetes=0)/P(Diabetes=2)} 
# (2) ln{P(Diabetes=1)/P(Diabetes=2)} 
# Call:
#   vglm(formula = Diabetes_012 ~ ., family = multinomial(), data = diabetes.train)
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Names of linear predictors: log(mu[,1]/mu[,3]), log(mu[,2]/mu[,3])
# Residual deviance: 14063.93 on 35422 degrees of freedom
# Log-likelihood: -7031.967 on 35422 degrees of freedom
# Number of Fisher scoring iterations: 17 
# Warning: Hauck-Donner effect detected in the following estimate(s):
#   '(Intercept):2'
# Reference group is level  3  of the response
coef(diabetes.mlogit.all)
# 오즈비 확인을 위해 지수함수 값으로 변환한다.
exp(coef(diabetes.mlogit.all))

# 예측
diabetes.mlogit.all.pred <- fitted(diabetes.mlogit.all)
head(diabetes.mlogit.all.pred)
tail(diabetes.mlogit.all.pred)

# 혼동 행렬을 생성하기 위한 작업
# 더 이상 예측 확률이 아닌, 예측 범주로 저장을 하게 된다.
diabetes.mlogit.all.pred <- colnames(diabetes.mlogit.all.pred)[max.col(diabetes.mlogit.all.pred)]
head(diabetes.mlogit.all.pred)
# 혼동 행렬 생성
table(diabetes.train$Diabetes_012, diabetes.mlogit.all.pred,
      dnn = c("Actual", "Predicted"))
# Accuracy
mean(diabetes.train$Diabetes_012 == diabetes.mlogit.all.pred) # 84.93%

##############################################################
diabetes.mlogit2 <- vglm(Diabetes_012 ~ HighBP + HighChol + CholCheck + BMI +
                           Stroke + HeartDiseaseorAttack + HvyAlcoholConsump +
                           AnyHealthcare + GenHlth + DiffWalk + Sex + Age +
                           Education + Income, family=multinomial(), 
                           data=diabetes.train)
# <g-1>개의 다항 로지스틱 회귀모델이 생성 >> 2개의 회귀 모델이 생성된다.
summary(diabetes.mlogit2)
# 각 2개 씩의 회귀 계수가 나타난다.
# (1) ln{P(Diabetes=0)/P(Diabetes=2)} 
# (2) ln{P(Diabetes=1)/P(Diabetes=2)} 
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Names of linear predictors: log(mu[,1]/mu[,3]), log(mu[,2]/mu[,3])
# Residual deviance: 14075.22 on 35436 degrees of freedom
# Log-likelihood: -7037.612 on 35436 degrees of freedom
# Number of Fisher scoring iterations: 17 
# Warning: Hauck-Donner effect detected in the following estimate(s):
#   '(Intercept):2'
# Reference group is level  3  of the response
coef(diabetes.mlogit2)
# 오즈비 확인을 위해 지수함수 값으로 변환한다.
exp(coef(diabetes.mlogit2))

# 예측
diabetes.mlogit2.pred <- fitted(diabetes.mlogit2)
head(diabetes.mlogit2.pred)
tail(diabetes.mlogit2.pred)

# 혼동 행렬을 생성하기 위한 작업
# 더 이상 예측 확률이 아닌, 예측 범주로 저장을 하게 된다.
diabetes.mlogit2.pred <- colnames(diabetes.mlogit2.pred)[max.col(diabetes.mlogit2.pred)]
head(diabetes.mlogit2.pred)
# 혼동 행렬 생성
table(diabetes.train$Diabetes_012, diabetes.mlogit2.pred,
      dnn = c("Actual", "Predicted"))
# Accuracy
mean(diabetes.train$Diabetes_012 == diabetes.mlogit2.pred) # 84.85%



##############################################################
#########                   방법 2)                   ########
##############################################################
# 모델 생성 및 평가
library(nnet)
# 하.... 이거 너무 오래 걸린다...ㅠㅠ 이건 iter도 100번 한다... 모델 용량도 너무 크다...ㅠㅠ
set.seed(123)
sample <- sample(nrow(Data), 0.01*nrow(Data))
Data <- Data[sample, ]
set.seed(123)
train <- sample(nrow(Data), 0.7*nrow(Data))
diabetes.train <- Data[train, ]
diabetes.test <- Data[-train, ]

diabetes.mlogit_2 <- multinom(Diabetes_012 ~ ., data = diabetes.train)
# multinom()는 첫번째 범주(WinF)를 기준범주로 사용. 
# 그리고 유의 확률을 제공하지 않음 >> 표준 오차로 나눠 z값을 구할 수 있고, 
# z값으로 유의확률을 산출 할 수 있다.
summary(diabetes.mlogit_2)

z <- summary(diabetes.mlogit_2)$coefficients / summary(diabetes.mlogit_2)$standard.errors
# pnorm() >> 양쪽 꼬리부분의 면적을 계산
p <- (1 - pnorm(abs(z), 0, 1)) * 2
print(p, digits = 3)


# 추정 모델의 예측 확률 확인
diabetes.mlogit_2.pred <- predict(diabetes.mlogit_2, newdata=diabetes.test,
                           type="probs") # 회귀모델에 의해 추정된 확률을 얻는다.
head(diabetes.mlogit_2.pred)
# 범주간 관계를 살펴보자
cbind(round(diabetes.mlogit_2.pred, 3), diabetes.test["Diabetes_012"])

# 가장 높은 확률을 갖는 열의 인덱스를 추출한다.
max.col(diabetes.mlogit_2.pred)
# 혼동 행렬을 생성하기 위한 작업
# 더 이상 예측 확률이 아닌, 예측 범주로 저장을 하게 된다.
diabetes.mlogit_2.pred <- colnames(diabetes.mlogit_2.pred)[max.col(diabetes.mlogit_2.pred)]
head(diabetes.mlogit_2.pred)
# 혼동 행렬 생성
table(diabetes.test$Diabetes_012, diabetes.mlogit_2.pred,
      dnn = c("Actual", "Predicted"))
# 근데 출력 순서가 맞지 않으니, 이렇게 맞춰준다
# 다시 한다.
table(diabetes.test$Diabetes_012, 
      factor(diabetes.mlogit_2.pred,
             levels = levels(diabetes.test$Diabetes_012),
             labels = levels(diabetes.test$Diabetes_012)),
      dnn = c("Actual", "Predicted"))

# 예측 정확도 계산
mean(diabetes.test$Diabetes_012 == diabetes.mlogit_2.pred) # 84.72%


##############################################################
#########           서포트 벡터 머선 일이고...        ########
##############################################################
# library(e1071) # svm 함수가 적재된 라이브러리
# str(diabetes.train$Diabetes_012)
# prop.table(table(diabetes.train$Diabetes_012))
# 
# # 두 테이블의 비율이 비슷하게 잘 나뉜 것을 볼 수 있다.
# prop.table(table(diabetes.train$Diabetes_012))
# prop.table(table(diabetes.test$Diabetes_012))
# 
# # 모델 돌리기
# set.seed(123)
# diabetes.svm <- svm(Diabetes_012 ~ ., data = diabetes.train)
# summary(diabetes.svm) # Number of Support Vectors:  3288 (1902 1239 147 )
# 
# diabetes.svm.pred <- predict(diabetes.svm, newdata = diabetes.test)
# head(diabetes.svm.pred)
# table(diabetes.test$Diabetes_012, diabetes.svm.pred,
#       dnn=c("Actual", "Predicted"))
# mean(diabetes.test$Diabetes_012 == diabetes.svm.pred) # 0.7403315 예측 정확도
























