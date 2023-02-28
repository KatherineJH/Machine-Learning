##############################
####### 로지스틱 회귀 ########
##############################
# 선형회귀에서는 y가 무조건 continuous한 numerical data type.
# 반면 generalized 선형회귀 모델에서는 x와y가 선형관계가 아니거나, y가 numerical이 아닌 경우에 쓸 수 있다. (discrete numerical에서도 가능)
# y를 설명하는 확률분포가 있고, 여전히 선형모델의 특징을 갖는데, y가 아닌 에타라는 공식을 가짐. 공식에 E는포함되지 않는다.그리고 에타는 link function에 의해 결정된다.
# 로지스틱 회귀는 이 glm(generalized linear model) 중 하나이다. y는 binary, x는 numerical 또는 categorical.
# y가 0/1일 때, Bernoulli distribution을 따른다고 가정한다.
# 여기선 link function 대신 ligit function을 사용한다.
# Odds: quantify the probablity of an event하는 방법이다. 주로 겜블링과 로지스틱 회귀에서 사용. 
# 오즈 구한다 > 오즈비 구한다 > logit 한다 > 정규분포를 따른다.

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
Data$Sex <- factor(Data$Sex, 
                            levels = c("0","1"), 
                            labels = c("Male", "Female"))
table(Data$Diabetes_status)

# (1)EDA
## Status vs Gender
table(Data$Diabetes_status, Data$Sex) # 셩별에 의한 차이는 딱히 보이지 않음
## Status vs Age
boxplot(Age~Diabetes_status,data=Data, main="Status vs Age",
        xlab="Diabetes_status", ylab="Age")
# according to the boxplot, there is slightly. will take it as a model parameter.

## relationship between age and BMI by status(0/1)
# install.packages("scatterplot3d")
# library(scatterplot3d)
# grps <- as.factor(Data$Diabetes_status)
# colors <- c("#56B4E9", "#E69F00")
# scatterplot3d(Data$BMI, Data$Age, Data$GenHlth, pch = 16,
#             color = colors[grps],
#             grid = TRUE, box = FALSE)
library(ggplot2)
ggplot(Data, aes(x = Age, y = BMI, color = factor(Diabetes_status))) +
  geom_point(aes(shape = factor(Diabetes_status)))
# focusing on blue colors, higher age/higher BMI -> more result in 1(positive diabetes)

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

nrow(diabetes.train)
nrow(diabetes.test)

str(diabetes.train)
str(diabetes.test)

table(diabetes.train$Diabetes_status)
sum(table(diabetes.train$Diabetes_status)) # 20294 sample
table(diabetes.test$Diabetes_status)
sum(table(diabetes.test$Diabetes_status))  #  5074 sample

# Both samples have similar amount of positives and negatives
prop.table(table(diabetes.train$Diabetes_status)) # probablity of positive in train data: 15.81%
prop.table(table(diabetes.test$Diabetes_status)) # probablity of positive in train data: 15.60%

#(3) Modeling 
## with all 'x' parameters.
diabetes.logit <- glm(Diabetes_status ~ ., data = diabetes.train, 
                      family = binomial(link = "logit")) # binominal regression + assigned a link function
# Interpretation
summary(diabetes.logit)
# Deviance Residuals: 
#   Min       1Q     Median     3Q      Max  
# -2.4073  -0.5565  -0.3265  -0.1706   3.2382  
# AIC: 11979
# Significant variables: HighBP, HighChol, CholCheck, BMI, Stroke, HeartDiseaseorAttack, HvyAlcoholConsump, GenHlth, PhysHlth, DiffWalk, Sex, Age, Income
par(mfrow=c(2, 2))
plot(diabetes.logit)

# For example of BMI
# BMI 6.777e-02  3.509e-03  19.316  < 2e-16 ***
### intercept: The log odds of diabetes for control group with an BMI and the others of 0. From this we can calculate the odds or probability, but additional calculations are necessary.
### slope: For a unit increase in BMI(being 1 BMI greater) how much will the log odds ratio change, not particularly intuitive.

# Odds: log odds -> exp() -> interpret
exp(coef(diabetes.logit))
# increase BMI 1 >> increase 1.057004341 times
1.057004341 ^ 2 # increase BMI 2  >> increase 1.117258 times
1.057004341 ^ 3 # increase BMI 3  >> increase 1.180947 times
1.057004341 ^ 4 # increase BMI 4  >> increase 1.248266 times


## check a stepwise function
step(diabetes.logit)
# Call:  glm(formula = Diabetes_status ~ HighBP + HighChol + CholCheck + 
#              BMI + HeartDiseaseorAttack + PhysActivity + HvyAlcoholConsump + 
#              GenHlth + PhysHlth + DiffWalk + Sex + Age + Income, family = binomial(link = "logit"), 
#            data = diabetes.train)
# Null Deviance: 14980 
# Residual Deviance: 11710 	
# AIC: 11770

# (4)Modifying the model, removing parameters that are not significant
## (4-1) with parameters which are calculated through step function 
diabetes.logit.step <- glm(HighBP + HighChol + CholCheck + BMI +
                            HeartDiseaseorAttack + PhysActivity +
                            HvyAlcoholConsump +GenHlth + PhysHlth + DiffWalk +
                            Sex + Age + Income, 
                           data = diabetes.train, 
                           family = binomial(link = "logit")) # 링크함수 지정
summary(diabetes.logit.step)
# AIC: 10292

## (4-2) with parameters based on significant level(t-value, p-value) on the very first model with all parameters.
diabetes.logit.manual <- glm(Diabetes_status ~ HighBP + HighChol + CholCheck +
                              BMI + HeartDiseaseorAttack + HvyAlcoholConsump +
                              GenHlth + PhysHlth + DiffWalk + Sex + Age +
                              Income, data = diabetes.train, 
                              family = binomial(link = "logit"))
summary(diabetes.logit.manual)
# AIC: 11774

# (4) Glm's statistical significance test(Null deviance/degrees of freedom)
# Strictly speaking, the larger the deviation, the worse the fit of the model(The less deviations, the better).
# It can be tested whether the degree of deviation decreases as much as the degree of freedom decreases.
# df of Null deviance = number of observed values of null deviance - number of constant terms to be estimated
# df of Residual deviance = number of observed values of Residual deviance - number of constant terms to be estimated
# Deviance follows a chi-square distribution.
pchisq(q=diabetes.logit.step$null.deviance-diabetes.logit.step$deviance, 
       df=diabetes.logit.step$df.null-diabetes.logit.step$df.residual,
       lower.tail = FALSE)
## nearly 0 means significance.

pchisq(q=diabetes.logit.manual$null.deviance-diabetes.logit.manual$deviance, 
       df=diabetes.logit.manual$df.null-diabetes.logit.manual$df.residual,
       lower.tail = FALSE)
## nearly 0 means significance.

# (5) Predict with all parameters model & diabetes.logit.step & diabetes.logit.manual
# I guess the last two models will be similar.
# Probability of diabetes 'yes(positive)' in the test data 
## (5-0)all parameters(predictors) model
# diabetes.logit.pred <- predict(diabetes.logit, newdata = diabetes.test,
#                                     type = "response") # type = "response >> It outputs the probability of occurrence of an event that is easier to interpret than log odds.
# # mean(diabetes.logit.step.pred)
# # head(diabetes.logit.step.pred)
# diabetes.logit.pred <- factor(diabetes.logit.pred > 0.5,
#                                    levels = c(FALSE, TRUE),
#                                    labels = c("No", "Yes"))
# # head(diabetes.logit.pred)
# # table(diabetes.logit.pred)
# table(diabetes.test$Diabetes_status, diabetes.logit.pred,
#       dnn=c("Actual", "Predicted"))
# mean(diabetes.test$Diabetes_status == diabetes.logit.pred) # Predict Accuracy >> 85.21%

## (5-1)Stepwise model
diabetes.logit.step.pred <- predict(diabetes.logit.step, newdata = diabetes.test,
                            type = "response") # type = "response >> It outputs the probability of occurrence of an event that is easier to interpret than log odds.
# mean(diabetes.logit.step.pred)
# head(diabetes.logit.step.pred)
diabetes.logit.step.pred <- factor(diabetes.logit.step.pred > 0.5,
                              levels = c(FALSE, TRUE),
                              labels = c("No", "Yes")
                              )
# head(diabetes.logit.pred)
# table(diabetes.logit.pred)
table(diabetes.test$Diabetes_status, diabetes.logit.step.pred,
      dnn=c("Actual", "Predicted"))
mean(diabetes.test$Diabetes_status == diabetes.logit.step.pred) # Predict Accuracy >> 85.24%

## (5-2)Manual model
diabetes.logit.manual.pred <- predict(diabetes.logit.manual, newdata = diabetes.test,
                               type = "response") # type = "response >> It outputs the probability of occurrence of an event that is easier to interpret than log odds.
# mean(diabetes.logit.manual.pred)
# head(diabetes.logit.manual.pred)
diabetes.logit.manual.pred <- factor(diabetes.logit.manual.pred > 0.5,
                              levels = c(FALSE, TRUE),
                              labels = c("No", "Yes")
)
table(diabetes.test$Diabetes_status, diabetes.logit.manual.pred,
      dnn=c("Actual", "Predicted"))
mean(diabetes.test$Diabetes_status == diabetes.logit.manual.pred) # Predict Accuracy >> 85.36%

# The last two models show the same results for AIC, model significance test, classification, and Predict Accuracy. Any of two can be used between diabetes.logit.step.pred & diabetes.logit.manual.pred

# (6) cross validation
set.seed(123)
train <- sample(nrow(Data), 0.8*nrow(Data))
diabetes.train <- Data[train, ]
diabetes.test <- Data[-train, ]

# (6-1) stepwise
diabetes.logit.step <- glm(Diabetes_status ~ HighBP + HighChol + CholCheck +
                             BMI + Stroke + HeartDiseaseorAttack + Veggies + HvyAlcoholConsump +
                             NoDocbcCost + GenHlth + MentHlth + Sex + Age + Education +
                             Income, data = diabetes.train, 
                           family = binomial(link = "logit")) # 링크함수 지정
summary(diabetes.logit.step) # AIC: 10292
diabetes.logit.step.pred <- predict(diabetes.logit.step, newdata = diabetes.test,
                                    type = "response") 
diabetes.logit.step.pred <- factor(diabetes.logit.step.pred > 0.5,
                                   levels = c(FALSE, TRUE),
                                   labels = c("No", "Yes"))
table(diabetes.test$Diabetes_status, diabetes.logit.step.pred,
      dnn=c("Actual", "Predicted"))
mean(diabetes.test$Diabetes_status == diabetes.logit.step.pred) # Predict Accuracy >> 85.26%

# (6-2) manual
diabetes.logit.manual <- glm(Diabetes_status ~ HighBP + HighChol + CholCheck +
                               BMI + Stroke + HeartDiseaseorAttack + PhysActivity + Fruits +
                               Veggies + HvyAlcoholConsump + GenHlth + DiffWalk + Sex +
                               Age + Income, data = diabetes.train, 
                             family = binomial(link = "logit")) 
summary(diabetes.logit.manual) # AIC: 10300
diabetes.logit.manual.pred <- predict(diabetes.logit.manual, newdata = diabetes.test,
                                      type = "response")
diabetes.logit.manual.pred <- factor(diabetes.logit.manual.pred > 0.5,
                                     levels = c(FALSE, TRUE),
                                     labels = c("No", "Yes"))
table(diabetes.test$Diabetes_status, diabetes.logit.manual.pred,
      dnn=c("Actual", "Predicted"))
mean(diabetes.test$Diabetes_status == diabetes.logit.manual.pred) # Predict Accuracy >> 85.26%


# (7) conclusion





####################################################
#######            랜덤 포레스트          ##########
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
# ######################################
# ######## 서포트 벡터 머신 ############
# ######################################
# # library(e1071)
# # set.seed(123)
# # diabetes.svm <- svm(Diabetes_status ~ ., data = diabetes.train)
# # summary(diabetes.svm) # Number of Support Vectors:  248 (144, 104)
# # 
# # diabetes.svm.pred <- predict(diabetes.svm, newdata = diabetes.test)
# # head(diabetes.svm.pred)
# # table(diabetes.test$Diabetes_status, diabetes.svm.pred,
# #       dnn=c("Actual", "Predicted"))
# # mean(diabetes.test$Diabetes_status == diabetes.svm.pred) # 0.7403315 예측 정확도
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
