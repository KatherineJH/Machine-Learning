setwd("D:/OneDrive/Documents/DM&ML/TABA/Datasets/")
# NULL은 초기화되지 않은 값.
# NA은 아직 모른다(Not Available): 할당하지 않은 값, 결측치.


Data <- read.csv("heart_disease_health_indicators_BRFSS2015.csv", header=T, na.strings=c(""), stringsAsFactors = T)

# # 결측치 없음
# print(length(Data[is.na(Data)])) 
# sum(!complete.cases(Data)) 

str(Data)
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

sample <- sample(nrow(Data), 0.05*nrow(Data))
Data <- Data[sample, ]

# training data / test data 쪼개기
set.seed(123)
train <- sample(nrow(Data), 0.7*nrow(Data))
hd.train <- Data[train, ]
hd.test <- Data[-train, ]

str(hd.train)
table(hd.train$HeartDiseaseorAttack)
prop.table(table(hd.train$HeartDiseaseorAttack))


table(hd.train$HeartDiseaseorAttack)
table(hd.test$HeartDiseaseorAttack)

##########################################
# str(hd.train)
# table(hd.train$CholCheck)
# library(rpart) # decision tree를 지원하는 패키지 중 하나
# hd.dtree <- rpart(formula = HeartDiseaseorAttack ~ ., data = hd.train, method = "class", 
#                   parms=list(split = "information"))
# hd.dtree
######################################

######################################
library (tree)
tree.hd <- tree(HeartDiseaseorAttack~ ., data = hd.train)
tree.hd # 5번의 분할, 6개의 최종 노드
summary(tree.hd)
# Variables actually used in tree construction:
#   [1] "GenHlth" "Age"    
# Number of terminal nodes:  5 
# Residual mean deviance:  0.509 = 4516 / 8873 
# Misclassification error rate: 0.08955 = 795 / 8878 
######################################
###############################################
# scale로 전부 다시 돌리기
###############################################
setwd("D:/OneDrive/Documents/DM&ML/TABA/Datasets/")
Data <- read.csv("heart_disease_health_indicators_BRFSS2015.csv", header=T, na.strings=c(""), stringsAsFactors = T)


# str(Data)
# str(hd_sd)
# str(Data[,-c(5, 16, 17, 20)])
# str(cbind(Data[,-c(5, 16, 17)], hd_sd))

Data$HeartDiseaseorAttack <- factor(Data$HeartDiseaseorAttack,
                                    levels = c("0", "1"),
                                    labels = c("no", "yes"))

mean(Data$HeartDiseaseorAttack == "no") # 0.9058144 음성
mean(Data$HeartDiseaseorAttack == "yes") # 0.09418559 양성
sum(!complete.cases(Data))

# # 2. 데이터를 표준화 한다.
# hd_sd <- as.data.frame(lapply(Data[,c(5, 16, 17)], scale))
# ## 다시 열을 합친다.
# Data <- cbind(Data[,-c(5, 16, 17)], hd_sd)

# # 3. factor 치환
# Data$HighBP <- as.factor(Data$HighBP)
# Data$HighChol <- as.factor(Data$HighChol)
# Data$CholCheck <- as.factor(Data$CholCheck)
# Data$Smoker <- as.factor(Data$Smoker)
# Data$Stroke <- as.factor(Data$Stroke)
# Data$Diabetes <- as.factor(Data$Diabetes)
# Data$PhysActivity <- as.factor(Data$PhysActivity)
# Data$Fruits <- as.factor(Data$Fruits)
# Data$Veggies <- as.factor(Data$Veggies)
# Data$HvyAlcoholConsump <- as.factor(Data$HvyAlcoholConsump)
# Data$AnyHealthcare <- as.factor(Data$AnyHealthcare)
# Data$NoDocbcCost <- as.factor(Data$NoDocbcCost)
# Data$GenHlth <- as.factor(Data$GenHlth)
# Data$DiffWalk <- as.factor(Data$DiffWalk)
# Data$Sex <- as.factor(Data$Sex)
# Data$Education <- as.factor(Data$Education)
# Data$Income <- as.factor(Data$Income)
# Data$Age <- as.factor(Data$Age)



table(Data$MentHlth)
          
str(Data)

sample <- sample(nrow(Data), 0.05*nrow(Data))
Data <- Data[sample, ]

# training data / test data 쪼개기
set.seed(123)
train <- sample(nrow(Data), 0.7*nrow(Data))
hd.train <- Data[train, ]
hd.test <- Data[-train, ]


library (tree)
tree.hd <- tree(HeartDiseaseorAttack~., data = hd.train )
tree.hd # 5번의 분할, 6개의 최종 노드
summary(tree.hd)
# Variables actually used in tree construction:
#   [1] "GenHlth"      "Age"          "BMI"         
# [4] "Education"    "PhysActivity" "Stroke"      
# [7] "DiffWalk"     "Income"       "PhysHlth"    
# Number of terminal nodes:  17 
# Residual mean deviance:  0.3624 = 154.4 / 426 
# Misclassification error rate: 0.07223 = 32 / 443 
tree.pred <- predict(tree.hd, hd.test, type = "class")
table(tree.pred, hd.test$HeartDiseaseorAttack)
str(hd.train)

plot(tree.hd)
text(tree.hd , pretty = 0)

set.seed (2)
train <- sample(1:nrow(Data), 200)
hd.test <- Data[-train , ]
hd.att.test <- Data$HeartDiseaseorAttack[-train]
tree.hd <- tree(HeartDiseaseorAttack~., Data, subset = train)
tree.pred <- predict (tree.hd, hd.test, type = "class")
table(tree.pred, hd.att.test)
str(train)

set.seed(123)
cv.hd <- cv.tree(tree.hd, FUN=prune.misclass)
names(cv.hd)

par(mfrow=c(1,2))
plot(cv.hd$size, cv.hd$dev, type = "b")
plot(cv.hd$k, cv.hd$dev, type = "b")



prune.hd <- prune.misclass(tree.hd, best = 9)
plot(prune.hd)
text(prune.hd, pretty = 0)












