# ATTRIBUTES:
# Id: Id number of the passengers
# Gender: Gender of the passengers (Female, Male)
# Customer Type: The customer type (Loyal customer, disloyal customer)
# Age: The actual age of the passengers
# Type of Travel: Purpose of the flight of the passengers (Personal Travel, Business Travel)
# Class: Travel class in the plane of the passengers (Business, Eco, Eco Plus)
# Flight Distance: The flight distance of this journey
# Inflight wifi service: Satisfaction level of the inflight wifi service (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Departure/Arrival time convenient: Satisfaction level of Departure/Arrival time convenient (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Ease of Online booking: Satisfaction level of online booking (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Gate location: Satisfaction level of Gate location (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Food and drink: Satisfaction level of Food and drink service (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Online boarding: Satisfaction level of online boarding (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Seat comfort: Satisfaction level of Seat comfort (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Inflight entertainment: Satisfaction level of inflight entertainment (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# On-board service: Satisfaction level of On-board service (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Leg room service: Satisfaction level of Leg room service (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Baggage handling: Satisfaction level of baggage handling (1,2,3,4,5/ 1=Least Satisfied to 5=Most Satisfied)
# Checkin service: Satisfaction level of Check-in service (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Inflight service: Satisfaction level of inflight service (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Cleanliness: Satisfaction level of Cleanliness (0,1,2,3,4,5/ 0=Not Applicable; 1=Least Satisfied to 5=Most Satisfied)
# Departure Delay in Minutes: Minutes delayed when departure
# Arrival Delay in Minutes: Minutes delayed when arrival
# Satisfaction: /output column/ Airline satisfaction level ('satisfied', 'neutral or dissatisfied')

setwd("D:/OneDrive/Documents/DM&ML/TABA/Datasets/")
# NULL은 초기화되지 않은 값.
# NA은 아직 모른다(Not Available): 할당하지 않은 값, 결측치.

#############
### Train ###
#############
train <- read.csv("Airline_train.csv", header=T, na.strings=c(""), stringsAsFactors = T)
# 결측치 처리(10만개 중 300개 >> 결측치 없애고 없었다고 친다.)
print(length(train[is.na(train)])) # 310
sum(!complete.cases(train)) # 310
train<- train[complete.cases(train), ]

# Numerical >> 표준화 시키기 !!!
####################################################################
train <- train[, -1:-2] # 아이디 열 삭제


train$satisfaction <- factor(train$satisfaction, 
                             levels = c("neutral or dissatisfied","satisfied"), 
                             labels = c("No", "Yes"))


# Age
train$Age <- cut(train$Age,
                 breaks = c(0, 10, 20, 30, 40, 50, 60, 70, max(train$Age)),
                 labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70+"))

# Flight.Distance
train$Flight.Distance <- cut(train$Flight.Distance,
                             breaks = c(0, 400, 800, 1200, 1600, 2000, 2400, 2800, 3200, 3600, max(train$Flight.Distance)),
                             labels = c("0-400", "400-800", "800-1200", "1200-1600", "1600-2000", "2000-2400", "2400-2800", "2800-3200", "3200-3600", "4000+"))

# Arrival.Delay.in.Minutes
train$Arrival.Delay.in.Minutes <- cut(train$Arrival.Delay.in.Minutes,
                                      breaks=c(0,1, 5, 10, 20, 30, 40, 50, 60, 90, 120, max(train$Arrival.Delay.in.Minutes)), 
                                      labels=c("0", "1-5", "5-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-90", "90-120", "120+"),
                                      include.lowest=TRUE, 
                                      right=FALSE)

# Departure.Delay.in.Minutes
train$Departure.Delay.in.Minutes <- cut(train$Departure.Delay.in.Minutes,
                                        breaks=c(0,1, 5, 10, 20, 30, 40, 50, 60, 90, 120, max(train$Departure.Delay.in.Minutes)), 
                                        labels=c("0", "1-5", "5-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-90", "90-120", "120+"),
                                        include.lowest=TRUE, 
                                        right=FALSE)


# 샘플링
sample <- sample(nrow(train), 0.05*nrow(train))
airline.train <- train[sample, ]

nrow(airline.train) # 5179
str(airline.train)


#############
### Test  ###
#############
test <- read.csv("Airline_test.csv", header=T, na.strings=c(""), stringsAsFactors = T)
# 결측치 처리(2만5천개 중 83개 >> 결측치 없애고 없었다고 친다.)
print(length(test[is.na(test)])) # 83
sum(!complete.cases(test)) # 83
test<- test[complete.cases(test), ]

############################################################

test <- test[, -1:-2]   # 아이디 열 삭제

test$satisfaction <- factor(test$satisfaction, 
                            levels = c("neutral or dissatisfied","satisfied"), 
                            labels = c("No", "Yes"))

# Age
test$Age <- cut(test$Age,
                breaks = c(0, 10, 20, 30, 40, 50, 60, 70, max(test$Age)),
                labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70+"))

# Flight.Distance
test$Flight.Distance <- cut(test$Flight.Distance,
                            breaks = c(0, 400, 800, 1200, 1600, 2000, 2400, 2800, 3200, 3600, max(test$Flight.Distance)),
                            labels = c("0-400", "400-800", "800-1200", "1200-1600", "1600-2000", "2000-2400", "2400-2800", "2800-3200", "3200-3600", "4000+"))

# Arrival.Delay.in.Minutes
test$Arrival.Delay.in.Minutes <- cut(test$Arrival.Delay.in.Minutes,
                                     breaks=c(0,1, 5, 10, 20, 30, 40, 50, 60, 90, 120, max(test$Arrival.Delay.in.Minutes)), 
                                     labels=c("0", "1-5", "5-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-90", "90-120", "120+"),
                                     include.lowest=TRUE, 
                                     right=FALSE)

# Departure.Delay.in.Minutes
test$Departure.Delay.in.Minutes <- cut(test$Departure.Delay.in.Minutes,
                                       breaks=c(0,1, 5, 10, 20, 30, 40, 50, 60, 90, 120, max(test$Departure.Delay.in.Minutes)), 
                                       labels=c("0", "1-5", "5-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-90", "90-120", "120+"),
                                       include.lowest=TRUE, 
                                       right=FALSE)

# 샘플링
sample <- sample(nrow(test), 0.05*nrow(test))
airline.test <- test[sample, ]

nrow(airline.test)  # 1294
str(airline.test)


# 결측치 없음을 확인 
sum(is.na(train))
sum(is.na(test))
sum(is.na(airline.train))
sum(is.na(airline.test))


str(airline.train)
str(airline.test)

######################################
######## 서포트 벡터 머신 ############
######################################
library(e1071)
set.seed(123)

airline.svm <- svm(satisfaction ~ ., data = airline.train,
                kernel="linear", #kernel의 디폴트 값은 RBF(방사형 기저함수)
                cost=1, # 0보다 큰 값을 입력해야 한다. 디폴트 값은 1
                scale = FALSE) # scale: 데이터 표준화 할 지 말지 여부.
summary(airline.svm) # Number of Support Vectors:  1609( 809 800 )

# airline.svm <- svm(satisfaction ~ ., data = airline.train)
# 예측 모델 (원래는 test 데이터 필요함)
airline.svm.pred <- predict(airline.svm, newdata = airline.test)
head(airline.svm.pred)
table(airline.test$satisfaction, airline.svm.pred,
      dnn = c("Actual", "Predicted"))
mean(airline.test$satisfaction == airline.svm.pred)


airline.svm$index # 이 1609개를 확인할 때 index를 사용.
airline.svm$SV # 1609개 객체의 실제 관측값
airline.svm[airline.svm$index, ] # 범주까지 출력하려면 이렇게 한다.

#############################################
#############################################
# rbf kernal을 통한 튜닝 과정 
set.seed(123)
# linear kernel
airline.svm.tuned <- tune.svm(satisfaction~., data = airline.train,
                          gamma = 10^(-2:2), # gamma: 데이터를 분할하는 초평면의 형태를 결정한다. 
                          # gamma가 커지면 더 많은 서포트 벡터가 선택되서 더 복잡한 형태가 된다.(굴곡진 분류 경계선) 
                          # 일반화 측면에서 보면, 새로운 데이터가 생겼을 때 오류값이 많아 질 수 있다.
                          # gamma는 0보다 큰 값을 설정해야 한다.
                          # 디폴트 gamma 값은, 예측변수 개수/ 1
                          cost=2^(-2:2)) # 디폴트 cost 값은, 1
# (-3:3)는 7개, (-5:5)는 11개 >> 총 77개의 모델 조합이 생긴다.
summary(airline.svm.tuned)
# Parameter tuning of ‘svm’:
# - sampling method: 10-fold cross validation 
# - best parameters: gamma    cost
#                    0.001   0.03125
airline.svm.tuned$best.model$gamma # 0.001
airline.svm.tuned$best.model$cost # 0.03125

# 찾은 최적 gamma와 cost를 대입하기
set.seed(123)
airline.svm2 <- svm(satisfaction ~., data = airline.train,
                gamma= 0.001, cost=0.03125)
airline.svm2.pred <- predict(airline.svm2, airline.test)
table(airline.test$satisfaction, airline.svm2.pred,
      dnn = c("Actual", "Predict"))
mean(airline.test$satisfaction == airline.svm2.pred) # 0.7458564
# 0.7403315 >> 0.7458564 아주아주아주 조금 개선된 성능을 보인다.

#############################################
#############################################
library(ggplot2)
airline.mds <- data.frame(cmdscale(dist(airline.train[, -23])))
ggplot(airline.mds, aes(x=X1, y=X2)) + 
  geom_point(aes(color=airline.train[, 23],
                 shape=airline.train[, 23]), size=2) +
  geom_point(data = airline.mds[airline.svm$index, ],
             color="dimgray", shape=21,
             stroke=1.0, size=5) +
  labs(color="Species", shape="Species", x="Dimension 1", y="Dimension 2",
       title="SVM Classification from Airline Dataset") +
  theme(plot.title = element_text(face="bold"))
# 잘 분류 됐음을 시각적으로 확인할 수 있다.

# 그럼 test 데이터는 어떨까?
airline.svm.pred <- predict(airline.svm, airline.test)
table(na.omit(airline.test)$satisfaction, airline.svm.pred, dnn = c("Actual", "Predicted"))
mean(na.omit(airline.test)$satisfaction == airline.svm.pred) # test dataset에 대해서도 0.9777778 높은 예측력을 가짐








