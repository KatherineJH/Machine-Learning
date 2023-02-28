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

# The Naive-Bayes method estimates the probability of an event of interest based on Bayes theorem. Since the probability of an event (Dependent Variable) occurring is calculated based on predictors(Independent Variables) known in advance, the DV corresponding to the event must be a categorical variable, and IVs assume a categorical variable.
# 
# When the predictive probability of an event occurrence is calculated for each case, this value is compared with a preset threshold (cut-off value), and if it is greater than that, it is classified as an event occurrence and otherwise, an event does not occur. For example, if the threshold is set to 0.5, an event occurs if the derived prediction probability is greater than 0.5, and an event does not occur if it is less than 0.5.
# 
# The breaking point
# Naive-Bayes assume that IVs are independent of each other and all equally important, however, in reality, all predictors cannot be completely independent of each other, and some variables play a more important role in predicting DV than others.

#######################################################
# 나이브는 이 사이트를 기초로 할 것.
# https://www.edureka.co/blog/naive-bayes-in-r/
# https://minicokr.com/27
#######################################################

setwd("D:/OneDrive/Documents/DM&ML/TABA/Datasets/")
# NULL은 초기화되지 않은 값.
# NA은 아직 모른다(Not Available): 할당하지 않은 값, 결측치.

##############################
########## Histogram #########
##############################
# Age is not far off of a normal distribution, But for Flight distance/Delay departure time/Delay arrival time, a proper decision is required where to put the boundaries. 
training <- read.csv("Airline_train.csv", header=T, na.strings=c(""), stringsAsFactors = T)
training<- training[complete.cases(training), ]

par(mfrow=c(2,2))
# X-axis grid
x <- seq(min(training$Age), max(training$Age))
# Normal curve
fun <- dnorm(x, mean = mean(training$Age), sd = sd(training$Age))
# Histogram
hist(training$Age, prob = TRUE, col = "white",
     ylim = c(0, max(fun)),
     main = "Histogram with normal curve")
curve(dnorm(x, mean=mean(training$Age), sd=sqrt(var(training$Age))), 
      col="tomato", lwd=2, add=TRUE, yaxt="n")
hist(training$Flight.Distance, breaks = 100)
hist(training$Departure.Delay.in.Minutes, breaks = 100)
hist(training$Arrival.Delay.in.Minutes, breaks = 100)

# X-axis grid
x <- seq(min(training$Flight.Distance), max(training$Flight.Distance))
# Normal curve
fun <- dnorm(x, mean = mean(training$Flight.Distance), sd = sd(training$Flight.Distance))
# Histogram
hist(training$Flight.Distance, prob = TRUE, col = "white",
     ylim = c(0, max(fun)),
     main = "Histogram with normal curve")
curve(dnorm(x, mean=mean(training$Flight.Distance), sd=sqrt(var(training$Flight.Distance))), 
      col="tomato", lwd=2, add=TRUE, yaxt="n")

dev.off()
####################################################################


###############################
########## Train Data #########
###############################
train <- read.csv("Airline_train.csv", header=T, na.strings=c(""), stringsAsFactors = T)
nrow(train) # 103904
# 결측치 처리(10만개 중 300개 >> 결측치 없애고 없었다고 친다.)
print(length(train[is.na(train)])) # 310
sum(!complete.cases(train)) # 310
310/103904 # 결측치 비율 >> 전체 데이터 중 0.002983523 > 제거한다.
train<- train[complete.cases(train), ]
nrow(train) # 103594

train <- train[, -1:-2] # 아이디 열 삭제

train$satisfaction <- factor(train$satisfaction, 
                             levels = c("neutral or dissatisfied","satisfied"), 
                             labels = c("No", "Yes"))
str(train)
####################################################################
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

# 나머지도 모두 factor화 시킨다.
train$Inflight.wifi.service <- as.factor(train$Inflight.wifi.service)
train$Departure.Arrival.time.convenient <- as.factor(train$Departure.Arrival.time.convenient)
train$Ease.of.Online.booking <- as.factor(train$Ease.of.Online.booking)
train$Gate.location <- as.factor(train$Gate.location)
train$Food.and.drink <- as.factor(train$Food.and.drink)
train$Online.boarding <- as.factor(train$Online.boarding)
train$Seat.comfort <- as.factor(train$Seat.comfort)
train$Inflight.entertainment <- as.factor(train$Inflight.entertainment)
train$On.board.service <- as.factor(train$On.board.service)
train$Leg.room.service <- as.factor(train$Leg.room.service)
train$Baggage.handling <- as.factor(train$Baggage.handling)
train$Checkin.service <- as.factor(train$Checkin.service)
train$Inflight.service <- as.factor(train$Inflight.service)
train$Cleanliness <- as.factor(train$Cleanliness)

# 샘플링
sample <- sample(nrow(train), 0.05*nrow(train))
airline.train <- train[sample, ]

nrow(airline.train) # 5179
str(airline.train)


###############################
########## Test Data  #########
###############################
test <- read.csv("Airline_test.csv", header=T, na.strings=c(""), stringsAsFactors = T)
nrow(test)
# 결측치 처리(2만5천개 중 83개 >> 결측치 없애고 없었다고 친다.)
print(length(test[is.na(test)])) # 83
sum(!complete.cases(test)) # 83
83/25976 # 결측치 비율 >> 0.003195257 >> 결측치 열을 제거한다.
test<- test[complete.cases(test), ]
nrow(test)

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

# 나머지도 모두 factor화 시킨다.
test$Inflight.wifi.service <- as.factor(test$Inflight.wifi.service)
test$Departure.Arrival.time.convenient <- as.factor(test$Departure.Arrival.time.convenient)
test$Ease.of.Online.booking <- as.factor(test$Ease.of.Online.booking)
test$Gate.location <- as.factor(test$Gate.location)
test$Food.and.drink <- as.factor(test$Food.and.drink)
test$Online.boarding <- as.factor(test$Online.boarding)
test$Seat.comfort <- as.factor(test$Seat.comfort)
test$Inflight.entertainment <- as.factor(test$Inflight.entertainment)
test$On.board.service <- as.factor(test$On.board.service)
test$Leg.room.service <- as.factor(test$Leg.room.service)
test$Baggage.handling <- as.factor(test$Baggage.handling)
test$Checkin.service <- as.factor(test$Checkin.service)
test$Inflight.service <- as.factor(test$Inflight.service)
test$Cleanliness <- as.factor(test$Cleanliness)


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

#######################################################
########     Categorigal Naive-Bayes       ############
########      Bernoulli Naive-Bayes        ############
#######################################################
library(e1071) # e1071 contains naiveBayes()
set.seed(123)
airline.naive <- naiveBayes(satisfaction ~ ., 
                            data = airline.train, laplace = 1) # Laplace Smoothing
airline.naive
summary(airline.naive)
# Predict
airline.naive.predicted <- predict(airline.naive, newdata=airline.test)
# p <- predict(model, airline.test, type= 'raw')
# head(cbind(p, airline.train))

length(airline.test$satisfaction)
length(airline.naive.predicted)
# 이전과 똑같은 결과를 얻게 된다.
table(airline.test$satisfaction, airline.naive.predicted, 
      dnn = c("Actual", "Predicted"))
#           Predicted
# Actual      No Yes
#        No  669  78
#       Yes  80 467
mean(airline.naive.predicted == airline.test$satisfaction) # 87.78%


# Misclassification error - train data
p1 <- predict(airline.naive, newdata = airline.train)
tab1 <- table(p1, airline.train$satisfaction)
sum(diag(tab1))/ sum(tab1)     # 89.05%
1 - sum(diag(tab1))/ sum(tab1) # 10.94%

# Misclassification error - test data
p2 <- predict(airline.naive, newdata = airline.test)
tab2 <- table(p2, airline.test$satisfaction)
sum(diag(tab2))/ sum(tab2)     # 87.78%
1 - sum(diag(tab2))/ sum(tab2) # 12.21%

#######################################################
# # same process
# airline.nb.pred <- predict(model, newdata = airline.test,
#                            type="raw") # raw >> probability 
# head(airline.nb.pred)
# 
# airline.nb.pred <- factor(airline.nb.pred[, "Yes"] > 0.5,
#                           levels = c(FALSE, TRUE),
#                           labels = c("No", "Yes"))
# head(airline.nb.pred)
# 
# # same output
# table(airline.test$satisfaction, airline.nb.pred, 
#       dnn = c("Actual", "Predicted"))
# mean(airline.nb.pred == airline.test$satisfaction) # 87.78%

#######################################################
########         Cross validation          ############
#######################################################
# cross validation
nbRuns <- function(fraction, run){
  results <- NULL
  for (i in 1:run) {
    sample <- sample(nrow(train), 0.05*nrow(train))
    ar.train <- train[sample, ]
    
    sample <- sample(nrow(test), 0.05*nrow(test))
    ar.test <- test[sample, ]
    
    ar.nb <- naiveBayes(satisfaction ~., data = ar.train)
    ar.nb.pred <- predict(ar.nb, newdata = ar.test)
    results[i] <- mean(ar.nb.pred == ar.test$satisfaction)
  }
  return(results)
}

ar.nb.cv <- nbRuns(0.7, 100)
ar.nb.cv
summary(ar.nb.cv)
# Min.    1st Qu.  Median  Mean   3rd Qu.  Max. 
# 0.8686  0.8833  0.8887  0.8876  0.8926  0.9042 

#####################################################
########         Check ggplot            ############
#####################################################
library(ggplot2)
ggplot(data.frame(acc=ar.nb.cv), aes(x="", y=acc)) +
  geom_boxplot(fill="slategray", color="darkslategray",
               width=0.5) +
  geom_point(position = "jitter", color="royalblue",
             alpha=0.7) +
  labs(title = "Accuracy for satisfied Prediction with 100 Samples",
       y="Accuracy") +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

#######################################################













































