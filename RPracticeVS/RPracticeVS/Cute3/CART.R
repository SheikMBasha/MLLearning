#Clear Environment variables
rm(list = ls(all = T))

#get\\set working directory
getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Cute3")

# add library references
library(caret)
library(DMwR)
library(rpart)
library(rpart.plot)

#read data file
raw <- read.csv("Group13-Cute2-Final-Data.csv", header = T, sep = ",")

#removing rows having Target varialbe as 0
raw <-  raw[raw$TotalRevenueGenerated > 0 ,]

#Check structure and summary of data
str(raw)
summary(raw)

#Converting character variables into factors
raw$FavSourceBin <- as.factor(raw$FavSourceBin)
raw$FavSource7Bin <- as.factor(raw$FavSource7Bin)
raw$FavSource30Bin <- as.factor(raw$FavSource30Bin)
raw$FavSource90Bin <- as.factor(raw$FavSource90Bin)
raw$FavSource180Bin <- as.factor(raw$FavSource180Bin)
raw$FavSource360Bin <- as.factor(raw$FavSource360Bin)

raw$FavChannelBin <- as.factor(raw$FavChannelBin)
raw$FavChannel7Bin <- as.factor(raw$FavChannel7Bin)
raw$FavChannel30Bin <- as.factor(raw$FavChannel30Bin)
raw$FavChannel90Bin <- as.factor(raw$FavChannel90Bin)
raw$FavChannel180Bin <- as.factor(raw$FavChannel180Bin)
raw$FavChannel360Bin <- as.factor(raw$FavChannel360Bin)

raw$FavGameBin <- as.factor(raw$FavGameBin)
raw$FavGame7Bin <- as.factor(raw$FavGame7Bin)
raw$FavGame30Bin <- as.factor(raw$FavGame30Bin)
raw$FavGame90Bin <- as.factor(raw$FavGame90Bin)
raw$FavGame180Bin <- as.factor(raw$FavGame180Bin)
raw$FavGame360Bin <- as.factor(raw$FavGame360Bin)

#Removing unwanted variables which have been converted to categorical type

rawFiltered <- subset(raw, select = -c(CONTACT_WID, NominationDate, OveralllastTransaction, CHILD_AGE_RANGE
                                       , FavoriteChannel, FavoriteChannel7, FavoriteChannel30, FavoriteChannel90, FavoriteChannel180, FavoriteChannel360,
                                       FavoriteSource, FavoriteSource7, FavoriteSource30, FavoriteSource90, FavoriteSource180, FavoriteSource360,
                                       FavoriteGame, FavoriteGame7, FavoriteGame30, FavoriteGame90, FavoriteGame180, FavoriteGame360))

summary(rawFiltered)
str(rawFiltered)

#Applying Square transformation
rawFiltered$TotalRevenueGenerated <- sqrt(rawFiltered$TotalRevenueGenerated)

#Set seed and split data into test and train
set.seed(123)
datapart <- createDataPartition(rawFiltered$TotalRevenueGenerated, times = 1, p = 0.7, list = F)
train <- rawFiltered[datapart,]
test <- rawFiltered[-datapart,]
#? rpart

rPartModel <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova")
print(rPartModel)
printcp(rPartModel)
rpart.plot(rPartModel)

predcartTrain <- predict(rPartModel, newdata = train, type = "vector")
predcartTest <- predict(rPartModel, newdata = test, type = "vector")

ErrorDF <- data.frame(ModelTrain = regr.eval(train$TotalRevenueGenerated, predcartTrain))
ErrorDF <- data.frame(ErrorDF, ModelTest = regr.eval(test$TotalRevenueGenerated, predcartTest))

#MAPE is 0.3141461 and 0.3188665
# we can further decrease the cp (complexity paramter) to check if the error goes down
# at cp 0.010000, xerror is 0.18908
#? rpart.control
#trying with cp 0.001
rPartModel1 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 0.001))
print(rPartModel1)
printcp(rPartModel1)
rpart.plot(rPartModel1)

predcartTrain1 <- predict(rPartModel1, newdata = train, type = "vector")
predcartTest1 <- predict(rPartModel1, newdata = test, type = "vector")

ErrorDF <- data.frame(ErrorDF, TrainErrorC0.001 = regr.eval(train$TotalRevenueGenerated, predcartTrain1))
ErrorDF <- data.frame(ErrorDF, TestErrorC0.001 = regr.eval(test$TotalRevenueGenerated, predcartTest1))

# at cp 0.001, xerror  0.12117 is further decreasing
# lets try with cp = 0.0001 and then tried 0.0009 because error was constant
rPartModel2 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 0.0001))
print(rPartModel2)
printcp(rPartModel2)
rpart.plot(rPartModel2)
plotcp(rPartModel2)

predcartTrain2 <- predict(rPartModel2, newdata = train, type = "vector")
predcartTest2 <- predict(rPartModel2, newdata = test, type = "vector")

ErrorDF <- data.frame(ErrorDF, TrainErrorC0.0001 = regr.eval(train$TotalRevenueGenerated, predcartTrain2))
ErrorDF <- data.frame(ErrorDF, TestErrorC0.0001 = regr.eval(test$TotalRevenueGenerated, predcartTest2))

# As the error is mostly constant in rpart.plot, lets try min split 10

rPartModel3 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 0.0001, minsplit = 10))
print(rPartModel3)
printcp(rPartModel3)
rpart.plot(rPartModel3)
plotcp(rPartModel3)

predcartTrain3 <- predict(rPartModel3, newdata = train, type = "vector")
predcartTest3 <- predict(rPartModel3, newdata = test, type = "vector")

ErrorDF <- data.frame(ErrorDF, TrainErrorC0.0001MinSplit10 = regr.eval(train$TotalRevenueGenerated, predcartTrain3))
ErrorDF <- data.frame(ErrorDF, TestErrorC0.0001MinSplit10 = regr.eval(test$TotalRevenueGenerated, predcartTest3))

# As the error is mostly constant in rpart.plot, lets try min split 50

rPartModel4 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 0.00001, minsplit = 10))
print(rPartModel4)
printcp(rPartModel4)
rpart.plot(rPartModel4)
plotcp(rPartModel4)

predcartTrain4 <- predict(rPartModel4, newdata = train, type = "vector")
predcartTest4 <- predict(rPartModel4, newdata = test, type = "vector")

ErrorDF <- data.frame(ErrorDF, TrainErrorC0.0001MinSplit50 = regr.eval(train$TotalRevenueGenerated, predcartTrain4))
ErrorDF <- data.frame(ErrorDF, TestErrorC0.0001MinSplit50 = regr.eval(test$TotalRevenueGenerated, predcartTest4))

######
# trying with cp #5.2803e-05

#rPartModel5 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 5.2803e-05, minsplit = 10))
rPartModel5 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 2.0873e-04, minsplit = 10))

print(rPartModel5)
printcp(rPartModel5)
rpart.plot(rPartModel5)
plotcp(rPartModel5)

predcartTrain5 <- predict(rPartModel5, newdata = train, type = "vector")
predcartTest5 <- predict(rPartModel5, newdata = test, type = "vector")

ErrorDF <- data.frame(ErrorDF, TrainErrorC0.00005MinSplit10 = regr.eval(train$TotalRevenueGenerated, predcartTrain5))
ErrorDF <- data.frame(ErrorDF, TestErrorC0.00005MinSplit10 = regr.eval(test$TotalRevenueGenerated, predcartTest5))

# when we saw summary of rpartModel3 
# summary shows error increases at below row
# 64  0.00028545     65  0.074950 0.094993 0.0034154
# so going with cp 0.00028545
# and min split 10

train_control <- trainControl(method = "repeatedcv", number = 50, savePredictions = TRUE)
?train
cvModel <- train(TotalRevenueGenerated ~ ., data = train, method = "rpart", trControl = train_control, parms = lift(split = "gini"), tunelength= 10 )
cvModel$bestTune
print(cvModel)

rPartModel5 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = cvModel$bestTune))
print(rPartModel5)
printcp(rPartModel5)
rpart.plot(rPartModel5)
plotcp(rPartModel5)

predcartTrain5 <- predict(rPartModel5, newdata = train, type = "vector")
predcartTest5 <- predict(rPartModel5, newdata = test, type = "vector")

ErrorDF <- data.frame(ErrorDF, TrainErrorKFold = regr.eval(train$TotalRevenueGenerated, predcartTrain5))
ErrorDF <- data.frame(ErrorDF, TestErrorKFold = regr.eval(test$TotalRevenueGenerated, predcartTest5))

cvModel$pred

#actual <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#predicted <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)
#regr.eval(actual, predicted)

#mape <- function() {
    #actual <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    #predicted <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)
    #p <- c()
    #for (i in 1:length(actual)) {
        #x <- (actual[i] - predicted[i]) / actual[i]
        #p <- c(p, x)
    #}
    ##print(actual[1])
    ##print(predicted[1])
    ##x <- (actual[1] - predicted[1]) / actual[1]
    ##p <- c(p, x)
    #return(p)
#}

#abc <- mape()