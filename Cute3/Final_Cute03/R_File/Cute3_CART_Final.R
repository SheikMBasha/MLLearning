rm(list = ls(all = TRUE))

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
raw <- raw[raw$TotalRevenueGenerated > 0,]

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

rawFiltered <- subset(rawFiltered, select = -c(FavSource7Bin, FavSource30Bin, FavSource90Bin, FavSource180Bin, FavSource360Bin
                                               , FavChannel7Bin, FavChannel30Bin, FavChannel90Bin, FavChannel180Bin, FavChannel360Bin
                                               , FavGame7Bin, FavGame30Bin, FavGame90Bin, FavGame180Bin, FavGame360Bin))

rawFiltered <- subset(rawFiltered, select = -c(Units7, Units30, Units90, Units180, FrequencyLF7, FrequencyLF90, FrequencyLF180, FrequencyLF360,
                                               FrequencyApp7, FrequencyApp90, FrequencyApp180, FreqGamePlay30, FreqGamePlay90, FreqGamePlay180,
                                               Revenue7, Revenue90, TotalTimeGamePlay, TotalTimeGamePlay7, TotalTimeGamePlay30, TotalTimeGamePlay90, TotalTimeGamePlay180,
                                               TotalTimeGamePlay360, NumGamesPlayed7, NumGamesPlayed30, NumGamesPlayed90, RecencyLF90, RecencyLF180))

rawFiltered <- subset(rawFiltered, select = -c(FreqGamePlay7, NumGamesBought, RecencyApp7))
rawFiltered <- subset(rawFiltered, select = -c(RecencyApp30, RecencyApp90, RecencyApp180, Recencydown7, Recencydown30, Recencydown90, Recencydown180,
                                               maxRecencyCum7, minRecencyCum7, maxRecencyCum30, minRecencyCum30, maxRecencyCum90, minRecencyCum90, maxRecencyCum180, minRecencyCum180))

rawFiltered <- subset(rawFiltered, select = -c(NumFemaleChildrenHousehold, NumMaleChildrenHousehold))
rawFiltered <- subset(rawFiltered, select = -c(NumGamesPlayed180, minRecencyCum360, FavChannelBin))

rawFiltered <- subset(rawFiltered, select = -c(MAX_AGE, NumHouseChildren, RecencyApp360, Recencydown, Recencydown360, maxRecencyCum, StrengthOfFavoriteGame))

str(rawFiltered)

# Check the summary of the dataset
summary(rawFiltered)

# Check for Missing Values
sum(is.na(rawFiltered))


#Log transformation to Target Variable
rawFiltered$TotalRevenueGenerated <- log(rawFiltered$TotalRevenueGenerated)

#Set seed and split data into test and train
set.seed(9983)
datapart <- createDataPartition(rawFiltered$TotalRevenueGenerated, times = 1, p = 0.7, list = F)
train <- rawFiltered[datapart,]
test <- rawFiltered[-datapart,]
#? rpart
#rm(ErrorDF)
#Building Decision Tree Regression model using rpart
rPartModel <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova")
print(rPartModel)
printcp(rPartModel)
rpart.plot(rPartModel)

predcartTrain <- predict(rPartModel, newdata = train, type = "vector")
predcartTest <- predict(rPartModel, newdata = test, type = "vector")

ErrorDF <- data.frame(TrainCP0.01 = regr.eval(train$TotalRevenueGenerated, predcartTrain))
ErrorDF <- data.frame(ErrorDF, TestCP0.01 = regr.eval(test$TotalRevenueGenerated, predcartTest))

#MAPE is 0.3141461 and 0.3188665
# we can further decrease the cp (complexity paramter) to check if the error goes down
# at cp 0.010000, xerror is 0.18908
#Building model with cp 0.001
rPartModel1 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 0.001))
print(rPartModel1)
printcp(rPartModel1)
rpart.plot(rPartModel1)

predcartTrain1 <- predict(rPartModel1, newdata = train, type = "vector")
predcartTest1 <- predict(rPartModel1, newdata = test, type = "vector")

ErrorDF <- data.frame(ErrorDF, TrainCP0.001 = regr.eval(train$TotalRevenueGenerated, predcartTrain1))
ErrorDF <- data.frame(ErrorDF, TestCP0.001 = regr.eval(test$TotalRevenueGenerated, predcartTest1))

# at cp 0.001, xerror  0.12117
# Building model with cp=0.0001 to check if further the xerror is decreasing
rPartModel2 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 0.0001))
print(rPartModel2)
printcp(rPartModel2)
rpart.plot(rPartModel2)
plotcp(rPartModel2)

predcartTrain2 <- predict(rPartModel2, newdata = train, type = "vector")
predcartTest2 <- predict(rPartModel2, newdata = test, type = "vector")

ErrorDF <- data.frame(ErrorDF, TrainCP0.0001 = regr.eval(train$TotalRevenueGenerated, predcartTrain2))
ErrorDF <- data.frame(ErrorDF, TestCP0.0001 = regr.eval(test$TotalRevenueGenerated, predcartTest2))

# The error seems to be constant as seen in rpart.plot, try with min split 10 to check if error is decreasing
# Building model with cp=0.0001 , minsplit = 10
rPartModel3 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 0.0001, minsplit = 10))
print(rPartModel3)
printcp(rPartModel3)
rpart.plot(rPartModel3)
plotcp(rPartModel3)

predcartTrain3 <- predict(rPartModel3, newdata = train, type = "vector")
predcartTest3 <- predict(rPartModel3, newdata = test, type = "vector")

ErrorDF <- data.frame(ErrorDF, TrainCP0.0001_MinSp10 = regr.eval(train$TotalRevenueGenerated, predcartTrain3))
ErrorDF <- data.frame(ErrorDF, TestCP0.0001_MinSp10 = regr.eval(test$TotalRevenueGenerated, predcartTest3))

# The mape seems to be constant , try with cp = 0.00001 to check if error is decreasing
# Building model with cp = 0.00001, minsplit = 10
rPartModel4 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 0.00001, minsplit = 10))
print(rPartModel4)
printcp(rPartModel4)
rpart.plot(rPartModel4)
plotcp(rPartModel4)

predcartTrain4 <- predict(rPartModel4, newdata = train, type = "vector")
predcartTest4 <- predict(rPartModel4, newdata = test, type = "vector")

ErrorDF <- data.frame(ErrorDF, Train0.00001_MinSp10 = regr.eval(train$TotalRevenueGenerated, predcartTrain4))
ErrorDF <- data.frame(ErrorDF, Test0.00001_MinSp10 = regr.eval(test$TotalRevenueGenerated, predcartTest4))

#xerror is increasing after 2.0873e-04
# Building model with cp = 2.0873e-04, minsplit = 10
#rPartModel5 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 5.2803e-05, minsplit = 10))
rPartModel5 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 2.0873e-04, minsplit = 10))

print(rPartModel5)
printcp(rPartModel5)
rpart.plot(rPartModel5)
plotcp(rPartModel5)

predcartTrain5 <- predict(rPartModel5, newdata = train, type = "vector")
predcartTest5 <- predict(rPartModel5, newdata = test, type = "vector")

ErrorDF <- data.frame(ErrorDF, Train0.00002_MinSp10 = regr.eval(train$TotalRevenueGenerated, predcartTrain5))
ErrorDF <- data.frame(ErrorDF, Test0.00002_MinSp10 = regr.eval(test$TotalRevenueGenerated, predcartTest5))




