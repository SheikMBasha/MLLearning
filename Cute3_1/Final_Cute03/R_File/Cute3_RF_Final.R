rm(list = ls(all = TRUE))

getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Cute3")

# add library references
library(caret)
library(DMwR)
library(vegan)
library(randomForest)

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

# Split the dataset into test and train
set.seed(9983)
train <- createDataPartition(rawFiltered$TotalRevenueGenerated, times = 1, p = 0.7, list = F)
train_data <- rawFiltered[train,]
test_data <- rawFiltered[-train,]

# Separating target variable from independent variables
train_target <- train_data$TotalRevenueGenerated
train_data$TotalRevenueGenerated <- NULL
test_target <- test_data$TotalRevenueGenerated
test_data$TotalRevenueGenerated <- NULL

#####################################################################################################################


## Random Forest
# We can build a random forest model using the randomForest() function from the randomForest() package
# Below, we use the default parameters to build the random forest model
model_Rf <- randomForest(train_target ~ ., train_data, ntree = 50, mtry = 5)

# We can also look at variable importance from the built model using the importance() function 
#and visualise it using the varImpPlot() funcion
importance(model_Rf)
varImpPlot(model_Rf)

#  predictions from the model
pred_train <- predict(model_Rf, train_data)
pred_test <- predict(model_Rf, test_data)

# Error verification on Train and Test data
ErrorFrame <- data.frame(RF_Train50_5 = regr.eval(train_target, pred_train))
ErrorFrame <- data.frame(ErrorFrame, RF_Test50_5 = regr.eval(test_target, pred_test))

#####################################################################################################################


#Build RF model with  ntree = 30, mtry = 9
model_Rf2 <- randomForest(train_target ~ ., train_data, ntree = 30, mtry = 9)

# We can also look at variable importance from the built model using the importance() function 
#and visualise it using the varImpPlot() funcion
importance(model_Rf2)
varImpPlot(model_Rf2)


#predictions from the model
pred_train2 <- predict(model_Rf2, train_data)
pred_test2 <- predict(model_Rf2, test_data)

# Error verification on Train and Test data
ErrorFrame <- data.frame(ErrorFrame, RF_Train30_9 = regr.eval(train_target, pred_train2))
ErrorFrame <- data.frame(ErrorFrame, RF_Test30_9 = regr.eval(test_target, pred_test2))

#####################################################################################################################

#Build RF model with  ntree = 50, mtry = 3
model_Rf3 <- randomForest(train_target ~ ., train_data, ntree = 50, mtry = 3)

# We can also look at variable importance from the built model using the importance() function 
#and visualise it using the varImpPlot() funcion
importance(model_Rf3)
varImpPlot(model_Rf3)


#predictions from the model
pred_train3 <- predict(model_Rf3, train_data)
pred_test3 <- predict(model_Rf3, test_data)

# Error verification on Train and Test data
ErrorFrame <- data.frame(ErrorFrame, RF_Train50_3 = regr.eval(train_target, pred_train3))
ErrorFrame <- data.frame(ErrorFrame, RF_Test50_3 = regr.eval(test_target, pred_test3))

#####################################################################################################################

#Build RF model with  ntree = 10, mtry = 6
model_Rf4 <- randomForest(train_target ~ ., train_data, ntree = 10, mtry = 6)

# We can also look at variable importance from the built model using the importance() function 
#and visualise it using the varImpPlot() funcion
importance(model_Rf4)
varImpPlot(model_Rf4)


#predictions from the model
pred_train4 <- predict(model_Rf4, train_data)
pred_test4 <- predict(model_Rf4, test_data)

# Error verification on Train and Test data
ErrorFrame <- data.frame(ErrorFrame,RF_Train10_6 = regr.eval(train_target, pred_train4))
ErrorFrame <- data.frame(ErrorFrame, RF_Test10_6 = regr.eval(test_target, pred_test4))


#####################################################################################################################

#Build RF model with  ntree = 20, mtry = 3
model_Rf5 <- randomForest(train_target ~ ., train_data, ntree = 20, mtry = 3)

# We can also look at variable importance from the built model using the importance() function 
#and visualise it using the varImpPlot() funcion
importance(model_Rf5)
varImpPlot(model_Rf5)


#predictions from the model
pred_train5 <- predict(model_Rf5, train_data)
pred_test5 <- predict(model_Rf5, test_data)

# Error verification on Train and Test data
ErrorFrame <- data.frame(ErrorFrame,RF_Train20_3 = regr.eval(train_target, pred_train5))
ErrorFrame <- data.frame(ErrorFrame, RF_Test20_3 = regr.eval(test_target, pred_test5))