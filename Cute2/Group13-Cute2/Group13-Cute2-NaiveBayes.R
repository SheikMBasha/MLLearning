############ Working on NaiveBayes Regression
# load all required libraries
library(e1071) # Loading library to run Naive Bayes
library(caret) # Library for stratified sampling and confusion matrix creation

# Setup working directory
setwd("/Users/nirdoshagarwal/Documents/INSOFE/CUTe/CSE7302c_CUTe01_Exam-Files/data/Group13-Cute2")

# Read final data file
raw <- read.csv("Group13-Cute2-Final-Data.csv", header = T, sep = ",")
head(raw)

# Converting categorical variables into factors
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

# Getting median of data
median(raw$TotalRevenueGenerated)

# Using median of data to categorize target variables in two categories
# Used median because of way too many 0s in data
raw$TotalRevenueCat <- as.factor(ifelse(raw$TotalRevenueGenerated < 7.50, "Cat1","Cat2"))

# Checking total count of data points of each category
sum(raw$TotalRevenueCat == 'Cat1')
sum(raw$TotalRevenueCat == 'Cat2')

# Filtering variables from data
rawFiltered <- subset(raw, select = -c(CONTACT_WID, NominationDate, OveralllastTransaction,CHILD_AGE_RANGE
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

# Checking if there are any NAs in the data
sum(is.na(rawFiltered))

# Separate target variable from data
data <- subset(rawFiltered, select = -c(TotalRevenueGenerated))
str(data)

# Divide data into train and test data based on 70 - 30 ratio
# We are using stratified sampling to have same ratio of target variable in train and test data
set.seed(9983)
datapart <- createDataPartition(data$TotalRevenueCat, times = 1, p = 0.7, list = F)
traindata <- data[datapart,]
testdata <- data[-datapart,]

# Running Naive Bayes algorithm
NaiveBayesModel <-naiveBayes(traindata$TotalRevenueCat ~ . ,data=traindata)
summary(NaiveBayesModel)

# Prediction on train data using model
trainPredictions <- predict(NaiveBayesModel, subset(traindata, select = -c(TotalRevenueCat)), type = "class")
pred <- prediction(as.numeric(trainPredictions), as.numeric(traindata$TotalRevenueCat))

# Checking performance of model on train data
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col = rainbow(10), colorize = T, print.cutoffs.at = seq(0, 1, 0.05))
perf_auc <- performance(pred, measure = "auc")
auc <- perf_auc@y.values[[1]]
print(auc)

# Prediction on test data using model
testPredictions <- predict(NaiveBayesModel, subset(testdata, select = -c(TotalRevenueCat)))

# Prepraing confusion matrix on train and test predictions against target variable
trainCM <- confusionMatrix(trainPredictions, traindata$TotalRevenueCat, positive = "Cat1")
# Accuracy on train is: 0.8863
testCM <- confusionMatrix(testPredictions, testdata$TotalRevenueCat, positive = "Cat1")
# Accuracy on test is: 0.8848