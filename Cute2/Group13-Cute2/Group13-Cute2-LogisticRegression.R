############ Working on Logistic Regression
# Loading required libraries
library(ROCR) # Library to see AUC curve
library(caret) # Library used to compute confusion matrix

# Setup working directory
setwd("/Users/nirdoshagarwal/Documents/INSOFE/CUTe/CSE7302c_CUTe01_Exam-Files/data/Group13-Cute2")

# Read final data file
raw <- read.csv("Group13-Cute2-Final-Data.csv", header = T, sep = ",")
head(raw)

# Binning Target variable TotalRevenueGenerated
raw$TotalRevenueCat <- as.factor(ifelse(raw$TotalRevenueGenerated < 500, "0" , "1" ))
str(raw)

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

# Filtered columns fom raw data based on understanding from Linear Regression Model
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

# Checking if there are any NAs
sum(is.na(rawFiltered))

# Filtering Target variable from data
data <- subset(rawFiltered, select = -c(TotalRevenueGenerated))
str(data)

# Divide data into train and test data based on 70 - 30 ratio
# We are using stratified sampling to have same ratio of target variable in train and test data
set.seed(9983)
datapart <- createDataPartition(data$TotalRevenueCat, times = 1, p = 0.7, list = F)
traindata <- data[datapart,]
testdata <- data[-datapart,]

# Running Logistic Model
Log_reg <-glm(traindata$TotalRevenueCat ~ ., data = traindata, family ="binomial")
summary(Log_reg)

# Predictions on train data
train_ProbPredict <- predict(Log_reg, type="response")
trainPrediction <- prediction(train_ProbPredict, traindata$TotalRevenueCat)

# Measure the performance
perf <- performance(trainPrediction, measure="tpr", x.measure="fpr")
perf_auc <- performance(trainPrediction, measure = "auc")

# Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col=rainbow(10), colorize=T, 
     print.cutoffs.at=seq(0,1,0.05))

# Getting AUC : Aread Under the Curve
auc <- perf_auc@y.values[[1]]
auc
# auc came as : 100%. We will run Logistic by tuning classes

# Choosing cut off
pred_class <- ifelse(train_ProbPredict > 0.1, "1", "0")
table(traindata$TotalRevenueCat, pred_class)

# Checking performance of model on test data
cutoffs <- data.frame(cut = perf@alpha.values[[1]], fpr = perf@x.values[[1]], tpr = perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing = TRUE),]
prob_test <- predict(Log_reg, testdata, type = "response")
preds_test <- ifelse(prob_test > 0.1, "1", "0")
table(testdata$TotalRevenueCat, preds_test)
confusionMatrix(preds_test, testdata$TotalRevenueCat, positive = "1")
# Accuracy: 0.9991