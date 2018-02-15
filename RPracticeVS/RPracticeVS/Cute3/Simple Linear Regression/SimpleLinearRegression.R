#Clear Environment variables
rm(list = ls(all = T))

#get\\set working directory
getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Cute3\\Simple Linear Regression")

# add library references
library(caret)
library(DMwR)

#read data file
raw <- read.csv("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Cute3\\Group13-Cute2-Final-Data.csv", header = T, sep = ",")

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

#table(rawFiltered$TotalRevenueGenerated)
# Adding a constant 1 to all rows of target variable
#rawFiltered$TotalRevenueGenerated <- rawFiltered$TotalRevenueGenerated + 3
rawFiltered <- rawFiltered[which(rawFiltered$TotalRevenueGenerated > 0),]

# Divide data into train and test data based on 70 - 30 ratio
# We are using stratified sampling to have same ratio of target variable in train and test data
set.seed(9983)
datapart <- createDataPartition(rawFiltered$TotalRevenueGenerated, times = 1, p = 0.7, list = F)
train_data = rawFiltered[datapart,]
test_data = rawFiltered[-datapart,]

# Running Linear Regression Model on filtered data
Lin_Reg <- lm(TotalRevenueGenerated ~ ., data = train_data)

# Checking summary of Linear Regression Model
summary(Lin_Reg)

# Plotting Linear Regression plots
#par(mfrow = c(2, 2))
#plot(Lin_Reg)

# Calculate Error metric on Train data for Linear Regression Model
error_df <- data.frame(LinReg_Train = regr.eval(train_data$TotalRevenueGenerated, Lin_Reg$fitted.values))

# Removing y = TotalRevenueGenerated from test_data and put in target
target <- test_data$TotalRevenueGenerated
test_data$TotalRevenueGenerated <- NULL

# Calculate Error metric on Test data for Linear Regression Model
test_prediction <- predict(Lin_Reg, test_data)
error_df <- data.frame(error_df, LinReg_Test = regr.eval(target, test_prediction))

#Applying Tuki's transformation

rawFiltered$TotalRevenueGeneratedT <- sqrt(rawFiltered$TotalRevenueGenerated)

rawFilterdTransform <- subset(rawFiltered, select = -c(TotalRevenueGenerated))

set.seed(9983)
datapart <- createDataPartition(rawFilterdTransform$TotalRevenueGeneratedT, times = 1, p = 0.7, list = F)
train_data_T = rawFilterdTransform[datapart,]
test_data_T = rawFilterdTransform[-datapart,]

Lin_Reg1 <- lm(TotalRevenueGeneratedT ~ ., data = rawFilterdTransform)
summary(Lin_Reg1)
#plot(Lin_Reg1)

print(head(Lin_Reg1$fitted.values))
print(head(Lin_Reg1$fitted.values^2))
error_df <- data.frame(error_df, LinReg_Train1 = regr.eval(train_data_T$TotalRevenueGenerated, Lin_Reg1$fitted.values))

# Removing y = TotalRevenueGenerated from test_data and put in target
target_T <- test_data_T$TotalRevenueGeneratedT
test_data_T$TotalRevenueGeneratedT <- NULL

# Calculate Error metric on Test data for Linear Regression Model
test_prediction_T <- predict(Lin_Reg1, test_data_T)
error_df <- data.frame(error_df, LinReg_Test1 = regr.eval(target_T, test_prediction_T))

# Store predictions from the model
preds_rf <- predict(Lin_Reg1, test_data_T)

# Predict on the train data
preds_train_rf <- predict(Lin_Reg1, train_data_T)

library(DMwR)
regr.eval(train_data_T$TotalRevenueGeneratedT, preds_train_rf)
regr.eval(target_T, preds_rf)