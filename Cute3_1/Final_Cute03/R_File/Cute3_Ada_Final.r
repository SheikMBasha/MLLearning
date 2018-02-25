rm(list = ls(all = TRUE))

getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Cute3")

# Load required libraries
library(vegan)
library(dummies)
library(xgboost) 
library(caret)
library(ada)

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

rawFiltered <- subset(rawFiltered, select = -c(MAX_AGE, RecencyApp360, Recencydown, Recencydown360, maxRecencyCum))

# Remove columns which does not add any information
rawFiltered$Country <- NULL
str(rawFiltered)

# Check for Missing Values
sum(is.na(rawFiltered))

# Convert few attributs to factor
rawFiltered$NumHouseChildren <- as.factor(rawFiltered$NumHouseChildren)
str(rawFiltered)

# Convert all categorical attributes to numeric using dummy function
children <- dummy(rawFiltered$NumHouseChildren)
favSource <- dummy(rawFiltered$FavSourceBin)
favGame <- dummy(rawFiltered$FavGameBin)
strength <- dummy(rawFiltered$StrengthOfFavoriteGame)
final_Data <- cbind(rawFiltered[, !names(rawFiltered) %in% c("NumHouseChildren", "FavSourceBin", "FavGameBin",
                                              "StrengthOfFavoriteGame")],
                                              children, favSource, favGame, strength)
rm(children, favSource, favGame, strength)

# Checking the structure of the dataset
str(final_Data)

# Bin the target variable TotalRevenuGenerated
final_Data$TotalRevenueGenerated <- LinearScaling(final_Data$TotalRevenueGenerated)
final_Data$CustCategory <- ifelse(final_Data$TotalRevenueGenerated < 0.5, "Regular", "Premimum")
final_Data$TotalRevenueGenerated <- NULL

# Split the dataset into test and train
set.seed(9983)
train <- createDataPartition(final_Data$CustCategory, times = 1, p = 0.7, list = F)
train_data <- final_Data[train,]
test_data <- final_Data[-train,]

# Decoupling target column
train_target <- train_data$CustCategory
test_target <- test_data$CustCategory
train_data$CustCategory <- NULL
test_data$CustCategory <- NULL

# Standardize all the real valued variables in the dataset as some models we use might be impacted due to non standardized variables
# Let's use the preProcess() function from the caret package to standardize the variables, using just the data points in the training data
std_method <- preProcess(train_data, method = c("center", "scale"))
train_data <- predict(std_method, train_data)
test_data <- predict(std_method, test_data)

# Check how records are split with respect to target attribute.
table(final_Data$CustCategory)
table(train_target)
table(test_target)
rm(final_Data)

####### ADA BOOST #######
# Run ADA Boost using ada function from R
model = ada(train_target ~ ., iter = 20, data = train_data, loss="logistic") 
# iter = 20 Iterations 
model

# predict the values using model on test data sets. 
test_pred <- predict(model, test_data)

# Creating confusion matrix for test predictions
confusionMatrix(test_target, test_pred) # Balanced accuracy: 94.97%