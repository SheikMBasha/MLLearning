# Cleanup environment
rm(list = ls(all = TRUE)) # Dont remove error_dfs

# Load required libraries
library(vegan)
library(dummies)
library(caret)
library(e1071)
library(DMwR)

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

# Taking out target variable separately
target <- rawFiltered$TotalRevenueGenerated;
rawFiltered$TotalRevenueGenerated <- NULL;

# Separating categorical variables
cat_data <- subset(rawFiltered, select = c(NumHouseChildren, FavSourceBin, FavGameBin,
                                              StrengthOfFavoriteGame))

# Separating numerical variables
num_data <- subset(rawFiltered, select = -c(NumHouseChildren, FavSourceBin, FavGameBin,
                                          StrengthOfFavoriteGame))

# Standardizing numerical data
num_data <- decostand(num_data, "range")

# Convert all categorical attributes to numeric using dummy function
children <- dummy(rawFiltered$NumHouseChildren)
favSource <- dummy(rawFiltered$FavSourceBin)
favGame <- dummy(rawFiltered$FavGameBin)
strength <- dummy(rawFiltered$StrengthOfFavoriteGame)

# Merge all data
final_Data <- cbind(num_data, children, favSource, favGame, strength, target)
rm(children, favSource, favGame, strength, num_data, cat_data)

# Rename target column to TotalRevenueGenerated
colnames(final_Data)[ncol(final_Data)] <- c('TotalRevenueGenerated')

# Checking the structure of the dataset
str(final_Data)

# Split the dataset into test and train
set.seed(9983)
train <- createDataPartition(final_Data$TotalRevenueGenerated, times = 1, p = 0.7, list = F)
train_data <- final_Data[train,]
test_data <- final_Data[-train,]

# Decoupling target column
train_target <- train_data$TotalRevenueGenerated
test_target <- test_data$TotalRevenueGenerated
train_data$TotalRevenueGenerated <- NULL
test_data$TotalRevenueGenerated <- NULL

# Building the SVM model
model = svm(train_data, train_target, method = "eps-regression", kernel = "linear", cost = 10, gamma = 0.1)
summary(model)

# Predict the values on train and test data
preds_train <- predict(model, train_data)
preds_test <- predict(model, test_data)

# Error verification on Train and Test data
error_df_svm <- data.frame(SVM_Reg1_Train = regr.eval(train_target, preds_train))
error_df_svm <- data.frame(error_df_svm, SVM_Reg1_Test = regr.eval(test_target, preds_test))