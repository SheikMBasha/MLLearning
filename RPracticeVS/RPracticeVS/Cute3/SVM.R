#Clear Environment variables
rm(list = ls(all = T))

#get\\set working directory
getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Cute3")

# add library references
library(caret)
library(DMwR)
library(vegan)
library(e1071)

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

summary(rawFiltered)
str(rawFiltered)
nlevels(rawFiltered$FavChannel180Bin)

numericVars <- rawFiltered[, sapply(rawFiltered, is.numeric)]
numericVarsWithoutTarget <- subset(numericVars, select = -c(TotalRevenueGenerated))
target <- data.frame(TotalRevenueGenerated = rawFiltered$TotalRevenueGenerated)
catVars <- rawFiltered[, sapply(rawFiltered, is.factor)]

#catVarsDummyNeeded <- catVars[, sapply(catVars, nlevels) > 2]
#catVarsDummyNotNeeded <- catVars[, sapply(catVars, nlevels) <= 2]
#catVarsDummyNeeded <- data.frame(model.matrix(~., data = catVarsDummyNeeded, contrasts.arg = lapply(catVarsDummyNeeded, contrasts, contrasts = FALSE))[, -1])

catVars <- data.frame(model.matrix(~., data = catVars, contrasts.arg = lapply(catVars, contrasts, contrasts = FALSE))[, -1])
numericVarsScaled <- decostand(numericVarsWithoutTarget, method = "range")
summary(numericVarsScaled)
final_df <- data.frame(numericVarsScaled, catVars, target)

##Applying Square transformation
final_df$TotalRevenueGenerated <- sqrt(final_df$TotalRevenueGenerated)

##Set seed and split data into test and train
set.seed(123)
datapart <- createDataPartition(final_df$TotalRevenueGenerated, times = 1, p = 0.7, list = F)
train <- final_df[datapart,]
test <- final_df[-datapart,]
##? rpart

train_x <- subset(train, select = -c(TotalRevenueGenerated))
train_y <- subset(train, select = c(TotalRevenueGenerated))
test_x <- subset(test, select = -c(TotalRevenueGenerated))
test_y <- subset(test, select = c(TotalRevenueGenerated))

SVMDefaultModel <- svm(TotalRevenueGenerated ~ ., data = train, kernel = "linear")#, cost = 10, gamma = 0.1)