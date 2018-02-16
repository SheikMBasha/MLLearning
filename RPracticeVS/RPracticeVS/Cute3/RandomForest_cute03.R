rm(list = ls(all.names = TRUE))

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

#Check Summary ans structure of the filtered data
summary(rawFiltered)
str(rawFiltered)

#separate numerical and categorical variables
#numericVars <- rawFiltered[, sapply(rawFiltered, is.numeric)]
#numericVarsWithoutTarget <- subset(numericVars, select = -c(TotalRevenueGenerated))
#target <- data.frame(TotalRevenueGenerated = rawFiltered$TotalRevenueGenerated)
#catVars <- rawFiltered[, sapply(rawFiltered, is.factor)]

#catVars <- data.frame(model.matrix(~., data = catVars, contrasts.arg = lapply(catVars, contrasts, contrasts = FALSE))[, -1])

#standardize numerical Variables
#numericVarsScaled <- decostand(numericVarsWithoutTarget, method = "range")
#summary(numericVarsScaled)

# Make dataframe with standardized numerical variables, categorical Variables and target variable
#final_df <- data.frame(numericVarsWithoutTarget, catVars, target)
#str(final_df)

final_dfTransform <- rawFiltered

# sqrt Transformation to target variable 
#final_df$TotalRevenueGeneratedT <- sqrt(final_df$TotalRevenueGenerated)
#final_dfTransform <- subset(final_df, select = -c(TotalRevenueGenerated))
#summary(final_dfTransform)
#colnames(final_dfTransform)
# Split Data into Train & Test 
set.seed(123)
datapart <- createDataPartition(final_dfTransform$TotalRevenueGenerated, times = 1, p = 0.7, list = F)
pre_train <- final_dfTransform[datapart,]
pre_test <- final_dfTransform[-datapart,]

#colnames(final_dfTransform)
# Standardize all the real valued variables in the dataset as some models we use might be impacted due to non standardized variables
# Let's use the preProcess() function from the caret package to standardize the variables, using just the data points in the training data

std_method <- preProcess(pre_train, method = c("center", "scale"))
train_data <- predict(std_method, pre_train)
test_data <- predict(std_method, pre_test)

train_target <- train_data$TotalRevenueGenerated
train_data$TotalRevenueGenerated <- NULL
test_target <- test_data$TotalRevenueGenerated
test_data$TotalRevenueGenerated <- NULL

## Building Multiple Models with Random Forest

##Build model with Random Forest  with 20 random variable and 20 trees i.e.
#ntree = 20, mtry = 20

model_rf2 <- randomForest(train_target ~ ., train_data, ntree = 20, mtry = 20)
importance(model_rf2)
varImpPlot(model_rf2)

pred_train2 <- predict(model_rf2, train_data)
pred_test2 <- predict(model_rf2, test_data)

ErrorFrame = data.frame(Train20_20 = regr.eval(train_target, pred_train2))
ErrorFrame = data.frame(ErrorFrame, Test20_20 = regr.eval(test_target, pred_test2))

##Build model with Random Forest  with 45 random variable and 20 trees i.e.
#ntree = 20, mtry = 45
model_rf11 <- randomForest(train_target ~ ., train_data, ntree = 20, mtry = 45)
importance(model_rf11)
varImpPlot(model_rf11)

pred_train1 <- predict(model_rf11, train_data)
pred_test1 <- predict(model_rf11, test_data)

ErrorFrame = data.frame(ErrorFrame, Train20_45 = regr.eval(train_target, pred_train1))
ErrorFrame = data.frame(ErrorFrame, Test20_45 = regr.eval(test_target, pred_test1))

##Build model with Random Forest  with 20 random variable and make 20 trees i.e.
#ntree = 45, mtry = 45
model_rf <- randomForest(train_target ~ ., train_data, ntree = 45, mtry = 45)
importance(model_rf)
varImpPlot(model_rf)

pred_train <- predict(model_rf, train_data)
pred_test <- predict(model_rf, test_data)
#pred_test_prob <- predict(model_rf, test_data, type = "prob")
ErrorFrame = data.frame(ErrorFrame, Train45_45 = regr.eval(train_target, pred_train))
ErrorFrame = data.frame(ErrorFrame, Test45_45 = regr.eval(test_target, pred_test))