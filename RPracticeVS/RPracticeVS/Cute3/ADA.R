# # Installation. 
# # Note: Windows user will need to install Rtools first
# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

rm(list = ls(all = TRUE))
# install.packages("dummies")

# Load required libraries
library(vegan)
library(dummies)
library(xgboost)


getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Cute3")


# Read the data using csv file
raw <- read.csv("Group13-Cute2-Final-Data.csv", header = T, sep = ",")

#removing rows having Target varialbe as 0
raw <- raw[raw$TotalRevenueGenerated > 0,]


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

rawFiltered <- subset(raw, select = -c(CONTACT_WID, NominationDate, OveralllastTransaction, CHILD_AGE_RANGE
                                       , FavoriteChannel, FavoriteChannel7, FavoriteChannel30, FavoriteChannel90, FavoriteChannel180, FavoriteChannel360,
                                       FavoriteSource, FavoriteSource7, FavoriteSource30, FavoriteSource90, FavoriteSource180, FavoriteSource360,
                                       FavoriteGame, FavoriteGame7, FavoriteGame30, FavoriteGame90, FavoriteGame180, FavoriteGame360))

#rawFiltered <- subset(rawFiltered, select = -c(FavSource7Bin, FavSource30Bin, FavSource90Bin, FavSource180Bin, FavSource360Bin
                                               #, FavChannel7Bin, FavChannel30Bin, FavChannel90Bin, FavChannel180Bin, FavChannel360Bin
                                               #, FavGame7Bin, FavGame30Bin, FavGame90Bin, FavGame180Bin, FavGame360Bin))

#rawFiltered <- subset(rawFiltered, select = -c(Units7, Units30, Units90, Units180, FrequencyLF7, FrequencyLF90, FrequencyLF180, FrequencyLF360,
                                               #FrequencyApp7, FrequencyApp90, FrequencyApp180, FreqGamePlay30, FreqGamePlay90, FreqGamePlay180,
                                               #Revenue7, Revenue90, TotalTimeGamePlay, TotalTimeGamePlay7, TotalTimeGamePlay30, TotalTimeGamePlay90, TotalTimeGamePlay180,
                                               #TotalTimeGamePlay360, NumGamesPlayed7, NumGamesPlayed30, NumGamesPlayed90, RecencyLF90, RecencyLF180))

#rawFiltered <- subset(rawFiltered, select = -c(FreqGamePlay7, NumGamesBought, RecencyApp7))
#rawFiltered <- subset(rawFiltered, select = -c(RecencyApp30, RecencyApp90, RecencyApp180, Recencydown7, Recencydown30, Recencydown90, Recencydown180,
                                               #maxRecencyCum7, minRecencyCum7, maxRecencyCum30, minRecencyCum30, maxRecencyCum90, minRecencyCum90, maxRecencyCum180, minRecencyCum180))

#rawFiltered <- subset(rawFiltered, select = -c(NumFemaleChildrenHousehold, NumMaleChildrenHousehold))
#rawFiltered <- subset(rawFiltered, select = -c(NumGamesPlayed180, minRecencyCum360, FavChannelBin))

rawFiltered$TotalRevenueGenerated <-  log(rawFiltered$TotalRevenueGenerated)

numericVars <- rawFiltered[, sapply(rawFiltered, is.numeric)]
numericVarsWithoutTarget <- subset(numericVars, select = -c(TotalRevenueGenerated))
target <- data.frame(TotalRevenueGenerated = rawFiltered$TotalRevenueGenerated)
catVars <- rawFiltered[, sapply(rawFiltered, is.factor)]

catVars <- data.frame(model.matrix(~., data = catVars, contrasts.arg = lapply(catVars, contrasts, contrasts = FALSE))[, -1])
#numericVarsScaled <- decostand(numericVarsWithoutTarget, method = "range")
#summary(numericVarsScaled)
final_df <- data.frame(numericVarsWithoutTarget, catVars, target)

#------------------------------------------------------

str(final_df)
summary(final_df)

#############################################################
library(caret)
set.seed(1234)

index_train <- createDataPartition(final_df$TotalRevenueGenerated, p = 0.7, list = F)

pre_train <- final_df[index_train,]
pre_test <- final_df[-index_train,]

# Decoupling target column
train_target <- pre_train$TotalRevenueGenerated
test_target <- pre_test$TotalRevenueGenerated
pre_train$TotalRevenueGenerated <- NULL
pre_test$TotalRevenueGenerated <- NULL

# Standardize all the real valued variables in the dataset as some models we use might be impacted due to non standardized variables
std_method <- preProcess(pre_train, method = c("center", "scale"))
train_Data <- predict(std_method, pre_train)
test_Data <- predict(std_method, pre_test)
# Let's use the preProcess() function from the caret package to standardize the variables, using just the data points in the training data




#############################################################################

# Check how records are split with respect to target attribute.
#table(final_Data$loan)
#table(train_target)
#table(test_target)
#rm(final_Data)
?ada
library(ada)
model = ada(train_target ~ ., iter = 20, data = train_Data, loss = "logistic")

################
#train_Data = subset(train_Data, select = -c(Country))
dtrain = xgb.DMatrix(data = as.matrix(train_Data),label = train_target)

dtest = xgb.DMatrix(data = as.matrix(test_Data), label = test_target)

?xgboost

#model = xgboost(data = dtrain, max.depth = 4,
                #eta = 0.4, nthread = 2, nround = 40,
                #objective = "reg:linear", verbose = 1)

watchlist = list(train = dtrain, test = dtest)
?xgb.train
model = xgb.train(data = dtrain, max.depth = 3,
                  eta = 0.3, nthread = 2, nround = 700,
                  watchlist = watchlist,
                  eval.metric = "rmse",
                  objective = "reg:linear", verbose = 1)

importance <- xgb.importance(feature_names = names(train_Data), model = model)
print(importance)
xgb.plot.importance(importance_matrix = importance)

pred_train <- predict(model, as.matrix(train_Data))
pred_test <- predict(model, as.matrix(test_Data))

#library(DMwR)
regr.eval(train_target, pred_train)
regr.eval(test_target, pred_test)