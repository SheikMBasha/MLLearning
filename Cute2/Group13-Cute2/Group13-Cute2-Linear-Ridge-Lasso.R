############ Data extraction is done. Model building code starts from here. ###########
# load all required libraries
library(glmnet) # To run Ridge, Lasso and Elastic Net
library(DMwR) # Library for calculation errors on train and test data

# Setup working directory
setwd("/Users/nirdoshagarwal/Documents/INSOFE/CUTe/CSE7302c_CUTe01_Exam-Files/data/Group13-Cute2")

# Read final csv file
raw <- read.csv("Group13-Cute2-Final-Data.csv", header = T, sep = ",")
head(raw)
str(raw)
colnames(raw)

# Converting character variables into factors
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

############ Linear Regression model
# Filtering columns based on data understanding and running multiple iterations of Linear Regression
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
par(mfrow = c(2,2))
plot(Lin_Reg)

# Calculate Error metric on Train data for Linear Regression Model
error_df <- data.frame(LinReg_Train = regr.eval(train_data$TotalRevenueGenerated, Lin_Reg$fitted.values))

# Removing y = TotalRevenueGenerated from test_data and put in target
target <- test_data$TotalRevenueGenerated
test_data$TotalRevenueGenerated <- NULL

# Calculate Error metric on Test data for Linear Regression Model
test_prediction <- predict(Lin_Reg, test_data)
error_df <- data.frame(error_df, LinReg_Test = regr.eval(target, test_prediction))

# Running Step AIC on Linear Regression Model obtained above
Step_AIC <- stepAIC(Lin_Reg)

# Checking summary of Step AIC to see significant variable
summary(Step_AIC)

# Calculate Error metric on Train and Test data for Step AIC Model
error_df <- data.frame(error_df, StepAIC_Train = regr.eval(train_data$TotalRevenueGenerated, Step_AIC$fitted.values))
test_prediction <- predict(Step_AIC, test_data)
error_df <- data.frame(error_df, StepAIC_Test = regr.eval(target, test_prediction))

############ Running Ridge, Lasso and ElasticNet
# Separating numeric variables separately
num_data <- rawFiltered[, sapply(rawFiltered, is.numeric)]

# Removing target variable from numeric variable set
num_data_wo_target <- subset(num_data, select = -c(TotalRevenueGenerated))

# Separating categorical variables
cat_data <- rawFiltered[, sapply(rawFiltered, is.factor)]
colnames(cat_data)

# Getting Target variable in separate data frame and running dummyfication on it
target <- subset(rawFiltered, select = c(TotalRevenueGenerated))
target_binned <- model.matrix(rawFiltered$TotalRevenueGenerated ~ . , data=cat_data)[,-1]

# Divide data into train and test data based on 70 - 30 ratio
# We are using stratified sampling to have same ratio of target variable in train and test data
datapart <- createDataPartition(rawFiltered$TotalRevenueGenerated, times = 1, p = 0.7, list = F)
train1 <- data.frame(num_data_wo_target, target_binned, target)[datapart,]
test1 <- data.frame(num_data_wo_target, target_binned, target)[-datapart,]

# Final combined data
data_final <- as.matrix(data.frame(num_data_wo_target, target_binned))
traindata <- data_final[datapart,]
testdata <- data_final[-datapart,]

# Separating target variables from both train and test
ytrain <- rawFiltered$TotalRevenueGenerated[datapart]
ytest <- rawFiltered$TotalRevenueGenerated[-datapart]

############ Lasso Regression  using glmnet - L1 norm 
LassoFit <- glmnet(traindata, ytrain, alpha = 1)

# Plotting Lasso Model
plot(LassoFit, xvar="lambda", label=TRUE)
plot(LassoFit, xvar = "dev", label = TRUE)

# Model selection
cv_Lasso <- cv.glmnet(traindata, ytrain, alpha = 1)
plot(cv_Lasso)
coef(cv_Lasso)

# lambda.min - value of lambda that gives minimum cvm - Mean Cross-Validated error
cv_Lasso$lambda.min

# Fit the model to use lambda which gives the least Mean Square error
# Run Lasso with best Lambda value got above which is: 0.01425907
LassoLambdaFit <- glmnet(traindata, ytrain, alpha = 1, lambda = cv_Lasso$lambda.min)
coef(LassoLambdaFit)

# Calculate Error metric on Train and Test data for Lasso Model
error_df <- data.frame(error_df, Lasso_Train = regr.eval(ytrain, predict(LassoLambdaFit, traindata)))
error_df <- data.frame(error_df, Lasso_Test = regr.eval(ytest, predict(LassoLambdaFit, testdata)))

############ Ridge Regression using glmnet  - L2 norm
RidgeFit <- glmnet(traindata,ytrain,alpha = 0)
par(mfrow = c(1,1))
plot(RidgeFit)

# Model selection
cv_Ridge <- cv.glmnet(traindata, ytrain, alpha = 0)
coef(cv_Ridge)
plot(cv_Ridge)

# lambda.min - value of lambda that gives minimum cvm - Mean Cross-Validated error
cv_Ridge$lambda.min

# Fit the model to use lambda which gives the least Mean Square error
# Run Ridge Regression with best Lambda value got above which is: 4.669203
RidgeLambdaFit <- glmnet(traindata, ytrain, alpha = 0, lambda = cv_Ridge$lambda.min)
coef(RidgeLambdaFit)

# Calculate Error metric on Train and Test data for Ridge Model
error_df <- data.frame(error_df, Ridge_Train = regr.eval(ytrain, predict(RidgeLambdaFit, traindata)))
error_df <- data.frame(error_df, Ridge_Test = regr.eval(ytest, predict(RidgeLambdaFit, testdata)))

############ ElasticNet
# Running model with alpha = 0.5
ElasticFit <- glmnet(traindata, ytrain, alpha = 0.5)
plot(ElasticFit)

# Model selection
cv_Elastic <- cv.glmnet(traindata, ytrain,alpha = 0.5)
plot(cv_Elastic)

# lambda.min - value of lambda that gives minimum cvm - Mean Cross-Validated error
cv_Elastic$lambda.min

# Fit the model to use lambda which gives the least Mean Square error
# Run Elastic Net with best Lambda value got above which is: 0.02367626
ElasticLambdaFit <- glmnet(traindata, ytrain, alpha = 0.5, lambda = cv_Elastic$lambda.min)
coef(ElasticLambdaFit)

# Calculate Error metric on Train and Test data for Elastic Net Model
error_df <- data.frame(error_df, Elastic_Train = regr.eval(ytrain, predict(ElasticLambdaFit, traindata)))
error_df <- data.frame(error_df, Elastic_Test = regr.eval(ytest, predict(ElasticLambdaFit, testdata)))