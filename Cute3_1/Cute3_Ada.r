# Cleanup environment
rm(list = setdiff(ls(), c("error_df_linear", "error_df_cart", "error_df_rf"))) 
# Dont remove error_dfs

# Load required libraries
library(vegan)
library(dummies)
library(xgboost) 
library(caret)
library(ada)

# Setup working directory
setwd("/Users/nirdoshagarwal/Documents/INSOFE/CUTe/CUTe03")

# Read data used in Linear Regression in csv file
data <- read.csv("Cute3-Linear-Data.csv", header = T, sep = ",")
head(data)

# Check the structure of the dataset
# Observing the structure will reveal what are the data types of attributes
# It can be helpful to understand any data type changes are required
str(data)

# Check the summary of the dataset
summary(data)

# Remove columns which does not add any information
data$Country <- NULL
str(data)

# Check for Missing Values
sum(is.na(data))

# Convert few attributs to factor
data$NumHouseChildren <- as.factor(data$NumHouseChildren)
str(data)

# Convert all categorical attributes to numeric using dummy function
children <- dummy(data$NumHouseChildren)
favSource <- dummy(data$FavSourceBin)
favGame <- dummy(data$FavGameBin)
strength <- dummy(data$StrengthOfFavoriteGame)
final_Data <- cbind(data[,!names(data) %in% c("NumHouseChildren","FavSourceBin","FavGameBin",
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