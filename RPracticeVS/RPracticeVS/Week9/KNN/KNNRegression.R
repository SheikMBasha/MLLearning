# Clear the environment
rm(list = ls(all = T))

# 1. Install packages and load libraries
# install.packages("FNN") #"Fast Nearest Neighbours" 
# install.packages("Metrics") #to calculate error metrics for regression
library(FNN)
library(Metrics)

# 2. Generate data for regression
set.seed(12345) 

# Create a dataframe of 100 rows and 25 columns
# Min value is 24 and Max value is 65
# 3. Target is x25
data <- data.frame(matrix(data = runif(2500, 24, 65), nrow = 100, ncol = 25))

# 4. Split the data into train and test based on 70-30 ratio
set.seed(9983)
train_part <- createDataPartition(data$X25, p = 0.7, times = 1, list = F)
train_data <- data[train_part,]
test_data <- data[-train_part,]

# 5. Separating independent attributes and target attribute into two different data frames
train_data_without_target <- subset(train_data, select = -c(X25))
test_data_without_target <- subset(test_data, select = -c(X25))
train_target <- subset(train_data, select = c(X25))
test_target <- subset(test_data, select = c(X25))

# 6. Run the model for k = 1 to 10
accuracy <- as.numeric()
rowNames <- NULL
for (i in 1:10) {
    pred <- knn.reg(train_data_without_target, test_data_without_target, train_target$X25, k = i)
    predictions <- data.frame(pred$pred)
    result <- rmse(test_target, predictions)
    print(result)
    accuracy <- rbind(accuracy, result)
    rowNames[i] <- paste("rmse_FNN_k", i, sep = "_") #Storing rowname for each row
}
rownames(accuracy) <- rowNames #Changing rownames of accuracy df
accuracy

#  Running KNN regresion on Universal bank problem

# Clear Environmet Variables
rm(list = ls(all = T))

#Get and Set Working Directory
getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Week9\\KNN")

#Read data from file
UniversalBankRaw <- read.csv("UniversalBank.csv", header = T, sep = ",")

#Check structure/summary of data
summary(UniversalBankRaw)
str(UniversalBankRaw)

# Check for missing values
sum(is.na(UniversalBankRaw))

# remove Id and Zip variables
# Removing unnecessary columns like ID and ZipCode
bank_data2 <- subset(UniversalBankRaw, select = -c(ID, ZIP.Code))

# Convert categorical attributes into numeric using dummyfication
library(dummies) # Library to use dummies function
bank_data2$Education <- as.factor(as.character(bank_data2$Education))
Education <- dummy(bank_data2$Education)
bank_data3 <- cbind(bank_data2, Education)
bank_data2$Family <- as.factor(as.character(bank_data2$Family))
Family <- dummy(bank_data2$Family)
bank_data3 <- cbind(bank_data3, Family)
bank_data3$Education <- NULL
bank_data3$Family <- NULL
names(bank_data3)

########## Standardizing data ##########
library(vegan) #Library to use decostand function to standardize data
std_bank_data <- decostand(bank_data3[, - c(4, 11, 14)], "range")
# CCAvg is the target variable

# Split the data into train and test based on 70-30 ratio
set.seed(9983)
train_part <- createDataPartition(bank_data3$CCAvg, p = 0.7, times = 1, list = F)
train_data <- bank_data3[train_part,]
test_data <- bank_data3[-train_part,]

# Separating independent attributes and target attribute into two different data frames
train_data_without_target <- subset(train_data, select = -c(CCAvg))
test_data_without_target <- subset(test_data, select = -c(CCAvg))
train_target <- subset(train_data, select = c(CCAvg))
test_target <- subset(test_data, select = c(CCAvg))

# 6. Run the model for k = 1 to 10
bank_accuracy <- as.numeric()
rowNames <- NULL
for (i in 1:10) {
    pred <- knn.reg(train_data_without_target, test_data_without_target, train_target$CCAvg, k = i)
    predictions <- data.frame(pred$pred)
    result <- rmse(test_target, predictions)
    print(result)
    bank_accuracy <- rbind(bank_accuracy, result)
    rowNames[i] <- paste("rmse_FNN_k", i, sep = "_") #Storing rowname for each row
}
rownames(bank_accuracy) <- rowNames #Changing rownames of accuracy df
bank_accuracy