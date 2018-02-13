# Clear Environmet Variables
rm(list = ls(all = T))

#Get and Set Working Directory
getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Week9\\KNN")

#Library References
library(MASS)
library(caret)
library(vegan)
library(dummies)
library(e1071)
library(class)

#Read data from file
UniversalBankRaw <- read.csv("UniversalBank.csv", header = T, sep = ",")

#Check structure/summary of data
str(UniversalBankRaw)
summary(UniversalBankRaw)

# Check for missing values
sum(is.na(UniversalBankRaw))

# remove Id and Zip variables
UniversalBankRaw <- subset(UniversalBankRaw, select = -c(ZIP.Code, ID))

#Convert Education and Family into factors
UniversalBankRaw$Education <- as.factor(UniversalBankRaw$Education)
UniversalBankRaw$Family <- as.factor(UniversalBankRaw$Family)

#Separating numeric and categorical variables and target variables
numericVars <- UniversalBankRaw[, sapply(UniversalBankRaw, is.numeric)]
numericVars$Personal.Loan <- NULL
catVars <- UniversalBankRaw[, sapply(UniversalBankRaw, is.factor)]
target <- as.factor(UniversalBankRaw$Personal.Loan)

#Dummify categorical variables
edu <- dummy(UniversalBankRaw$Education)
family <- dummy(UniversalBankRaw$Family)

#Finally merging numeric and categorical variables
final_data <- data.frame(numericVars, edu, family, target)

#Rename target variable
colnames(final_data)[ncol(final_data)] <- c('PersonalLoan')

#Split data into test and train
set.seed(123)
datapart <- createDataPartition(final_data$PersonalLoan, times = 1, p = 0.7, list = F)
train <- final_data[datapart,]
test <- final_data[-datapart,]

#Removing variables
final_data$Personal.Loan <- NULL

#Check distribution of target variable
table(final_data$PersonalLoan)
table(train$PersonalLoan)
table(test$PersonalLoan)

#KNN Needs data without target variable
#remove target variable

train_WithoutClass <- subset(train, select = -c(PersonalLoan))
test_WithoutClass <- subset(test, select = -c(PersonalLoan))

#Run knn with k = 1..10 and record accuracy
accuracy <- as.numeric()
rowNames <- NULL
for (i in 1:10) {
    pred <- knn(train_WithoutClass, test_WithoutClass, train$PersonalLoan, k = i)
    a <- table(pred, test$PersonalLoan)
    acc_wo_stand <- sum(diag(a)) / nrow(train_WithoutClass)
    print(acc_wo_stand)
    accuracy = rbind(accuracy, acc_wo_stand)
    rowNames[i] <- paste("acc_wo_stand_k", i, sep = "_") #Storing rowname for each row
}
rownames(accuracy) <- rowNames #Changing rownames of accuracy df
accuracy

# With Standardization
final_data_std <- decostand(subset(final_data, select = -c(PersonalLoan)), "range")
final_data_std <- data.frame(final_data_std, PersonalLoan= final_data$PersonalLoan)

str(final_data_std)

#Split data
set.seed(123)
datapart <- createDataPartition(final_data_std$PersonalLoan, times = 1, p = 0.7, list = F)
train1 <- final_data_std[datapart,]
test1 <- final_data_std[-datapart,]

train1_WithoutClass <- subset(train1, select = -c(PersonalLoan))
test1_WithoutClass <- subset(test1, select = -c(PersonalLoan))

# Run KNN with standardized data
accuracy2 <- as.numeric()
rowNames2 <- NULL
for (i in 1:10) {
    pred <- knn(train1_WithoutClass, test1_WithoutClass, train1$PersonalLoan, k = i)
    a <- table(pred, test1$PersonalLoan)
    acc_wo_stand <- sum(diag(a)) / nrow(train1_WithoutClass)
    print(acc_wo_stand)
    accuracy2 = rbind(accuracy2, acc_wo_stand)
    rowNames2[i] <- paste("acc_wo_stand_k", i, sep = "_") #Storing rowname for each row
}
rownames(accuracy2) <- rowNames #Changing rownames of accuracy df
accuracy2

#Merge output frames of accuracy
accuracy_df <- cbind(accuracy, accuracy2)
for (i in 1:10) {
    rowNames[i] <- paste("acc_k", i, sep = "_")
}
rownames(accuracy_df) <- rowNames
colnames(accuracy_df) <- c('Without Std', 'With Std')

# 8. Reduce model complexity with condensation (Wilson Editing)
keep <- condense(train_WithoutClass, train$PersonalLoan)
length(keep)
head(keep)

# 9. Use condensed data to generate model
accuracy3 <- as.numeric()
rowNames <- NULL
for (i in 1:10) {
    pred <- knn(train_WithoutClass[keep,], test_WithoutClass, train$PersonalLoan[keep], k = i)
    a <- table(pred, test$PersonalLoan)
    acc_w_condense <- sum(diag(a)) / nrow(test_WithoutClass)
    print(acc_w_condense)
    accuracy3 = rbind(accuracy3, acc_w_condense)
    rowNames[i] <- paste("acc_w_condense_k", i, sep = "_") #Storing rowname for each row
}
rownames(accuracy3) <- rowNames #Changing rownames of accuracy df
accuracy3

# Merging accuracy with condense df to actual accuracy data frame
accuracy_df <- cbind(accuracy_df, accuracy3)
colnames(accuracy_df)[ncol(accuracy_df)] <- c('With Condense')

#Run the model using FNN library
#install.packages("FNN")
library(FNN) #"Fast Nearest Neighbours" for knn regression
accuracy4 <- as.numeric()
rowNames <- NULL
for (i in 1:10) {
    pred <- FNN::knn(train_WithoutClass[keep,], test_WithoutClass, train$PersonalLoan[keep], k = i)
    a <- table(pred, test$PersonalLoan)
    acc_FNN <- sum(diag(a)) / nrow(test_WithoutClass)
    print(acc_FNN)
    accuracy4 = rbind(accuracy4, acc_FNN)
    rowNames[i] <- paste("acc_FNN_k", i, sep = "_") #Storing rowname for each row
}
rownames(accuracy4) <- rowNames #Changing rownames of accuracy df
accuracy4

# Merging accuracy with condense df to actual accuracy data frame
accuracy_df <- cbind(accuracy_df, accuracy4)
colnames(accuracy_df)[ncol(accuracy_df)] <- c('FNN')

# 10.2. Check data of the 5 nearest neighbors for row 20 of test data set
indices <- knnx.index(train_WithoutClass[keep,], test_WithoutClass, k = 5)
print(indices[20,])