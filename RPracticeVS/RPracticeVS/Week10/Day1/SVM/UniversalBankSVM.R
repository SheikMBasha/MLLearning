# Clear Environmet Variables
rm(list = ls(all = T))

#Get and Set Working Directory
getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Week10\\Day1\\SVM")

#Library References
library(MASS)
library(caret)
library(vegan)
library(dummies)
library(e1071)

#Read data from file
UniversalBankRaw <-  read.csv("UniversalBank.csv", header = T, sep = ",")

#Check structure of data
str(UniversalBankRaw)

# remove Id and Zip variables
UniversalBankRaw <- subset(UniversalBankRaw, select = -c(ZIP.Code, ID))

#Convert Education and Family into factors
UniversalBankRaw$Education <- as.factor(UniversalBankRaw$Education)
UniversalBankRaw$Family <- as.factor(UniversalBankRaw$Family)

str(UniversalBankRaw)

#Separating numeric and categorical variables and target variables
numericVars <- UniversalBankRaw[, sapply(UniversalBankRaw, is.numeric)]
numericVars$Personal.Loan <- NULL
catVars <- UniversalBankRaw[, sapply(UniversalBankRaw, is.factor)]
target <- as.factor(UniversalBankRaw$Personal.Loan)

#Standaradize numeric variables
numericStd <- decostand(numericVars, "range")

#Dummify categorical variables
edu <- dummy(UniversalBankRaw$Education)
family <- dummy(UniversalBankRaw$Family)

#Finally merging numeric and categorical variables
final_data <- data.frame(numericStd, edu,family,target)

#Rename target variable
colnames(final_data)[ncol(final_data)] <- c('PersonalLoan')

#Split data into test and train
set.seed(123)
datapart <- createDataPartition(final_data$PersonalLoan, times = 1, p = 0.7, list = F)
train <- final_data[datapart,]
test <- final_data[-datapart,]

#Removing variables
rm(catVars, datapart, edu, family, numericStd, numericVars, target, UniversalBankRaw)
final_data$Personal.Loan <-  NULL

#Model building
train_x <- subset(train, select = -c(PersonalLoan))
train_y <- subset(train, select = c(PersonalLoan))
test_x <- subset(test, select = -c(PersonalLoan))
test_y <- subset(test, select = c(PersonalLoan))

#Default SVM Model without tuning 

SVMDefaultModel <- svm(PersonalLoan ~ ., data = train, kernel = "linear", cost = 10, gamma = 0.1)
summary(SVMDefaultModel)

# Predict the values on train and test data
preds_train <- predict(SVMDefaultModel, train_x)
preds_test <- predict(SVMDefaultModel, test_x)

#Confusion matrix for train and test
confusionMatrix(preds_train, train$PersonalLoan, positive = "1") # Balanced accuracy: 82.66%
confusionMatrix(preds_test, test$PersonalLoan, positive = "1") # Balanced accuracy: 86.36%


#Tuning
tuned <- tune.svm(PersonalLoan ~ ., data = train, gamma = 10 ^ (-6:-1), cost = 10 ^ (1:2))
print(tuned)

#Best model
tunedModel <- tuned$best.model

#prediction on train and test
preds_train <- predict(tunedModel, train_x)
preds_test <- predict(tunedModel, test_x)

#Confusion matrix with tunedModel
confusionMatrix(preds_train, train$PersonalLoan, positive = "1") # Balanced accuracy: 95.45%
confusionMatrix(preds_test, test$PersonalLoan, positive = "1") # Balanced accuracy: 94.80%