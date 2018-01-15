rm(list=ls(all=T))

getwd()
setwd("E://Insofe//Week6//Day2//Labwork//20170107_Batch37_CSE7302c_LogisticRegression_Activity")

bank_data <- read.table("bank.txt", sep = ";", header = T)
#rm(bank_data)
str(bank_data)
summary(bank_data)

library(e1071)
?naiveBayes

rows <- seq(1, nrow(bank_data),1)
set.seed(123)
trainRows <- sample(rows, 70*nrow(bank_data)/100)
bank_train <- bank_data[trainRows,]
bank_test <- bank_data[-trainRows,]

#NaiveBayes on train data

trainDataModel <- naiveBayes( y ~ ., data = bank_train)


train_predictions_raw <- predict(trainDataModel, bank_train[-ncol(bank_train)], type = "raw")
train_predictions <- predict(trainDataModel, bank_train[-ncol(bank_train)], type = "class")


#NaiveBayes on test data
test_predictions <- predict(trainDataModel, bank_test[-ncol(bank_test)], type = "class")

library(caret)

confusionMatrix(train_predictions, bank_train$y, positive = "yes")
confusionMatrix(test_predictions, bank_test$y, positive = "yes")


# second try: removing contact column as looks like its insignificant

bank_data_second <- bank_data

bank_data_second <- subset(bank_data_second, select=-c(contact))


library(caTools)
rows_second <- seq(1, nrow(bank_data_second),1)
set.seed(123)
bank_second_trainRows <- sample(rows_second, 70*nrow(bank_data_second)/100)
bank_data_second_train <- bank_data_second[bank_second_trainRows,]
bank_data_second_test <- bank_data_second[-bank_second_trainRows,]

secondAttemptModelTrain <- naiveBayes(y ~ ., data=bank_data_second_train)


predictSecondAttemptModelTrain <- predict(secondAttemptModelTrain, bank_data_second_train[-ncol(bank_data_second_train)])
predictSecondAttemptModelTest <- predict(secondAttemptModelTrain, bank_data_second_test[-ncol(bank_data_second_test)])

confusionMatrix(predictSecondAttemptModelTrain, bank_data_second_train$y, positive = "yes")
confusionMatrix(predictSecondAttemptModelTest, bank_data_second_test$y, positive = "yes")


#Third attempt: Categorizing age

rm(bank_data_third)

bank_data_third <- bank_data
bank_data_third$ageCategory <- as.factor(ifelse(bank_data$age >= 0 & bank_data$age <= 30, 
       'Young',ifelse(bank_data$age >30 & bank_data$age <= 60 , "Middle", "old"  )))

bank_data_third
str(bank_data_third)

bank_data_third <- subset(bank_data_third, select = -c(age,contact))
str(bank_data_third)


third_rows <- seq(1, nrow(bank_data_third),1)
set.seed(123)
third_train_rows <- sample(third_rows, 70*nrow(bank_data_third)/100)
bank_data_third_train <- bank_data_third[third_train_rows,]
bank_data_third_test <- bank_data_third[-third_train_rows,]

library(e1071)
third_trainDataModel <- naiveBayes( y ~ ., data=bank_data_third_train)

?predict
third_trainDataModelPredictions <- predict(third_trainDataModel, subset(bank_data_third_train, select = -c(y)))
third_testDataModelPredictions <- predict(third_trainDataModel, subset(bank_data_third_test, select = -c(y)))

library(caret)
confusionMatrix(third_trainDataModelPredictions, bank_data_third_train$y, positive="yes" )
confusionMatrix(third_testDataModelPredictions, bank_data_third_test$y, positive = "yes")


#Fourth attempt: Categorizing pdays

rm(bank_data_fourth)

bank_data_fourth <- bank_data
bank_data_fourth$pdaysCategory <- as.factor(ifelse(bank_data$pdays < 0, 
                                                'Not Contacted',ifelse(bank_data$pdays <=90 , "Recently contacted", "Long back contacted"  )))

bank_data_fourth
str(bank_data_fourth)

bank_data_fourth <- subset(bank_data_fourth, select = -c(pdays))
str(bank_data_fourth)


fourth_rows <- seq(1, nrow(bank_data_fourth),1)
set.seed(123)
fourth_train_rows <- sample(fourth_rows, 70*nrow(bank_data_fourth)/100)
bank_data_fourth_train <- bank_data_fourth[fourth_train_rows,]
bank_data_fourth_test <- bank_data_fourth[-fourth_train_rows,]

library(e1071)
fourth_trainDataModel <- naiveBayes( y ~ ., data=bank_data_fourth_train)

?predict
fourth_trainDataModelPredictions <- predict(fourth_trainDataModel, subset(bank_data_fourth_train, select = -c(y)))
fourth_testDataModelPredictions <- predict(fourth_trainDataModel, subset(bank_data_fourth_test, select = -c(y)))

library(caret)
confusionMatrix(fourth_trainDataModelPredictions, bank_data_fourth_train$y, positive="yes" )
confusionMatrix(fourth_testDataModelPredictions, bank_data_fourth_test$y, positive = "yes")
