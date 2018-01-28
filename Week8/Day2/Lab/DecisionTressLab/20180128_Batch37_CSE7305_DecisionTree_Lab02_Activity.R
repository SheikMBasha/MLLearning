
rm(list=ls(all=TRUE))

#Load all the Libraries Required


library(caret)
library(DMwR)


# Read the Data Set into R 


##Check the structure of the dataset
# * Observing the structure will reveal what are the data types of attributes
# * It can be helpful to understand any data type changes are required



##Check the summary of the dataset


## Remove columns which does not add any information


##Check for Missing Values



##Convert necessary columns into  factors


##Split the dataset into test and train 

set.seed(9983)
train=
test=


#Build a regression model using rpart 
library(rpart)
library(rpart.plot)


predCartTrain=
predCartTest=
  
regr.eval(train[,"TotalRevenueGenerated"], predCartTrain)
regr.eval(test[,"TotalRevenueGenerated"], predCartTest)

##Tweeking the data set for classification  and splitting into test and train

CustomerData$Revenue <-
CustomerData$Revenue<-as.factor(CustomerData$Revenue)
CustomerData$TotalRevenueGenerated<-NULL

set.seed(125)

train=
test=

##Build classification model using C50
library(C50)

#a. Build model
DT_C50 <- 
summary(DT_C50)

#b. Predict "Revenue" for train and test datasets
pred_Train = 
pred_Test = 

#c.Error Metrics on train and test

#d. Check variable importance
C5imp(DT_C50, pct=TRUE)



# Build classification model using RPART
library(rpart)

#a. Build model
DT_rpart_class <- 
printcp(DT_rpart_class)
DT_rpart_class
summary(DT_rpart_class)

# Writing model summary
write(capture.output(summary(DT_C50)), "c50model.txt")

#b. Predict "Revenue" for train and test datasets
pred_Train = 
pred_Test =
  
#c.Error Metrics on train and test
confusionMatrix(train$Revenue,pred_Train)
confusionMatrix(test$Revenue,pred_Test)


#Predict "Revenue" for train and test datasets
pred_Train1 = 
pred_Test1 = 

#Error Metrics on train and test
confusionMatrix(train$Revenue,pred_Train1)
confusionMatrix(test$Revenue,pred_Test1)
