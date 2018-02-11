
rm(list = ls(all = TRUE))
getwd()

setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Week8\\Day2DecisionTrees")
#Load all the Libraries Required
#Decision tress make axis parallel cuts.
# rpart is recursion partitioning algorithm, can be used for regression as well
#Given data is clusterd, we can create parallel axis (obliques) and create mulitple regression model.

library(caret)
library(DMwR)


# Read the Data Set into R 
customerDataRaw <-  read.csv('CustomerData.csv', header = T, sep = ",")


##Check the structure of the dataset
# * Observing the structure will reveal what are the data types of attributes
# * It can be helpful to understand any data type changes are required



##Check the summary of the dataset

summary(customerDataRaw)
str(customerDataRaw)
## Remove columns which does not add any information
customerDataRaw <-  subset(customerDataRaw, select = -c(CustomerID))

#Surrogate splits, ingestion trees can be used to check how DT handles NA
##Check for Missing Values
sum(is.na(customerDataRaw))


##Convert necessary columns into  factors
customerDataRaw$City <- as.factor(customerDataRaw$City)

##Split the dataset into test and train 

set.seed(9983)
? createDataPartition
datapart <- createDataPartition(customerDataRaw$TotalRevenueGenerated, times = 1, p = 0.7, list = F)
train = customerDataRaw[datapart,]
test = customerDataRaw[-datapart,]

#C50 is classification model (DT) and rpart does both regression and classification

#Build a regression model using rpart 
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
? rpart
#Decision trees can overgrow, pruning can help us
# pre-pruning, pre decide the length of the tree, length and nbin can be parameters
# length is the lenght of tree and nbin is the number of elements in a node after which we dont split
#post-pruning, create the tree and cut/prune it later
#Anova is used as its regression
DT_rpart_Reg <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova") # by default cp is 0.01
print(DT_rpart_Reg)
printcp(DT_rpart_Reg)



plot(printcp(DT_rpart_Reg), type = 'b') # its bit less than 0.1
rsq.rpart(DT_rpart_Reg)
predCartTrain = predict(DT_rpart_Reg, newdata = train, type = "vector")
predCartTest = predict(DT_rpart_Reg, newdata = test, type = "vector")

#rm(ErrorDf)

ErrorDf <- data.frame(Model1_Train = regr.eval(train$TotalRevenueGenerated, predCartTrain))
ErrorDf <- data.frame(ErrorDf, Model1_Test = regr.eval(test$TotalRevenueGenerated, predCartTest))
View(ErrorDf)
#Model with CP
#It will stop at the best CP
DT_rpart_Reg1 <- rpart(TotalRevenueGenerated ~ ., data = train, method = "anova", control = rpart.control(cp = 0.001))
print(DT_rpart_Reg1)
plot(printcp(DT_rpart_Reg1), type = 'b')
plotcp(DT_rpart_Reg1)
#on running plot

predCartTrain = predict(DT_rpart_Reg1, newdata = train, type = "vector")
predCartTest = predict(DT_rpart_Reg1, newdata = test, type = "vector")

ErrorDf <- data.frame(ErrorDf, TrainModel2 = regr.eval(train[, "TotalRevenueGenerated"], predCartTrain))
ErrorDf <- data.frame(ErrorDf, TestModel2 = regr.eval(test[, "TotalRevenueGenerated"], predCartTest))
View(ErrorDf)
#regr.eval(train[,"TotalRevenueGenerated"], predCartTrain)
#regr.eval(test[,"TotalRevenueGenerated"], predCartTest)

##Tweeking the data set for classification  and splitting into test and train

#Below is classification
# add REvenue column on which we do classification
customerDataRaw$Revenue <- ifelse(customerDataRaw$TotalRevenueGenerated < 150, "Regular", "Premium")
customerDataRaw$Revenue <- as.factor(customerDataRaw$Revenue)
customerDataRaw$TotalRevenueGenerated <- NULL
table(customerDataRaw$Revenue)

set.seed(125)

datapart <- createDataPartition(customerDataRaw$Revenue, times = 1, p = 0.7, list = F)
train = customerDataRaw[datapart,]
test = customerDataRaw[-datapart,]

##Build classification model using C50
library(C50)
?C5.0
#a. Build model
DT_C50 <- C5.0(Revenue ~ ., data = train)
summary(DT_C50)
plot(DT_C50)
#b. Predict "Revenue" for train and test datasets
pred_Train = predict(DT_C50, newdata = train, type = "class")
pred_Test = predict(DT_C50, newdata = test, type = "class")

#c.Error Metrics on train and test
confusionMatrix(train$Revenue, pred_Train, positive="Regular") # by default it takes premium, alphabetical order
confusionMatrix(test$Revenue, pred_Test, positive = "Regular")
#d. Check variable importance
#variable importance means what variable is playing importance in GDP,
# so that we can use it
C5imp(DT_C50, pct=TRUE)
#Ouput
#NoOfUnitsPurchase 100


# Build classification model using RPART
library(rpart)

#a. Build model
DT_rpart_class1 <- rpart(Revenue~., data = train, method = "class")
printcp(DT_rpart_class1)
plot(DT_rpart_class1)
text(DT_rpart_class1)
rpart.plot(DT_rpart_class1)
DT_rpart_class1
summary(DT_rpart_class1)

# Writing model summary
write(capture.output(summary(DT_C50)), "c50model.txt")

#b. Predict "Revenue" for train and test datasets
pred_Train1 = predict(DT_rpart_class1, newdata=train, type="class")
pred_Test1 = predict(DT_rpart_class1, newdata = test, type = "class")
  
#c.Error Metrics on train and test
confusionMatrix(train$Revenue, pred_Train1)
confusionMatrix(test$Revenue, pred_Test1)

DT_rpart_Class2 <- rpart(Revenue ~ ., data = train, method = "class", control = rpart.control(cp = 0.001))
print(DT_rpart_Class2)
plot(printcp(DT_rpart_Class2))
plotcp(DT_rpart_Class2)

#Predict "Revenue" for train and test datasets
pred_Train1 = predict(DT_rpart_Class2, newdata = train, type = "class")
pred_Test1 = predict(DT_rpart_Class2, newdata = test, type = "class")

#Error Metrics on train and test
confusionMatrix(train$Revenue,pred_Train1)
confusionMatrix(test$Revenue,pred_Test1)


#Hyper parameters can be used to control the model tuning.
#Parameter tuning - here it is length and nbin, length of tree and nbin is the number of elements in bin
# caret provides automated way to do this
# gridSearch is one of the way
#ex lengt = frm 3 to 10 and nbin from 5 to 50
# we have  m * n combinations and creates a grid, gives the model with the best accuracy