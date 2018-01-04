
##----------------------------------------------------------------------------------##
##--- SIMPLE LINEAR REGRESSION MODEL - ACTIVITY-------------------------------------##
##----------------------------------------------------------------------------------##


##--- Step 1: Clear environment variables ------------------------------------------##
#$$$$$$$$~~~Type the code below:~~~
rm(list=ls(all=T))
##__________________________________________________________________________________##


##--- Step 2: Set working Directory ------------------------------------------------##
setwd("E://Insofe//Week5//Day2//LAbWork//20171231_Batch37_CSE7302c_Lab01_SimpleLinReg_Lab")
getwd()
##__________________________________________________________________________________##


##--- Step 3: Read the data from "Toyota_SimpleReg.csv" file --------------------------------------##
#$$$$$$$$~~~Complete the code below:~~~
cars_data = read.csv("Toyota_SimpleReg.csv", header = T, sep = ",")
names(cars_data)
##__________________________________________________________________________________##


##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop the Id, Model attributes:
#$$$$$$$$~~~Complete the code below:~~~
cars_data = subset(cars_data, select = -c(Id,Model))

## Summary of the data and look for any missing values:
str(cars_data)
summary(cars_data)
#No missing values
cars_data[!complete.cases(cars_data$Price),]
cars_data[!complete.cases(cars_data$Age_06_15),]
## Correlation and Covariance between the attributes:

cov(cars_data)
#The covariance of the Age of car and Price is -59136.11. 
#It indicates a negative linear relationship between the two variables. 
#This relation could be observed from the scatter plot also.
plot(cars_data$Age_06_15, cars_data$Price)
plot(cars_data$Age_06_15, cars_data$Price, xlab = "Age of the car", ylab = "Price in ($)", pch = 18, col = "blue")

cor(cars_data)
cor(cars_data$Age_06_15, cars_data$Price)
#The correlation coefficient of the Age of car and Price is -0.8765905. 
#Since the value is close to 1 and has a -ve sign, we can conclude that the variables are strongly negatively correlated.
##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (70:30) ratio
rows = seq(1, nrow(cars_data),1)
set.seed(123) # any random value to start the random value
trainRows = sample(rows,(70*nrow(cars_data))/100)
cars_train = cars_data[trainRows,] 
cars_test = cars_data[-trainRows,]
##__________________________________________________________________________________##


##--- Step 6: Linear regression model building--------------------------------------##
#$$$$$$$$~~~Complete the code below:~~~
LinReg = lm(Price ~ Age_06_15, data = cars_train)
#LinReg = lm(cars_train$Price ~ cars_train$Age_06_15)
coefficients(LinReg)

# y = mx+ c where m = -171.12 and c = 26127
# price = -171(Age) + 26127
## Summary of model:
summary(LinReg)
#R square - 76.59
# p value : variables are independent of each other (Age, price)
# p value < 0.05,
#*** 3 starts shows very significatnt
#Std. Error is standard deviation of error
#1003 is n-k-1 - degrees of freedom
#F - distribution

plot(LinReg)


#Optional for info: 
#To extract the coefficients:
coefficients(LinReg)
coefficients(LinReg)[1]
coefficients(LinReg)[2]
names(coefficients(LinReg))
#To extract the residuals:
LinReg$residuals
#To extract the train predictions:
LinReg$fitted.values
##__________________________________________________________________________________##

plot(cars_train$Age_06_15, cars_train$Price)
abline(LinReg)
##--- Step 7: Check for validity of linear regression assumptions ------------------##
par(mfrow = c(2,2))
#$$$$$$$$~~~Complete the code below:~~~

par(mfrow = c(1,1))
##__________________________________________________________________________________##


##--- Step 8: Predict on testdata --------------------------------------------------##
#$$$$$$$$~~~Complete the code below:~~~

# In test data dont keep target variables
target <- cars_test$Price
cars_test$Price <- NULL
class(cars_test)
test_prediction = predict(LinReg,cars_test)
#test_actual = 
##__________________________________________________________________________________##


##--- Step 9: Error Metrics --------------------------------------------------------##
library(DMwR)
#Error verification on train data
regr.eval(cars_train$Price, LinReg$fitted.values)

#mae = mod(y-y-cap)/n

#mse = (y-y cap)^2/n

#rmse = sqrt(mse)
# mape = (y-ycap)/n  output shown in rer.eval does not have multiplciation with 100 on mape
#mean absolute percentage error

#Error verification on test data
regr.eval(target, test_prediction)
##__________________________________________________________________________________##


##--- Step 10: Confidence and Prediction Intervals----------------------------------##
# Confidence Intervals talk about the average values intervals
# Prediction Intervals talk about the all individual values intervals
conf_Pred = data.frame(predict(LinReg, cars_test, interval="confidence",level=0.95))
#$$$$$$$$~~~Complete the code below:~~~
pred_Pred = data.frame(predict(LinReg, cars_test, interval="prediction",level=0.95))

names(conf_Pred)

plot(cars_test$Age_06_15, cars_test$Price, xlab = "Age of the car", ylab = "Price in ($)")

points(cars_test$Age_06_15,conf_Pred$fit,type="l", col="green", lwd=2)
points(cars_test$Age_06_15,conf_Pred$lwr,pch="-", col="red", lwd=4)
points(cars_test$Age_06_15,conf_Pred$upr,pch="-", col="red", lwd=4)
points(cars_test$Age_06_15,pred_Pred$lwr,pch="-", col="blue", lwd=4)
points(cars_test$Age_06_15,pred_Pred$upr,pch="-", col="blue", lwd=4)
##__________________________________________________________________________________##
#-----------------------end---------------------------------------------------------##