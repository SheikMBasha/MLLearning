
##----------------------------------------------------------------------------------##
##--- SIMPLE LINEAR REGRESSION MODEL building on BigMac dataset - ASSIGNMENT -------##
##----------------------------------------------------------------------------------##


##--- Step 1: Clear environment variables ------------------------------------------##
rm(list=ls(all=T))

##__________________________________________________________________________________##


##--- Step 2: Set working Directory ------------------------------------------------##
setwd("E://Insofe//Week5//Day2//LAbWork//20171231_Batch37_CSE7302c_Lab01_SimpleLinReg_Lab")
getwd()
##__________________________________________________________________________________##


##--- Step 3: Read the data from the csv file --------------------------------------##

BigMacAll <- read.csv("BigMac-NetHourlyWage.csv", header = T, sep =",")

##__________________________________________________________________________________##


##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop any irrelevant attribute(s):
BigMacAll <- subset(BigMacAll, select = -c(Country))

## Summary of the data and look for any missing values:

summary(BigMacAll)
BigMacAll[!complete.cases(BigMacAll$Big.Mac.Price....),]
BigMacAll[!complete.cases(BigMacAll$Net.Hourly.Wage....),]

## Correlation and Covariance between the attributes:
cov(BigMacAll)
cor(BigMacAll)


#Describe how the covarainace and correlation coefficients are for the BigMac dataset
cor(BigMacAll$Big.Mac.Price....,BigMacAll$Net.Hourly.Wage....)
# The covariance of BigMacPrice and NetHourlyWage is 5.092068
# 0.71 is a good strength as just one variable covers 71% variation of data.
# +ve value 0.71 indicates positive relationship between BigMacPrice and NetHourlyWage

#Do the attributes have a good enough correlation coefficient to support linear regression model building?


##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (80:20) ratio


rows = seq(1,nrow(BigMacAll),1)
set.seed(44)
trainBigMacAllrows <- sample(rows,(80*nrow(BigMacAll))/100)
trainBigMacAll <- BigMacAll[trainBigMacAllrows,]
testBigMacAll <- BigMacAll[-trainBigMacAllrows,]
##__________________________________________________________________________________##


##--- Step 6: Linear regression model building--------------------------------------##

LinRegBigMacAll <- lm(Net.Hourly.Wage.... ~ Big.Mac.Price...., data = trainBigMacAll)
coefficients(LinRegBigMacAll)
## Summary of model:

summary(LinRegBigMacAll)
#Extract the intercept coefficient from the linear regression model

coefficients(LinRegBigMacAll)[1]
coefficients(LinRegBigMacAll)[2]

#Extract the residual values

LinRegBigMacAll$residuals

##__________________________________________________________________________________##


##--- Step 7: Check for validity of linear regression assumptions ------------------##
#HINT: plot the 4 graphs to check. Write your comments

plot(LinRegBigMacAll)
# The model looks linear with exception of couple of data points.
# Q-Q plot shows that model is normal, data is normally distributed on Z scale.
# variance is homoschedastic except on one data point "3"
# except one data point "3", none of the data points cross the cooks-distance of 0.5

plot(trainBigMacAll$Big.Mac.Price...., trainBigMacAll$Net.Hourly.Wage....)
abline(LinRegBigMacAll)
##__________________________________________________________________________________##


##--- Step 8: Predict on testdata --------------------------------------------------##

test <- testBigMacAll$Net.Hourly.Wage....
testBigMacAll$Net.Hourly.Wage.... <- NULL
class(testBigMacAll)

test_prediction <- predict(LinRegBigMacAll, testBigMacAll)
##__________________________________________________________________________________##


##--- Step 9: Error Metrics --------------------------------------------------------##

library(DMwR)
#Error verification on train data
regr.eval(trainBigMacAll$Net.Hourly.Wage....,LinRegBigMacAll$fitted.values)


#Error verification on test data
regr.eval(test,test_prediction)


##__________________________________________________________________________________##


##--- Step 10: Confidence and Prediction Intervals----------------------------------##
#Find the confidence and prediction intervals and plot them for the WHOLE dataset

#confidence  is sure 
conf_pred <- data.frame(predict(LinRegBigMacAll, testBigMacAll, interval = "confidence", level = 0.95))
pred_pred <- data.frame(predict(LinRegBigMacAll, testBigMacAll, interval = "prediction", level = 0.95))
#prediction accounts for uncertainitiy

names(conf_pred)

plot(testBigMacAll$Big.Mac.Price....,testBigMacAll$Net.Hourly.Wage...., xlab = "BigMac Price", ylab = "Net Hourly Wage")

#points(testBigMacAll$Big.Mac.Price....,conf_pred$fit, type = "1", col="green", lwd=2)

points(testBigMacAll$Big.Mac.Price....,conf_pred$fit,type="l", col="green", lwd=2)
points(testBigMacAll$Big.Mac.Price....,conf_pred$lwr,pch="-", col="red", lwd=4)
points(testBigMacAll$Big.Mac.Price....,conf_pred$upr,pch="-", col="red", lwd=4)
points(testBigMacAll$Big.Mac.Price....,pred_pred$lwr,pch="-", col="blue", lwd=4)
points(testBigMacAll$Big.Mac.Price....,pred_pred$upr,pch="-", col="blue", lwd=4)

##__________________________________________________________________________________##
#-----------------------end---------------------------------------------------------##