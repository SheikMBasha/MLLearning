rm(list=ls(all=TRUE))
setwd('E://Insofe//Week7//Day1//Lab//20180120//')

getwd()
data<-read.csv("CustomerData.csv",header=T)
str(data)
summary(data)
# Data Preparation
data = data[,-1]# remove CustomerID column
numeric_Variables = data[,c(2:10)]
str(nu)
target_variable = subset(data,select="TotalRevenueGenerated")
# convert City attribute as factor
data$City = as.factor(as.character(data$City))
#Converting categorical attributes into dummy variables
xfactors <- model.matrix(data$TotalRevenueGenerated ~ data$City + data$FavoriteChannelOfTransaction + data$FavoriteGame)[,-1]

#Split the data into train and test data sets
rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(data))/100)
train1 = data.frame(numeric_Variables, xfactors,TotalRevenueGenerated=data$TotalRevenueGenerated)[trainRows,]
test1 = data.frame(numeric_Variables, xfactors,TotalRevenueGenerated=data$TotalRevenueGenerated)[-trainRows,]

LinReg<- lm(TotalRevenueGenerated ~ ., data=train1)  
summary(LinReg)
#Evaluation of the model: Error metrics
library(DMwR)
regr.eval(train1$TotalRevenueGenerated, predict(LinReg,train1))
regr.eval(test1$TotalRevenueGenerated, predict(LinReg,test1))

#############################################################################
#Converted the data into matrix form to input into glm model
data2 <- as.matrix(data.frame(numeric_Variables, xfactors))
train = data2[trainRows,] 
test = data2[-trainRows,]

#Target Varaible
y=data$TotalRevenueGenerated[trainRows]
ytest = data$TotalRevenueGenerated[-trainRows]

# Lasso Regression  using glmnet - L1 norm
install.packages("glmnet")
library(glmnet)
# fit model
fit1 <- glmnet(train,y, alpha=1)

plot(fit1,xvar="lambda",label=TRUE)
plot(fit1,xvar="dev",label=TRUE)

#Model Selection
coef(fit1)
cv.lasso=cv.glmnet(train,y)
plot(cv.lasso)
coef(cv.lasso)

#############################################################################
# Ridge Regression  using glmnet  - L2 norm
library(glmnet)
# fit model
fit2 <- glmnet(train,y,alpha=0)
plot(fit2,xvar="lambda",label=TRUE)

#Model Selection
coef(fit2) 
cv.ridge=cv.glmnet(train,y,alpha=0)
plot(cv.ridge)
coef(cv.ridge)

regr.eval(y, predict(fit2,train))
regr.eval(ytest, predict(fit2,test))

regr.eval(y, predict(fit1,train))
regr.eval(ytest, predict(fit1,test))

