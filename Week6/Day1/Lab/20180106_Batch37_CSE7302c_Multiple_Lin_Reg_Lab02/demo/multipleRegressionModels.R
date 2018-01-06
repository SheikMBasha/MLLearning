rm(list=ls(all=TRUE))

setwd("/home/jagan/Jagan/INSOFE/Regression/Batch37")
### What will we learn in the regression lab sessons
### Recently solved problems
# Life time value of a customer for a gaming company
# predicting the shipment cost  - Freight management 
# offering the incentive program for a food restraunt

# Understanding the multiple R^2 and adj R^2
# importance of standardization
# What happens if the number of columns are the same as 
# the number of rows in the model ? 
# what happens if we are adding the random variable into the model
# study the impact on multiple R^2 and adj R^2
#regression with categorical variables and it's interpretation
#Deciding the order of the categorical variables
# importance of standardizing the data
# Compare the coefficients before and after standardization
# model with only intercept
# model without intercept
# Apply transformations and Evaluating the regression model
# interpreting the categorical variable coefficients
# capturing the output to an external file
# Diagnostic plots - understanding the influence observations
### Computing the relative weights 
# study the impact of the influence observations on the coefficients
# what if least square model fails - Robust regression
# comparing the both linear and robust models
# weighted least squares
# forward and backward regressions
### Variable selection - using AIC information
#### Variation inflation factors - multicollinearity
### To figureout variables with perfect correlation
### Ridge and Lasso regression

### 
library(rpart)
?car.test.frame
getwd()
write.csv(car.test.frame,"cars.csv",row.names = F)
summary(car.test.frame)
dim(car.test.frame)
names(car.test.frame)
cor(car.test.frame$Weight,car.test.frame$Price)

### Does a variable with higher coefficient means 
# more important ? 
# How to interpret  the standard error 
### understand the 1 one in the intercept
### Look at the coefficeints for Weight and milege
### Larger the coefficient/estimate doesn't mean 
# the variable is significant
### In this example, from model2, 
# the Estimate or coefficient for Mileage has higher estimate 
##value compared to that of Weight. This doesn't mean 
#that Mileage is more important than Weight variable
### Look at the standard error , the variable with less error,
## is better for the model

model2 = lm(Price~Weight+Mileage,
            data=car.test.frame)
summary(model2)
attach(car.test.frame)
plot(Weight,Mileage)
cor(Price,Mileage)
cor(Price,Weight)^2
summary(model1)
summary(model2)$r.squared
summary(model1)$r.squared

### standardzing the data
# no need to standardize the target variable
library(rpart)
str(car.test.frame)
numeric.data <- car.test.frame[,which(sapply(car.test.frame,is.numeric))]
# Excluding the target variable
numeric.data <- numeric.data[,colnames(numeric.data)!="Price"]
numeric.data <- scale(numeric.data)

names(numeric.data)
cat.data <- data.frame((car.test.frame[,which(sapply(car.test.frame,is.factor))]))
final.data <- cbind(cat.data,numeric.data,"Price"=car.test.frame$Price)
rm(numeric.data,cat.data)
str(final.data)

model3 <- lm(Price~Weight+Mileage,data=final.data)
summary(model3)
summary(model2)
summary(model2)$r.squared
# Do you notice that standardizing the data 
# doesn't change the r squared and adj r squared 
# values and the model peerformance? 

#####
# let us unnderstand how R^2 is computed
cor(Weight,Price)^2
model1 <- lm(Price~Weight)
summary(model1)$r.squared

# How to get the adj squared value for the model output ? 
# Look at the str of summary model to see all the parameters 
# that R has compupted
# very useful feature to extract the values 
# automatically. 

str(summary(model2))
summary(model2)$adj.r.squared
summary(model2)$residuals
# understanding the adj multiple r squared value
### Significance helps to generalize the model on any 
# other sample data
# Adj R2 also helps to choose a model with 
# less number of values given tha R2 is same for the 2 models
MultipleR_squared <- summary(model2)$r.squared 
adjRsquared <- 1-((1-MultipleR_squared)*(nrow(cars)-1))/(nrow(cars)-ncol(cars))

# what happens if the number of columns are the same 
# as the number of rows a data ? 
# here is the example
# Note this model is unreliable

mtcars
dim(mtcars)
d1 <- mtcars[1:2,1:2] 
dim(d1)
model1 <- lm(d1$mpg~.,data=d1)
summary(model1)


# if we have such scenario, how do we handle ?
# Solution is to get more data from the client/DB or simulate the data.

# How does the adding a random varaible impacts
# the multiple r squared?
set.seed(12489)
random.waiting = rnorm(nrow(final.data),0,1)
#random.waiting = rep(1,50)
new = data.frame(cbind(final.data,random.waiting))
dim(new)
names(new)
model1 = lm(Price~.,data=new)
model2 = lm(Price~.-random.waiting,data=new)
c(summary(model2)$r.squared,summary(model2)$adj.r.squared)
c(summary(model1)$r.squared,summary(model1)$adj.r.squared)
summary(model2)$r.squared
model2$model
names(car.test.frame)
model1 = lm(Price~.,data=car.test.frame)
c(summary(model1)$r.squared,summary(model1)$adj.r.squared)
summary(model1)
# also look at the summary 

# data with missing values
# In r, regression model are built 
# with the data without missing values
# If there are missing values in the data
# the model will be built with the data
# without the missing values 
summary(car.test.frame)
# Note that there are missing values 
# in the variable "Reliability"
nomissing <- na.omit(car.test.frame)
dim(nomissing)
dim(car.test.frame)

model2 <- lm(Price~.,data=nomissing)
# look at the Df for F statistics
summary(model2)
model1 <- lm(Price~.,data=car.test.frame)
summary(model1)

# rest of the parameters are not changed
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared

### Regression with categorical variables
table(car.test.frame$Type)
names(car.test.frame)
length(table(car.test.frame$Type))
model6 = lm(Price~Type,data=car.test.frame)
summary(model6)
mean(car.test.frame[car.test.frame$Type=="Compact",]$Price)
### What happened to the level "Compact" in Type ? 
summary(car.test.frame$Type)
plot(car.test.frame$Type,car.test.frame$Price)
# plot
### What is the price for "Compact" Car ? 
### Deciding the base level in model building, 
## the default order is : Alpahabetial order
detach(car.test.frame)
cars.data <- within(car.test.frame, 
                    Type <- relevel(Type, ref = 5))
model7 = lm(cars.data$Price~cars.data$Type,data=cars.data)
summary(model7)$adj.r.squared
summary(model6)$adj.r.squared
### Do i reject the model ? 
par(mfrow=c(1,1))
plot(car.test.frame$Price~car.test.frame$Country)
model8 = lm(Price~Country,data=car.test.frame)
table(car.test.frame$Country)
summary(model8)
# The above model is insignificant
# which means that the variable : Country 
# is not driver to predict the price

# model diagnositics
# residuals
carsData <- na.omit(car.test.frame)
dim(carsData)
model9 <- lm(Price~., data=carsData)
summary(model9)
residuals(model9)
plot(model9)
summary(cooks.distance(model9))
length(cooks.distance(model9))
plot(x=1:nrow(carsData),y=cooks.distance(model9),
     xlab="Row Number",ylab="Cook's Distance")
noout <- carsData[cooks.distance(model9)<=0.1,]


### from extending the linear models with R 
install.packages("faraway")
library(faraway)
data(gavote)
dim(gavote)
?gavote
names(gavote)
summary(gavote)
head(gavote)
attach(gavote)
undercount = ballots-votes
gavote$undercount <- (gavote$ballots-gavote$votes)/gavote$ballots
summary(gavote$undercount)
gavote$pergore = gavote$gore/gavote$votes
plot(undercount~equip,gavote,xlab="")

### which factors accout for undercount
cor(gavote[,c(3,10,11,12)])
lmod = lm(undercount~pergore+perAA,gavote)
summary(lmod)
coef(lmod)
predict(lmod)
# residuals
residuals(lmod)

#### Evaluating the model 
undercount_hat <- fitted(lmod) # predicted values
as.data.frame(undercount_hat)
install.packages("DMwR")
library(DMwR)
regr.eval(gavote[,"undercount"], undercount_hat, 
          train.y = gavote[,"undercount"])
# regr.eval(test[,"undercount"], undercount.test_hat, train.y = gavote[,"undercount"])

## Not a great model and what constitutes a good values of 
### R^2 varies according to the application
### R^2 it can never decrease when you new precictor values
### This means , it favors add more variables
### Another way to think R^2 is 
cor(predict(lmod),gavote$undercount)^2
### Adj R^2 - Adding a predictor will only increase
### the value
?gavote
gavote$cpergore = gavote$pergore-mean(gavote$pergore)
gavote$cperAA = gavote$perAA-mean(gavote$perAA)

lmodi = lm(undercount~cperAA+cpergore+cpergore*rural+equip,gavote)
summary(lmodi)
table(gavote$equip)

#cat(out,file="out.csv",sep="\n",append=TRUE)

### Diagnostic plots
plot(lmodi)
influencePlot(lmodi,id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

### influencing and leverage points
# Look at the image "Leverage-Influenctial.png"
# Points with extrems values of X are said to be high leverage points
# An observation which, when not included, 
# greatly alters the predicted scores of other observations.
# Cook's D is a measure of the influence 
# and is proportional to the sum of the 
# squared differences between predictions made with all 
# observations in the analysis and predictions made leaving out the observation in question.
# If Cook's D > 1, the observation can be considered as 
# having too much influence.
summary(cooks.distance(lmodi))
plot(x=1:nrow(gavote),y=cooks.distance(lmodi),
     xlab="Row Number",ylab="Cook's Distance")
gavote[cooks.distance(lmodi)>0.1,]
plot(undercount~equip,gavote,xlab=" ")
# halfnorm(influence(lmodi)$hat)
?gavote
summary(lmodi)
### two points are much higher leverage than the rest
### let us understand them a bit
gavote[influence(lmodi)$hat>0.3,]
### these are the only two counties use a paper ballot
### so, they will be the only cases that determine the coefficients 
### the paper note that the counties are not identifed 
### as highly inflential
### Having high leverage alone not enough to be influential
# termplot(lmodi,partial=TRUE,terms=1)
### Gives the snapshot of the marginal relatioship between
### this predictor and the response

####### Robust regression ######
### Least square works well when there are normal errors
### but performs poorly for long-tailed erros.
### Outilier also there
write.csv(gavote,"D:/INSOFE/Regression/gavote.csv",row.names=TRUE)

library(MASS)
rlmodi = rlm(undercount~cperAA+cpergore*rural+equip,gavote)
summary(rlmodi)
summary(lmodi)
# Robust reg : Residual se: 0.01722 on 150 df
# liner reg : Residual se: 0.02335 on 150 df

### look at the coefficients for equipOS-Paper
### robust fit reduced the effect of the two outlying
### counties

# ### Weighted least squares
# ### the proportion of undercounted votes to Gore 
# ### variable in smaller counties than the larger ones
# ?gavote
# xx <-data.frame(table(gavote$ballots,gavote$rural))
# xx$Var1 <- as.numeric(as.character(xx$Var1))
# str(xx)
# 
# wlmodi = lm(undercount~cperAA+cpergore*rural+equip,gavote,weights=ballots)
# summary(wlmodi)
# summary(lmodi)
# names(gavote)
# summary(gavote$vote)

### Applying transformations on the regressors
plmodi = lm(undercount~poly(cperAA,4)+cpergore*rural+equip,gavote)
summary(plmodi)

### Applying multiple transformations 
### to fit the better model
library(rpart)
names(car.test.frame)
plot(Price~Weight,
     data=car.test.frame)

model3 = lm(Price~Weight,data=car.test.frame)
summary(model3)
plot(model3)
par(mfrow=c(1,1))

car.test.frame$Weightsquare <- (car.test.frame$Weight)^2
car.test.frame$logwt <- log(car.test.frame$Weight)
names(car.test.frame)

model3 = lm(Price~Weight,data=car.test.frame)
c(summary(model3)$r.squared,summary(model3)$adj.r.squared)

model4 = lm(Price~logwt,data=car.test.frame)
c(summary(model4)$r.squared,summary(model4)$adj.r.squared)

model5 = lm(Price~Weightsquare,data=car.test.frame)
c(summary(model5)$r.squared,summary(model5)$adj.r.squared)


model6 = lm(log(Price)~Weight,data=car.test.frame)
c(summary(model6)$r.squared,summary(model6)$adj.r.squared)


model7 = lm(log(Price)~log(Weight),data=car.test.frame)
c(summary(model7)$r.squared,summary(model7)$adj.r.squared)


car.test.frame$Pricesquare <- (car.test.frame$Price)^2
car.test.frame$logwt <- log(car.test.frame$Weight)

model7 = lm(log(Price)~log(Weight))
c(summary(model7)$r.squared,summary(model7)$adj.r.squared)

model8 = lm(Pricesquare~logwt,data=car.test.frame)
c(summary(model8)$r.squared,summary(model8)$adj.r.squared)

### Tukey's approach for transformations

#### Variation inflation factors - multicollinearity
library(car)
fit <- lm(mpg~disp+hp+wt+drat, data=mtcars)
names(mtcars)

summary(fit)
1/(1-(0.8782))
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?
fit2 <- lm(mpg~hp+drat, data=mtcars)
summary(fit2)

fit2 <- lm(mpg~hp+drat-1, data=mtcars)
summary(fit2)
sqrt(vif(fit2))

fit2 <- lm(mpg~hp:drat, data=mtcars)
summary(fit2)

set.seed(1234)
x1 <- rnorm(100)
x2 <- 3 * x1
x3 <- 5*x2
y <- rnorm(100)
alia <- alias(lm(y ~ x1 + x2+x3 ))
alia
lm(y~x1+x2+x3)
# Look at the  vif values for x2 and x3, 
# they are the sqaures of cofficients of relation between
# x2 and x3 
vif(lm(y~x1+x2+x3))

# if you add some noise, it may not identify the collinearlity 
set.seed(1234)
x1 <- rnorm(100)
x2 <- 3 * x1 + rnorm(100,5,2)
y <- rnorm(100)
vif( lm( y ~ x1 + x2 ) )
alia <- alias(lm(y ~ x1 + x2+x3 ))
alia
lm(y ~ x1 + x2+x3 )

### http://ww2.coastal.edu/kingw/statistics/R-tutorials/formulae.html
# :  -  x : z	include the interaction between these variables
# *	 - x * z	include these variables and the interactions between them
library(car)
data(cars)
cars
names(cars)
nrow(cars)
plot(cars$speed,cars$dist)
cor(cars$speed,cars$dist)
model = lm(dist~speed,data=cars)
summary(model)
str(summary(model))
### Search procedures
### Variable selection - using AIC information
### Stepwise regression
biglm = lm(undercount~(equip+econ+rural+atlanta)^2+(equip+econ+rural+atlanta)*(perAA+pergore),gavote)
?step(biglm,trace=T)
### the final model is 
finallm = lm(formula = undercount ~ equip + econ + rural + perAA + equip:econ + 
     equip:perAA + rural:perAA, data = gavote)
summary(finallm)



### Model evaluation and predictions
model.final = lm(Price~., data=car.test.frame)
test <- car.test.frame[1:10,]
summary(test)
stepAIC(model.final)
# imporant to mention the right parameters 
# in the predict function
# Also ensure the data has no missing value, 
# else you will not
## see the results
result        <- predict(model.final,test)
library(DMwR)
regr.eval(trues=test[,"Price"], 
          preds=result,
          train.y = car.test.frame[,"Price"])

regr.eval(trues=test[,"Price"], 
          preds=result)

test1 <- na.omit(test)
dim(test1)
result        <- predict(model.final,newdata=test1)


regr.eval(trues=test1[,"Price"], 
          preds=result)
options(scipen=9,digits=2)


regr.eval(trues=test1[,"Price"], 
          preds=result,
          train.y = car.test.frame[,"Price"])
options(scipen=9,digits=2)


### Relative weights


setwd("~/R")

setwd("D:/INSOFE/Consusetwd("~/R")
ltancy/Wendys/Case-study-for-students/Rcode")
load("pbo_buketed_data.RData")
str(pbo_bin1)
# Model -1 
lm_1 = lm(pbo~.,data=pbo_bin1)
summary(lm_1)
#install.packages("yhat")
library(yhat)
weights=rlw(pbo_bin1,"pbo",names(pbo_bin1)[!names(pbo_bin1) %in% c("pbo")])
y<- cbind("Attribute"=names(pbo_bin1)[-19],weights)
y <- data.frame(y)
names(y)[2]<-"weights"
# cheking if it matches to the R squared value or not 
sum(as.numeric(as.character(y$weights)))
write.csv(y,"pbo_1_rlw_wendys.csv",row.names=F)
summary(lm_1)

#### exlcluding "mgmt_promote"
lm_2 = lm(pbo~.-mgmt_promote,data=pbo_bin1)
summary(lm_2)
library(yhat)
weights=rlw(pbo_bin1,"pbo",names(pbo_bin1)[!names(pbo_bin1) %in% c("pbo","mgmt_promote")])
y<- cbind("Attribute"=names(pbo_bin1)[-19],weights)
y <- data.frame(y)
names(y)[2]<-"weights"
# cheking if it matches to the R squared value or not 
sum(as.numeric(as.character(y$weights)))
write.csv(y,"pbo_1_rlw_wendys.csv",row.names=F)

### writing the output to a CSV or TXT file ###
out<- capture.output(summary(model8))
cat(out,file="D:/INSOFE/Regression/model8out.csv",sep="\n",append=TRUE)

# model without intercept
model.wointercpet = lm(dist~speed+random.waiting-1,data=new)
summary(model.wointercpet)
# Suppose you built a model with 200 variables and 
# now you want to update the model 
# excluding some variables 
model.wospeed = lm(dist~.-speed,data=new)
summary(model.wospeed)
