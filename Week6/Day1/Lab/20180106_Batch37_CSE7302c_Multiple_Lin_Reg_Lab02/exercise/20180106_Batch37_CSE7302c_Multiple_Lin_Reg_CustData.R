rm(list=ls(all=TRUE))

#DIY Set directory and read the data 
setwd("E://Insofe//Week6//Day1//Lab//20180106_Batch37_CSE7302c_Multiple_Lin_Reg_Lab02//exercise")
data<-read.csv("CustomerData.csv",header=T)

#DIY Data Exploration - Check Data Structure, and Summary
str(data)
summary(data)
#DIY remove CustomerID Column
data <- subset(data, select=-c(CustomerID))

dataForModel = data

#DIY convert City attribute as factor
data$City <- as.factor(data$City)

#DIY split the data into train and test data sets (70/30 Split)
rows = seq(1,nrow(dataForModel),1)
set.seed(123)
trainRows = sample(rows,(70*nrow(dataForModel))/100)
#DIY save train data to a dataframe named "train" and test data to a dataframe named "test"
train <- dataForModel[trainRows,]
test <- dataForModel[-trainRows,]
# BUILD LINEAR REGRESSION MODEL 

# Build model with all attributes into model. 
# "TotalRevenueGenerated" is the target variable 
?lm
LinReg1<- lm( TotalRevenueGenerated ~ ., data=train)
summary(LinReg1)

#Review the residual plots
par(mfrow=c(2,2))
plot(LinReg1)
plot(LinReg1,which=4)
par(mfrow=c(1,1)) #reset default
# look at the residual and comment on them
# Do they  follow normal distribution? 
#Yes except few values it follows normal distribution
# Look at the other residual plots and check 
#Density of data is distributed properly, so it is normal distribution
# whether the linear regression assumptions are 
# satisfied or not ? 

#Action items: from Residual vs fitted and Scale-location
  #a. Segment data by splitting
  #b. transformation
  #c. non linear model

# Normal Q-Q graph
#a. remove the outliers

# to remove outliers, use cooks distance
hist(LinReg1$residuals)
#predict(LinReg1)
#resid(LinReg1) #OBS-PRED 

# Error metrics evaluation on train data and test data
library(DMwR)
#Error verification on train data
regr.eval(train$TotalRevenueGenerated, LinReg1$fitted.values) 
#Error verification on test data
target <- test$TotalRevenueGenerated
test$TotalRevenueGenerated <- NULL
test_prediction <- predict(LinReg1,test)
Pred<- regr.eval(target, test_prediction)
Pred


#DIY Build model (LinReg2) with significant attributes and 
# check the model summaries

summary(LinReg1)
LinReg2 <- lm(TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + FrquncyOfPurchase
              + NoOfUnitsPurchased + FrequencyOFPlay + NoOfGamesBought 
              + FavoriteChannelOfTransaction + FavoriteGame, data = train)

#In LinReg1 observe the significant values
summary(LinReg2)

par(mfrow=c(2,2))
plot(LinReg2)

#DIY Error metrics evaluation on train data and test data 
# for LinReg2 Model

regr.eval(train$TotalRevenueGenerated, LinReg2$fitted.values)
test
test_prediction2 <- predict(LinReg2,test)
pred2 <- regr.eval(target,test_prediction2)
pred2

####################################################

####################################################
#Scaling the data
library(vegan)
str(data)
data_cat <- data[,c(1,11,12)]
data_num <- data[,-c(1,11,12)]
summary(data_num)
# Apply standardization. Ensure , you exclude the target variable 
#during standardization
#rm(data_num_subset)
data_num_std <- decostand(subset(data_num, select=-c(TotalRevenueGenerated)),"standardize")

#Combine standardized attributes back with the 
# categorical attributes
data_std_final <- cbind(data_num_std,data_cat)
data_std_final <- cbind(data_std_final, data_num$TotalRevenueGenerated)
class(data_std_final)
colnames(data_std_final)[ncol(data_std_final)] <- "TotalRevenueGenerated"
#Split the data into train(70%) and test data sets
rows= seq(1, nrow(data_std_final),1)
set.seed(123)
trainRows= sample(rows,(70*nrow(data_std_final))/100)
train_std = data_std_final[trainRows,] 
test_std = data_std_final[-trainRows,]

# Build linear regression with all attributes
LinReg_std1 <- lm(TotalRevenueGenerated~., data=train_std)
summary(LinReg_std1)

#Error verification on train data
regr.eval(train_std$TotalRevenueGenerated,LinReg_std1$fitted.values)

#Error verification on test data
target_std <- test_std$TotalRevenueGenerated
test_prediction_std1 <- predict(LinReg_std1, test_std)
pred_Std1 <- regr.eval(target_std,test_prediction_std1)
pred_Std1

#Why standardization
#1. We can compare coeff.
#2. variables wont overlshadow each other.
#3. speeds up the convergence process (Gradient Descent algorithm is used in backend)

#IF NA's are there in coefficients, then that means there is a strong collinearity
#refer video last few mins on 06th jan for details

# Check for multicollinearity 

# 1. VIF: (check attributes with high VIF value)
library(car)
vif(LinReg_std1)
str(train_std)
# remove the highly correlated attributes and 
# build the model 

LinReg_std2<- 
summary(LinReg_std2)
vif(LinReg_std2)
#Error verification on test data

#AIC tells how muc data is lost.
# lower the AIC, it is best

#StepAIC helps us in feature selection.
# three types : both, forward and backward

# 2. Stepwise Regression
library(MASS)
Step1 <- stepAIC(LinReg_std1, direction="backward")
#Step2 <- stepAIC(LinReg1, direction="forward")
Step3 <- stepAIC(LinReg_std1, direction="both")
summary(Step3)
# select the final list of variables
# and build the model
Mass_LinReg1 <- 
summary(Mass_LinReg1)
par(mfrow=c(2,2))
plot(Mass_LinReg1)
plot(Mass_LinReg1,which=4)
par(mfrow=c(1,1))
head(train)
# Identify the outliers using the cook's distance
# remove them
# build model without the influencial points (record #2729) 
which(rownames(train_std)%in%c(???))
LinReg_No_infl<- 
summary(LinReg_No_infl)

#Error verification on train data
regr.eval(train_std$TotalRevenueGenerated, Mass_LinReg1$fitted.values) 
#Error verification on test data
MASS_Pred1<-predict(Mass_LinReg1,test_std)
regr.eval(test$TotalRevenueGenerated, MASS_Pred1)

Error_calc = data.frame(train_std$TotalRevenueGenerated,Mass_LinReg1$fitted.values)
write.csv(x = Error_calc,file = "Error_calc.csv")



##########Other Experiments
# Instead of removing the outliers, replace them with median
# If MinAgeofChild  and MaxAgeofChild is greater than or equal to 100, 
# then, replace with median of the respective column

# split the data into train and test data sets

# BUILD LINEAR REGRESSION MODEL 

# build model with all attributes into model 
#Error verification on train data
#Error verification on test data

#Exp2- Variable Transformations Y and/or X variables
#Y Variable Transformation - Apply log 
datatemp$TotalRevenueGenerated <- log(datatemp$TotalRevenueGenerated)
# Build the model and compute the error metrics 
#X Variable Transformation  and Build the model and validate

#Exp3- Variable Interactions

dataForModel = data

# split the data into train and test data sets
rows=seq(1,nrow(dataForModel),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(dataForModel))/100)
train = dataForModel[trainRows,] 
test = dataForModel[-trainRows,] 

# BUILD LINEAR REGRESSION MODEL 

# build model with all attributes into model 
LinReg6<- lm(TotalRevenueGenerated ~ .^2, data=train)
#LinReg6<- lm(TotalRevenueGenerated ~ (x1+x2+x3)^2,data=train)
summary(LinReg6)
par(mfrow=c(2,2))
plot(LinReg6)
plot(LinReg6,which=4)

#Error verification on train data
library(DMwR)
regr.eval(train$TotalRevenueGenerated, LinReg6$fitted.values) 
#Error verification on test data
Pred<-predict(LinReg6,test)
regr.eval(test$TotalRevenueGenerated, Pred)

