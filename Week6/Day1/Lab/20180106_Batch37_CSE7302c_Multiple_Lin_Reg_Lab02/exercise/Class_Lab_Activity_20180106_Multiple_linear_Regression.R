rm(list=ls(all=TRUE))

library(e1071)

library(caret) # for box-cox transformation

library(lmtest) # bptest for testing heteroscedasticity

library(gvlma) # global validation of linear model assumptions

######################### Reading and seeing the structure of the data ###############

#DIY Set directory and read the data 
setwd("E://Insofe//Week6//Day1//Lab//20180106_Batch37_CSE7302c_Multiple_Lin_Reg_Lab02//exercise//")
data<-read.csv("CustomerData.csv",header=T)

#DIY Data Exploration - Check Data Structure, and Summary
str(data)

#DIY remove CustomerID Column
data=subset(data,select = -c(CustomerID))

#DIY convert City attribute as factor
data$City=as.factor(data$City)

############################## Splitting into train test data #############################

#DIY split the data into train and test data sets (70/30 Split)
#DIY save train data to a dataframe named "train" and test data to a dataframe named "test"
rows = seq(1, nrow(data),1)
set.seed(123)
trainrows = sample(rows,(70*nrow(data))/100)
data_train = data[trainrows,] 
data_test = data[-trainrows,]

##########################################################################################
############################ Regression with all the variables ###########################
##########################################################################################

# BUILD LINEAR REGRESSION MODEL 
LinReg1 =lm(formula =TotalRevenueGenerated~., data = data_train)

# Build model with all attributes into model. 
# "TotalRevenueGenerated" is the target variable 
summary(LinReg1)

############################## Residuals plot for checking the assumptions ################

#Review the residual plots
par(mfrow=c(2,2))
plot(LinReg1)

resid=residuals(LinReg1)
table(resid)

gvlma(x=LinReg1)

library(olsrr)
ols_bp_test(LinReg1)
lmtest::bptest(LinReg1,studentize = F)

plot(LinReg1,which=4)
par(mfrow=c(1,1)) #reset default

# look at the residual and comment on them
# Do they  follow normal distribution? 

#yes they are normally distributed inspite of some values

# Look at the other residual plots and check 
# whether the linear regression assumptions are 
# satisfied or not ?
#Yes they are satisied with one value as a outlier and heteroscadacity is been prevaled
#by checking it with the varioius tests.


################################# Removing the outliers and checking the model ################################
data2=data[-c(974,1764,2729),]

data2$TotalRevenueGenerated=log10(data2$TotalRevenueGenerated)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data2$TotalRevenueGenerated, SplitRatio = 0.8)
training_set = subset(data2, split == TRUE)
test_set = subset(data2, split == FALSE)


# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = TotalRevenueGenerated ~ .,
               data = training_set)


#Review the residual plots
par(mfrow=c(2,2))
plot(regressor)

resid=residuals(LinReg1)
table(resid)

gvlma(x=LinReg1)

library(olsrr)
ols_bp_test(LinReg1)
lmtest::bptest(LinReg1,studentize = F)

plot(regressor,which=4)
par(mfrow=c(1,1)) #reset default

#insite of removing the outliers the outliers and scaling the y axis still the heteroscadacity prervals.
############################# Cheking is done #################################

hist(LinReg1$residuals)
options(scipen = 4)
hist(regressor$residuals)

#predict(LinReg1)
#resid(LinReg1) #OBS-PRED 

##################### Checking the residuals values of mse rmse #################
# Error metrics evaluation on train data and test data
library(DMwR)

#Error verification on train data
train_error=regr.eval(data_train$TotalRevenueGenerated, LinReg1$fitted.values) 
train_error_df=data.frame(train_error)

#Error verification on test data
test=subset(data_test,select = -c(TotalRevenueGenerated))
target=data_test$TotalRevenueGenerated
pred=predict(object = LinReg1,test)

Pred<- 
  regr.eval(target, pred)

test_error_df=data.frame(Pred)
colnames(test_error_df)="test_error"

erroe_df=data.frame(train_error_df,test_error_df)






#######################################################################################
############################ Model with removing insignificat variables################
#######################################################################################

#DIY Build model (LinReg2) with significant attributes and 
# check the model summaries
summary(LinReg1)

data_2=subset(data,select=-c(MaxAgeOfChild,Tenure,NoOfGamesPlayed))
str(data_2)
#DIY Error metrics evaluation on train data and test data 
# for LinReg2 Model
rows = seq(1, nrow(data_2),1)
set.seed(123)
trainrows = sample(rows,(70*nrow(data_2))/100)
data_train_2 = data_2[trainrows,] 
data_test_2 = data_2[-trainrows,]

#MOdel Creation 
LinReg2=lm(formula =TotalRevenueGenerated~., data = data_train_2)

#Review the residual plots
par(mfrow=c(2,2))
plot(LinReg2)

resid=residuals(LinReg2)
table(resid)

gvlma(x=LinReg2)

library(olsrr)
ols_bp_test(LinReg2)
lmtest::bptest(LinReg2,studentize = F)

par(mfrow=c(1,1)) #reset default

##################### Checking the residuals values of mse rmse #################
# Error metrics evaluation on train data and test data
library(DMwR)

#Error verification on train data
train_error_2=regr.eval(data_train_2$TotalRevenueGenerated, LinReg2$fitted.values) 
train_error_df_2=data.frame(train_error_2)

#Error verification on test data
test_2=subset(data_test_2,select = -c(TotalRevenueGenerated))
target_2=data_test_2$TotalRevenueGenerated
pred_2=predict(object = LinReg2,test_2)
Pred_2<- 
  regr.eval(target_2, pred_2)
test_error_df_2=data.frame(Pred_2)
colnames(test_error_df_2)="test_error_2"

erroe_df_2=data.frame(train_error_df_2,test_error_df_2)

sumoferrors=data.frame(erroe_df,erroe_df_2)






########################################################################################
######################## Scaling the data and making the regression ####################
#########################################################################################
#Scaling the data
library(vegan)
str(data)

################## Extracting the numeric and facor data seperately #########################
data_cat= data[,sapply(data,is.factor)]
data_num= data[,sapply(data, is.numeric)]
#########################################################################################

# Apply standardization. Ensure , you exclude the target variable 
#during standardization
data_num_std <- decostand(subset(data_num,select=-c(TotalRevenueGenerated)),method ='standardize')

#Combine standardized attributes back with the 
# categorical attributes
data_std_final <- data.frame(data_cat,data_num_std,data_num$TotalRevenueGenerated)
colnames(data_std_final)[ncol(data_std_final)]="TotalRevenueGenerated"

#Split the data into train(70%) and test data sets
rows= seq(1, nrow(data_std_final),1)
set.seed(123)
trainRows= sample(rows,(70*nrow(data_std_final))/100)
train_std = data_std_final[trainRows,] 
test_std = data_std_final[-trainRows,]

# Build linear regression with all attributes
LinReg_std1 <- lm(TotalRevenueGenerated~., data=train_std)
summary(LinReg_std1)


#Review the residual plots
par(mfrow=c(2,2))
plot(LinReg_std1)

resid=residuals(LinReg_std1)
table(resid)

gvlma(x=LinReg_std1) 

library(olsrr)
ols_bp_test(LinReg_std1)
lmtest::bptest(LinReg_std1,studentize = F)

plot(LinReg_std1,which=4)
par(mfrow=c(1,1)) #reset default


#Error verification on train data
train_std_error=data.frame(regr.eval(train_std$TotalRevenueGenerated, LinReg_std1$fitted.values))
colnames(train_std_error)="train_std_error"

#Error verification on test data
target=test_std$TotalRevenueGenerated
test=subset(test_std,select=-c(TotalRevenueGenerated))
pred=predict(LinReg_std1,test)
test_std_error=data.frame(regr.eval(target,pred))
colnames(test_std_error)="test_std_error"

std_err_df=data.frame(train_std_error,test_std_error)

error=data.frame(sumoferrors,std_err_df)





###############################################################################################
########################## Checking for multicolinartity##################################
#########################################################################################
# Check for multicollinearity 
# 1. VIF: (check attributes with high VIF value)
library(car)
vif(LinReg_std1)
str(train_std)

# remove the highly correlated attributes whose value is greter than 10 and
data_3=subset(data_std_final,select=-c(FrquncyOfPurchase,NoOfGamesBought))

#Split the data into train and test
rows= seq(1, nrow(data_3),1)
set.seed(123)
trainRows= sample(rows,(70*nrow(data_3))/100)
train_std_mc = data_3[trainRows,] 
test_std_mc = data_3[-trainRows,]

# build the model 
LinReg_std2<- lm(formula = TotalRevenueGenerated~.,data = train_std_mc)
summary(LinReg_std2)
vif(LinReg_std2)

#Error verification on train data
train_std_mc_error=data.frame(regr.eval(train_std_mc$TotalRevenueGenerated, LinReg_std2$fitted.values))
colnames(train_std_mc_error)="train_std_error"

#Error verification on test data
target=test_std_mc$TotalRevenueGenerated
test=subset(test_std_mc,select=-c(TotalRevenueGenerated))
pred=predict(LinReg_std2,test)
test_std_mc_error=data.frame(regr.eval(target,pred))
colnames(test_std_mc_error)="test_std_error"

std_err_mc_df=data.frame(train_std_mc_error,test_std_mc_error)

error=data.frame(error,std_err_mc_df)


########################################################################################


# 2. Stepwise Regression
library(MASS)

Step1 <- stepAIC(LinReg_std1, direction="backward")

#Step2 <- stepAIC(LinReg1, direction="forward")
Step3 <- stepAIC(LinReg_std1, direction="both")
summary(Step3)

# select the final list of variables
data_4=subset(data_3,select=-c(MaxAgeOfChild,NoOfGamesPlayed))

#Split the data into train and test
rows= seq(1, nrow(data_4),1)
set.seed(123)
trainRows= sample(rows,(70*nrow(data_4))/100)
train_std_aic = data_4[trainRows,] 
test_std_aic = data_4[-trainRows,]

# and build the model
Mass_LinReg1 <- lm(TotalRevenueGenerated~.,data = train_std_aic)
summary(Mass_LinReg1)
par(mfrow=c(2,2))
plot(Mass_LinReg1)
plot(Mass_LinReg1,which=4)
par(mfrow=c(1,1))


#Error verification on train data
train_std_aic_error=data.frame(regr.eval(train_std_aic$TotalRevenueGenerated, Mass_LinReg1$fitted.values))
colnames(train_std_aic_error)="train_std_aic_error"

#Error verification on test data
target=test_std_aic$TotalRevenueGenerated
test=subset(test_std_aic,select=-c(TotalRevenueGenerated))
pred=predict(Mass_LinReg1,test)
test_std_aic_error=data.frame(regr.eval(target,pred))
colnames(test_std_aic_error)="test_std_aic_error"

std_err_aic_df=data.frame(train_std_aic_error,test_std_aic_error)

error=data.frame(error,std_err_aic_df)


# Identify the outliers using the cook's distance
# remove them
# build model without the influencial points (record #2729) 
which(rownames(train_std_aic)%in%c(2729))
train_std_aic_inf=train_std_aic[-c(2729),]
LinReg_No_infl<-lm(TotalRevenueGenerated~.,data = train_std_aic_inf)
summary(LinReg_No_infl)

plot(LinReg_No_infl,which=4)
#Error verification on train data
inf_error=data.frame(regr.eval(train_std_aic$TotalRevenueGenerated, Mass_LinReg1$fitted.values)) 
colnames(inf_error)="Inf_Error_train"
#Error verification on test data
MASS_Pred1<-predict(Mass_LinReg1,test_std)
inf_tes_error=data.frame(regr.eval(test_std_aic$TotalRevenueGenerated, MASS_Pred1))
colnames(inf_tes_error)="Inf_Error_test"

error=data.frame(error,inf_error,inf_tes_error)

Error_calc = data.frame(train_std_aic_inf$TotalRevenueGenerated,Mass_LinReg1$fitted.values)
write.csv(x = Error_calc,file = "Error_calc.csv")


