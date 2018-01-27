rm(list=ls(all=TRUE))
getwd()
setwd('E://Insofe//Week7//Day2//Lab')
load("Data.RData")
str(data)
dim(data)
head(data)
names(data)
tail(data)

library(sqldf) # to write SQL like commands in R to aggregate the data. 
RtData2.Day <- sqldf("select Date,min(Price) as MIN_PRICE from data group by Date")
str(RtData2.Day)
RtData2.Day$Date=as.Date(RtData2.Day$Date,format="%Y-%m-%d")
str(RtData2.Day)

head(RtData2.Day) # we have missing values 02nd jan doesnt have an entry

# To find the minimum of the dates 

minDate=min(as.Date(RtData2.Day$Date,format="%Y-%m-%d"))
# To find the maximum of the dates
maxDate =max(as.Date(RtData2.Day$Date,format="%Y-%m-%d"))
# generating the series of dates
seq <- data.frame("dateRange"=seq(minDate,maxDate,by="days"))

# left joining to see the missing values for the dates. all.x will do the left join."all.y" will do right join.  
RtData2.Day2= merge(seq,RtData2.Day,by.x="dateRange",by.y="Date",all.x=T)
head(RtData2.Day2)

#   Here is the example to understand how "na.locf()"  function works
library(zoo)
x <- c(NA,1,2,3,4,5,NA,NA,NA,7,8,NA)
# na.locf function is used to replace the missing values. This will replace the missing value with the it's immediate preceding value. 
na.locf(x)
# This function reverses the sequence
rev(x)
# if you want to replace the missing value with its immediate neighbors, here is the R code. This code is to show that missing value is replaced with it's preceding and succeeding values
na.locf(x)
rev(na.locf(rev(x)))
(na.locf(x) + rev(na.locf(rev(x))))/2

# Use the above code to replace the missing values 
# in the Price variable
RtData2.Day2$MIN_PRICE=(na.locf(RtData2.Day2$MIN_PRICE) + 
                          rev(na.locf(rev(RtData2.Day2$MIN_PRICE))))/2

# Let us verify this before we move on. 
head(RtData2.Day2)
str(RtData2.Day2)

RtData2.Day2$WEEK <- as.numeric(format(RtData2.Day2$dateRange, format="%Y.%W"))
head(RtData2.Day2)
# Now aggregating to weekly data 
RtData2Day2 <- RtData2.Day2
head(RtData2Day2)
library(sqldf)
RtData2.Week <- sqldf("select WEEK as WEEK,min(MIN_PRICE) as MIN_PRICE from RtData2Day2 group by WEEK")

#Dividing data as Train & Test
Train=RtData2.Week[which(RtData2.Week$WEEK<=2013.37),]
Test=RtData2.Week[which(RtData2.Week$WEEK>2013.37),]

#Constucting a time series object
Price <- ts(Train$MIN_PRICE, frequency =52)
plot(Price,type="l",lwd=3,col="blue",xlab="week",ylab="Price", main="Time series plot")
# l means line in type above

pricedecomp <- decompose(Price)
plot(pricedecomp)

# we can choose any lag
acf(Price,lag=30)
pacf(Price,lag=30) # according to pacf seasonality is not there

Price1 <- ts(Train$MIN_PRICE, frequency =1) # frequence equal to 1 means yearly data
par(mfrow=c(1,2))
acf(Price1,lag=30)
pacf(Price1,lag=30)


# we are doing differences below, at the end there is a spike (diff is throughout not season or non seasonal)
par(mfrow=c(1,2))
plot(diff(Price1,lag=1),type="l")
plot(diff(Price1,lag=2),type="l")



# The library TTR stands for Technical trading rules. 
library(TTR)
fitsma <- SMA(Price,n=2) # n=2 means 2 lags
length(fitsma)
length(Price)
fitsma # check how to plot this


#Let us see how this model performs. You could choose any of the error metrics. Here we used MAPE to compute the error.
# MAPE - percentage diff between absoukte values and take mean
smaMape <- mean(abs((Price[2:length(Price)]-fitsma[2:length(Price)])/Price[2:length(Price)]))
smaMape
# TP=Test$MIN_PRICE
mean(abs((Price[2:length(Price)]-fitsma[2:length(Price)])/Price[2:length(Price)]))


fitwma<- WMA(Price,n=2,1:2) # 1:2 is vector of weights and should be same as n
# Similar to above fitwma<- WMA(Price,n=2,c(1,2))

fitEma <- EMA(Price, n = 2)
emaMape <- mean(abs((Price[2:length(Price)]-fitEma[2:length(Price)])
                    /Price[2:length(Price)]))
emaMape # this is the metric MAPE



par(mfrow=c(2,2))
plot(Train$MIN_PRICE, type="l", col="black")
plot(fitsma, type="l", col="red")
plot(fitwma, type="l", col="blue")
plot(fitEma, type="l", col="brown")
par(mfrow=c(1,1))
plot(Train$MIN_PRICE, type="l", col="black")
lines(fitsma,col="red")
lines(fitwma, col="blue")
lines(fitEma, col="brown")

#Building the Holt winter's model taking only Trend component. 
holtpriceforecast <- HoltWinters(Train$MIN_PRICE,  beta=TRUE, gamma=FALSE)
# alpha is coefficient for level and its by default included

head(holtpriceforecast$fitted)# Look the fitted or forecasted values

priceholtforecast <-  HoltWinters(Price, beta=TRUE, gamma=TRUE,
                                  seasonal="additive")
head(priceholtforecast$fitted)
#Observe predicted values in xhat

#Considering Train data
#hw_price <- HoltWinters(Price , beta=TRUE, gamma=FALSE)
# OR 
#Considering  first 260 rows from RtData2.Week as train, and forecasting for the last week
#hw_price <- HoltWinters(RtData2.Week[1:260] , beta=TRUE, gamma=FALSE)

#hw_price_gamma <- HoltWinters(Price[1:260], beta=TRUE, gamma=TRUE, seasonal="additive")
#hw_price$fitted
train_actuals <- Train$MIN_PRICE[53:251]
#train_pred <- data.frame(hw_price$fitted)[1]
length(priceholtforecast$fitted)
length(train_actuals)
DMwR::regr.eval(train_actuals,priceholtforecast$fitted[,1])

library("forecast")
hw_price_forecasts = forecast(priceholtforecast,h=11)
  #forecast(hw_price,h=1)

test_preds <- data.frame(hw_price_forecasts)$Point.Forecast
#forecast.HoltWinters(hw_price,h=1)
test_actuals <- Test$MIN_PRICE
#OR test_actuals <- RtData2.Week$MIN_PRICE[262]
DMwR::regr.eval(test_actuals,test_preds)

# # Getting the predictions on Training data
# holtforecastTrain <- data.frame(priceholtforecast$fitted)
# holtforecastTrainpredictions <- holtforecastTrain$xhat
# head(holtforecastTrainpredictions)
# To get the predictions on Test Data, you can use function "forecast.Holt". "h" indicates the number of future weeks (or whatever be your reference time period, say months, quarters, etc.,) for which you want to get the predictions
priceforecast <-  forecast.HoltWinters(priceholtforecast, h=8)


#### ARIMA models

library("forecast")
par(mfrow=(c(1,3)))
plot(Price)
acf(Price)
pacf(Price)

ndiffs(Price) #choose d from here; OR check manually
plot(pacf(diff(Price),1, lag.max = 30))#choose p from here
plot(acf(diff(Price),1)) #choose q from here

# Model with no trend and no seasonality. 
model1 <- arima(Price,c(0,0,0))
model1
forecast.Arima(model1, h=4)

model2 <- arima(Price,c(0,1,0))
model2

model3 <- arima(Price,c(0,2,0))
model3

model4 <- arima(Price,c(1,1,1))
model4
dev.off()
par(mfrow=c(1,4))
plot(model1$residuals,ylim=c(-50,50))
plot(model2$residuals,ylim=c(-50,50))
plot(model3$residuals,ylim=c(-50,50))
plot(model4$residuals,ylim=c(-50,50))

library("forecast")
MODEL_ARIMA <- auto.arima(Price, ic='aic')
plot(model2$residuals,ylim=c(-50,50))
summary(MODEL_ARIMA)



# Lest us look at the acf and Pacf graphs to check if 
# there are patterns
acf(as.numeric(MODEL_ARIMA$residuals) ,lag.max = 20, 
    main = "Residuals ACF plot")
pacf(as.numeric(MODEL_ARIMA$residuals) ,lag.max = 20, 
     main = "Residuals PACF plot")


pricearimaforecasts <- forecast.Arima(MODEL_ARIMA, h=4)
