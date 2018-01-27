rm(list = ls(all = TRUE))
setwd("E://Insofe//RPracticeVS//RPracticeVS//Week7//TimeSeries//TimeSeries Assignment2")
StockRaw = read.csv("train.csv", header = T, sep = ",")
StockRaw$ds <- as.Date(StockRaw$ds, format = "%Y-%m-%d")
StockRaw$y <- sqrt(StockRaw$y)
plot(StockRaw$ds, StockRaw$y, type = "l")



StockTimeSeries <- ts(StockRaw$y, frequency = 12, start = c(1992, 1))

StockDecompose <- decompose(StockTimeSeries)
plot(StockDecompose)

acf(StockTimeSeries, lag = 30)
pacf(StockTimeSeries, lag = 30)

hwStock <- HoltWinters(StockTimeSeries, beta = TRUE, gamma = TRUE, seasonal = "additive")
head(hwStock$fitted)

TrainActuals <- StockTimeSeries[13:nrow(StockRaw)]

library(DMwR)
ErrorMetris <- data.frame(HW_Train = regr.eval(TrainActuals, hwStock$fitted))

library(forecast)
hwForecasts <- forecast(hwStock, h = 12)
hwtestPred <- data.frame(hwForecasts)$Point.Forecast

#Auto Arima
StockAutoArima <- auto.arima(StockTimeSeries, ic = 'aic')
StockAutoArimaForecast <- forecast(StockAutoArima, h=12)
StockAutoArimaPredictions <- data.frame(StockAutoArimaForecast)$Point.Forecast

sample = read.csv("predictions.csv")
sample$y <- StockAutoArimaPredictions^2
write.csv(sample,"predictions.csv")