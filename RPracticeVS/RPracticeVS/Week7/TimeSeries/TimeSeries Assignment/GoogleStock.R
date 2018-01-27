install.packages("quantmod")
library(quantmod)
start <- as.Date("2016-01-01")
end <- as.Date("2018-01-26")
getSymbols("GOOGL", src = "yahoo", from = start, to = end)

GoogleDFRaw <- as.data.frame(GOOGL)
GoogleDFRaw$date <- rownames(GoogleDFRaw)
GoogleDFRaw$date <- as.Date(rownames(GoogleDFRaw), format = "%Y-%m-%d")
str(GoogleDFRaw)
rownames(GoogleDFRaw) <- NULL

GoogleDFFiltered <- subset(GoogleDFRaw, select = c(GOOGL.Close, date))
str(GoogleDFFiltered)

GoogleDFFiltered$Week <- as.numeric(format(GoogleDFFiltered$date, format = "%Y.%W"))
colnames(GoogleDFFiltered) <- c('close','date','week')
GoogleDFWeekly <- sqldf("select week as week, avg(close) as closingPrice from GoogleDFFiltered group by week")
plot(GoogleDFWeekly)


Train <- GoogleDFWeekly[which(GoogleDFWeekly$week <= 2017.52),]
Test <- GoogleDFWeekly[which(GoogleDFWeekly$week > 2017.52),]

Stocks <- ts(Train$closingPrice, frequency = 52)
?plot
plot(Stocks, type="l", lwd=3, col="blue", xlab="week", ylab="price", title="Google stoc")
StocksDecomposed <- decompose(Stocks)
plot(StocksDecomposed)

acf(Stocks, lag = 30)
pacf(Stocks, lag = 30)

#HoltWinters with only trend - Begin

holtStockPriceForecastWithOnlyTrend <- HoltWinters(Stocks, beta = TRUE, gamma = FALSE)
head(holtStockPriceForecastWithOnlyTrend$fitted)

library(DMwR)
nrow(Train)
TrainActuals <- Stocks[3:nrow(Train)]
ErrorMetrics <- data.frame(HoltWintersTrainWithTrend = regr.eval(TrainActuals, holtStockPriceForecastWithOnlyTrend$fitted[, 1]))

library(forecast)
holtStockPriceForecastWithOnlyTrend_Forecasts <- forecast(holtStockPriceForecastWithOnlyTrend, h = 4)
test_preds <- data.frame(holtStockPriceForecastWithOnlyTrend_Forecasts)$Point.Forecast
test_actuals <- Test$closingPrice

ErrorMetrics <- data.frame(ErrorMetrics, HoltWintersTestWithTrend = regr.eval(test_actuals, test_preds))
View(ErrorMetrics)

HWPriceForeCastWithOnlyTrend <- forecast(holtStockPriceForecastWithOnlyTrend, h = 4)
#HoltWinters with only trend - End

#HoltWinters with trend and season- Begin
holtStockPriceForecastWithSeasonAndTrend <- HoltWinters(Stocks, beta = TRUE, gamma = TRUE)
head(holtStockPriceForecastWithSeasonAndTrend$fitted)
TrainActualsWithSeason <- Stocks[53:nrow(Train)]

ErrorMetrics <- data.frame(ErrorMetrics, HoltWintersTrainWithSeasonAndTrend = regr.eval(TrainActualsWithSeason, holtStockPriceForecastWithSeasonAndTrend$fitted[, 1]))

holtStockPriceForecastWithSeasonAndTrend_Forecasts <- forecast(holtStockPriceForecastWithSeasonAndTrend, h = 4)
test_predsWithSeason <- data.frame(holtStockPriceForecastWithSeasonAndTrend_Forecasts)$Point.Forecast

ErrorMetrics <- data.frame(ErrorMetrics, HoltWintersTestWithSeasonAndTrend = regr.eval(test_actuals, test_predsWithSeason))
View(ErrorMetrics)
#HoltWinters with trend and season- End

#Arima - Begin
#Auto arima on timeSeries data calculated above - Stocks
Stock_Arima <- auto.arima(Stocks, ic = 'aic')

#Evaluate using Acf and pacf on stock_arima
acf(as.numeric(Stock_Arima$residuals), lag.max = 30, main = "Residuals ACF Plot")
pacf(as.numeric(Stock_Arima$residuals), lag.max = 30, main = "Residuals PACF Plot")
residuals(Stock_Arima)
TrainActualsWithArima <- Stocks[1:104]

ErrorMetrics <- data.frame(ErrorMetrics, AutoArimaTrain = regr.eval(TrainActualsWithArima, Stock_Arima$fitted))


StockArimaForecasts <- forecast(Stock_Arima, h = 4)
test_preds_AutoArima <- data.frame(StockArimaForecasts)$Point.Forecast
test_actuals <- Test$closingPrice

ErrorMetrics <- data.frame(ErrorMetrics, AutoArimaTest = regr.eval(test_actuals, test_preds_AutoArima))
#Arima - End

View(ErrorMetrics)
