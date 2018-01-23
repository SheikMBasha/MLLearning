rm(list=ls(all=T))
getwd()

setwd("E://Insofe//Week7//Day1//Lab//20180120//Onlinenewspopularity_OK")

newsData <- read.csv('OnlineNewsPopularity.csv', header = T, sep = ',')
str(newsData)

#As url and timedelta are non predicted values, we can remove those from dataset 
# and moreover those are categorical values and url cannot be converted to numeric data 
# if dummification is used it will be too many variables.
newsDataFiltered <- subset(newsData, select = -c(url,timedelta))
str(newsDataFiltered)

is.na(newsDataFiltered)

rows <- seq(1, nrow(newsDataFiltered),1)
set.seed(123)

trainRows <- sample(rows, 70*nrow(newsDataFiltered)/100)


newsDataFiltered_Train <- newsDataFiltered[trainRows,]
newsDataFiltered_Test <-  newsDataFiltered[-trainRows,]


x_train <- newsDataFiltered_Train[,-ncol(newsDataFiltered_Train)]
x_test <- newsDataFiltered_Test[,-ncol(newsDataFiltered_Test)]
y_train <- newsDataFiltered_Train[,ncol(newsDataFiltered_Train)]
y_test <- newsDataFiltered_Test[,ncol(newsDataFiltered_Test)]

pca <- princomp(x_train, cor = T)
summary(pca)
pca$scores
pca$loadings

pca_train <- predict(pca, x_train)
pca_test <- predict(pca, x_test)

TrainData <- data.frame(pca_train[,1:30], y_train)
TestData <- data.frame(pca_test[,1:30], y_test)

LinReg <- lm(y_train ~ .,data = TrainData)
summary(LinReg)

#rm(Errors)
library(DMwR)
Errors <-  data.frame(Train = regr.eval(TrainData$y_train, LinReg$fitted.values))
test <- predict(LinReg,TestData)
Errors <- data.frame(Errors, Test = regr.eval(TestData$y_test, test))


LinReg1 <- lm(shares ~ . , data =  newsDataFiltered_Train)
summary(LinReg1)
regr.eval(newsDataFiltered_Train$shares, LinReg1$fitted.values)
x <- predict.lm(LinReg1, newsDataFiltered_Test[,-ncol(newsDataFiltered_Test)])
