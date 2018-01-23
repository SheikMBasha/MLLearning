rm(list=ls(all=T))
getwd()
setwd('E://Insofe//Week7//Day1//Lab//20180120//housing_data')

housing_raw <- read.csv('housing_data.csv', header = T, sep = ',')

summary(housing_raw)
str(housing_raw)
#is.na(housing_raw)
#na.omit(housing_raw)
housing_numeric <- subset(housing_raw, select = -c(CHAS,RAD))
library(DMwR)
housing_numeric <- knnImputation(housing_numeric)
sum(is.na(housing_numeric))

rows <- seq(1, nrow(housing_numeric),1)
set.seed(123)
trainRows <- sample(rows, 70* nrow(housing_numeric)/100)

housing_train <- housing_numeric[trainRows,]
housing_test <- housing_numeric[-trainRows,]

x_train <- housing_train[, -ncol(housing_train)]
x_test <- housing_test[, -ncol(housing_test)]
y_train <- housing_train[, ncol(housing_train)]
y_test <- housing_test[, ncol(housing_test)]


pca <- princomp(x_train)
pca$scores
pca$loadings

pca_std <- princomp(x_train, cor = T)

summary(pca)
summary(pca_std)

pca_train = predict(pca, x_train)
pca_test = predict(pca, x_test)

pca_std_train = predict(pca_std, x_train)
pca_std_test = predict(pca_std, x_test)

# linear model without pca
train2 = data.frame(x_train, y_train)
test2 = data.frame(x_test,y_test)
LinReg <- lm( y_train ~ ., data=train2)
summary(LinReg)

library(DMwR)
#Error verificatoin on train data
regr.eval(train2$y_train, LinReg$fitted.values)
#Error verification on test data
pred <- predict(LinReg, test2)
regr.eval(test2$y_test,pred)

#using PCA
train3 = data.frame(pca_train[,1:3], y_train)
test3 = data.frame(pca_test[,1:3], y_test)

LinRegpca <- lm(y_train~., data = train3)
summary(LinRegpca)
regr.eval(train3$y_train, LinRegpca$fitted.values)
predpca = predict(LinRegpca, test3)
regr.eval(test3$y_test, predpca)

#using standard pca
train4 = data.frame(pca_std_train[,1:7], y_train)
test4 = data.frame(pca_std_test[,1:7], y_test)

LinRegstdpca <- lm(y_train~., data = train4)
summary(LinRegstdpca)
regr.eval(train4$y_train, LinRegstdpca$fitted.values)
predstdpca = predict(LinRegstdpca, test4)
regr.eval(test4$y_test, predstdpca)
