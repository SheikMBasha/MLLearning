rm(list = ls(all = T))

getwd()
setwd('E://Insofe//RPracticeVS//RPracticeVS//Week7//Regularization Assignment')

pbobinraw = read.csv('pbo_bin.csv', header = T, sep = ",")
str(pbobinraw)
summary(pbobinraw)
pbobinraw[is.na(pbobinraw)]

numeric_var <- pbobinraw[, sapply(pbobinraw, is.numeric)]
num_var_final <- subset(numeric_var, select = -c(pbo))
factor_var <- pbobinraw[, sapply(pbobinraw, is.factor)]
target_var <- subset(pbobinraw, select = c(pbo))

#Converting categorical attributes into dummy variables

#dummy_factor_varbl <- model.matrix(pbobinraw$pbo ~ ., factor_val)[, -1]
dummy_factor_var <- model.matrix(pbobinraw$pbo ~ pbobinraw$Freestyle_flag + pbobinraw$IA_Flag)[, -1]
pbobin_data <- data.frame(num_var_final,dummy_factor_var,target_var)

#Split the data into train and test data sets
rows = seq(1, nrow(pbobinraw), 1)
set.seed(123)
trainRows <- sample(rows, 70 * nrow(pbobinraw) / 100)
trainpbo <- pbobin_data[trainRows,]
testpbo <- pbobin_data[-trainRows,]

LinReg <- lm(pbo ~ ., data = trainpbo)
summary(LinReg)

library(DMwR)
RegErros <- data.frame(LinearTrain = regr.eval(trainpbo$pbo, LinReg$fitted.values))

testLinreg <- predict(LinReg, testpbo)
regr.eval(testpbo$pbo, testLinreg)
RegErros <- data.frame(RegErros, LinearTest = regr.eval(testpbo$pbo, testLinreg))
View(RegErros)
# There is a drastic increase in MSE between train and test
# which indicates high variance
# Model is getting overfitted
plot(LinReg)

#observe if there is any change with pbobinraw data without dummyfying variables
#as while doing lm itself do dummification
#Findings: No Change in Result
trainRawpbo <- pbobinraw[trainRows,]
testRawpbo <- pbobinraw[-trainRows,]

LinRegRaw <- lm(pbo ~ ., data = trainRawpbo)
summary(LinRegRaw)

library(DMwR)
LinRegRawErros <- data.frame(Train = regr.eval(trainRawpbo$pbo, LinRegRaw$fitted.values))

testLinregRaw <- predict(LinRegRaw, testRawpbo)
regr.eval(testRawpbo$pbo, testLinregRaw)
LinRegRawErros <- data.frame(LinRegRawErros, Test = regr.eval(testRawpbo$pbo, testLinregRaw))
View(LinRegRawErros)
# There is a drastic increase in MSE between train and test
# which indicates high variance
# Model is getting overfitted



data2 <- as.matrix(data.frame(num_var_final,dummy_factor_var))
traindata2 <- data2[trainRows,]
testdata2 <- data2[-trainRows,]
ytrain <- pbobinraw$pbo[trainRows]
ytest <- pbobinraw$pbo[-trainRows]

library(glmnet)

#Lasso 
fit1 <- glmnet(traindata2, ytrain, alpha = 1)
plot(fit1)
#Model Selection
coef(fit1)
cv_lasso <- cv.glmnet(traindata2, ytrain)
#Result
#$lambda.min = 0.01673831/ 0.013
#$lambda.1se = 0.0741611

plot(cv_lasso)
coef(cv_lasso)
cv_lasso$lambda.min

plot(fit1, xvar = "lambda", label = TRUE)
plot(fit1, xvar = "dev", label = TRUE)

fit2 <- glmnet(traindata2, ytrain, alpha = 1, lambda = cv_lasso$lambda.min)
coef(fit2)
plot(fit2, xvar='lambda', label=TRUE)
#Error Verification
RegErros <- data.frame(RegErros, Lasso_Train = regr.eval(ytrain, predict(fit2, traindata2)))
RegErros <- data.frame(RegErros, Lasso_Test = regr.eval(ytest, predict(fit2,testdata2)))
View(RegErros)

#Ridge Regression
fit_Ridge <- glmnet(traindata2, ytrain, alpha = 0)

# Model selection
coef(fit_Ridge)
cv_Ridge <- cv.glmnet(traindata2, ytrain, alpha = 0)

#Result
#$lambda.min = 0.1003435
#$lambda.1se = 0.587714

plot(cv_Ridge)
coef(cv_Ridge)
plot(fit_Ridge, xvar = "lambda", label = TRUE)

ridge_fit2 <- glmnet(traindata2, ytrain, alpha = 0, lambda = cv_Ridge$lambda.min)
coef(ridge_fit2)

RegErros <- data.frame(RegErros, RidgeTrain = regr.eval(ytrain, predict(ridge_fit2, traindata2)))
RegErros <- data.frame(RegErros, RidgeTest = regr.eval(ytest, predict(ridge_fit2, testdata2)))

View(RegErros)

#ElasticNet

cv_elastic <- cv.glmnet(traindata2, ytrain, alpha = 0.5)
plot(cv_elastic)

elastic_fit2 <- glmnet(traindata2, ytrain, lambda = cv_elastic$lambda.min, alpha = 0.5)
coef(elastic_fit2)

RegErros <- data.frame(RegErros, ElasticTrain = regr.eval(ytrain, predict(elastic_fit2, traindata2)))
RegErros <- data.frame(RegErros, ElasticTest = regr.eval(ytest, predict(elastic_fit2, testdata2)))
View(RegErros)