
getwd()
setwd('E://Insofe//Week7//Lab//Day1//20180120')
rm(list=ls(all=T))

library(datasets)
attach(attitude)


str(attitude)

#For PC
#Step1: mean centering the data
f <- function(x){
  y = x - mean(x)
  return(y)
}

data <- attitude
data_mean <- apply(data[,-1], 2,FUN= f)

#step2:
# compute the variance-covariance matrix
data_cov <- cov(data_mean)

#step3: compute eigen values and vectors
Eig <- eigen(data_cov)

Eig$values
Eig$vectors

FinData <- (data_mean)%*%(Eig$vectors)

#princomp uses spectral decomposition
#prcomp used svd 

pca <- princomp(data[2:7])
pca$loadings
pca$scores

# if we standardize the data use below
data_cor <- cor(data_mean)
pca_cor <- princomp(data_cor, cor = T)
pca_cor$scores

#project the unseen data on the model and see how it looks in new dimensions
predict(pca,data[2:7])

plot(pca)
pca$loadings
plot(pca_cor)
screeplot(pca)

#Steps to do linear regression with PCA involved
#1. standardize the data.
#2. split the data.
rows <- seq(1, nrow(data),1)
set.seed(123)
trainRows <- sample(rows, 70* nrow(data)/100)
train <- data[trainRows,]
test <- data[-trainRows,]
#3. Do PCA on the data.

pca_train <- princomp(data[2:7],cor = T)
pca_train$loadings
pca_train$scores
plot(pca_train)

lm(train$rating~ data.frame(pca_train$scores))
# Assignment
# write a function to store mean and sd of train data
# also explore preprocessing function from caret package