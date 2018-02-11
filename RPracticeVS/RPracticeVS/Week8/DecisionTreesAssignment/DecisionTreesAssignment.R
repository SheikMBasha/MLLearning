rm(list = ls(all = T))
getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Week8\\DecisionTreesAssignment")

train <- read.csv("Train.csv", header = T, sep = ",")
test <- read.csv("Test.csv", header = T, sep = ",")
str(train)
str(test)

colnames(train)
factorCols <-  c("new_car","used_car","furniture","radio_tv","education","retraining","co_applicant",
                "guarantor", "real_estate", "prop_unkn_none", "other_install", "rent", "own_res", "telephone",
                "foreign", "response")
factorColsTest <- c("new_car", "used_car", "furniture", "radio_tv", "education", "retraining", "co_applicant",
                "guarantor", "real_estate", "prop_unkn_none", "other_install", "rent", "own_res", "telephone",
                "foreign")

train[factorCols] <- lapply(train[factorCols], factor)
test[factorColsTest] <- lapply(test[factorColsTest], factor)

sum(is.na(train))
sum(is.na(test))
#x <- data.frame(train[!complete.cases(train),])
#library(DMwR)
#? manyNAs
#naids <- train[manyNAs(train, 0.2),]
x <- train[manyNAs(train, 0.2),]
rownames(x)
train <-  train[rowSums(is.na(train)) < 2,]
sum(is.na(train))
x <- data.frame(train[!complete.cases(train),])
train <- na.omit(train)

log_reg <- glm(response ~ ., data = train, family = "binomial")
summary(log_reg)


# classification using c50
library(C50)
dim(train)
DT_C50 <- C5.0(response ~ ., data = train,, control = C5.0Control(noGlobalPruning = TRUE, minCases = 10))
summary(DT_C50)
pred_train <- predict(DT_C50, newdata = train, type = "class")
pred_test <- predict(DT_C50, newdata = test, type = "class")
z <- data.frame(pred_test)
library(caret)
confusionMatrix(train$response, pred_train, positive = "1")
printcp(DT_C50)
C5imp(DT_C50, pct = TRUE)
library(rpart)
library(rpart.plot)
rpart.plot(DT_C50)