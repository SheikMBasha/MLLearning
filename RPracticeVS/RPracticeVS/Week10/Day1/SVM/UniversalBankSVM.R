library(MASS)
rm(list = ls(all=  T))
getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Week10\\Day1\\SVM")

UniversalBankRaw <-  read.csv("UniversalBank.csv", header = T, sep = ",")

str(UniversalBankRaw)

UniversalBankRaw <- subset(UniversalBankRaw, select = -c(ZIP.Code, ID))
UniversalBankRaw$Education <- as.factor(UniversalBankRaw$Education)
UniversalBankRaw$Securities.Account <- as.factor(UniversalBankRaw$Securities.Account)
UniversalBankRaw$CD.Account <- as.factor(UniversalBankRaw$CD.Account)
UniversalBankRaw$Online <- as.factor(UniversalBankRaw$Online)
UniversalBankRaw$CreditCard <- as.factor(UniversalBankRaw$CreditCard)
UniversalBankRaw$Personal.Loan <- as.factor(UniversalBankRaw$Personal.Loan)
numericVars <- UniversalBankRaw[, sapply(UniversalBankRaw, is.numeric)]
catVars <- UniversalBankRaw[, sapply(UniversalBankRaw, is.factor)]

library(vegan)
numericStd <- decostand(numericVars, "range")
library(dummies)
#catvars <- dummy(catVars)
#cat <- model.matrix( ~ catVars$Education + catVars$Securities.Account + catVars$CD.Account
                #+ catVars$Online + catVars$CreditCard)
#catvars <- model.matrix(catVars$Personal.Loan ~ catVars$Education + catVars$Securities.Account + catVars$CD.Account
#+ catVars$Online + catVars$CreditCard)
#install.packages("mlr")
library(mlr)
target <- subset(catVars, select = c(Personal.Loan))
catVars <- subset(catVars, select = -c(Personal.Loan))
catVarsWithoutDummy <- subset(catVars, select = c(Securities.Account, CD.Account, Online, CreditCard))
catVarsWithDummy <-  subset(catVars, select=c(Education))

catVarsWithDummy <- model.matrix(~catVarsWithDummy$Education)
catVarsWithDummy[2:3]
UniversalBank <- data.frame(numericVars, catVarsWithoutDummy, catVarsWithDummy[,2:3], target)
colnames(UniversalBank)

library(caret)
set.seed(123)
datapart <- createDataPartition(UniversalBank$Personal.Loan, times = 1, p = 0.7, list = F)
train <- UniversalBank[datapart,]
test <- UniversalBank[-datapart,]

x <- subset(UniversalBank, select = -c(Personal.Loan))
y <- subset(UniversalBank, select = c(Personal.Loan))

library(e1071)
?svm
model <- svm()