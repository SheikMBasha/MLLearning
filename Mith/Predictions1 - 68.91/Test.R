#rm(list = ls(all = T))

getwd()
setwd("E://Insofe//Mith")

rawTest <- read.csv("Test.csv", header = T, sep = ",")
dim(rawTest)
#raw <- subset(raw, raw$Customer.Lifetime.Value < 30000)
sum(is.na(rawTest))
#sum(is.na(test))

library(DMwR)
#x <- manyNAs(raw, nORp = 0.2)
#raw <- raw[-manyNAs(raw, nORp = 0.2),]
#rawTest <- rawTest[(rowSums(is.na(rawTest)) <= 2),]
sum(is.na(rawTest))

str(rawTest)
unique(rawTest$Education)
sum(is.na(rawTest$Education))
unique(rawTest$EmploymentStatus)
sum(is.na(rawTest$EmploymentStatus)) # think about imputation
unique(rawTest$Income)
#raw$IncomeN <- as.numeric(raw$Income)
rawTest$Income <- as.numeric(as.character(rawTest$Income))
#remove location geo and keep location code

sum(is.na(rawTest$Number.of.Open.Complaints))
rawTest$Number.of.Open.Complaints[is.na(rawTest$Number.of.Open.Complaints)] <- 0
dim(rawTest)
unique(rawTest$Policy)
sum(is.na(rawTest$Policy)) # use knn to impute the policy details

str(rawTest)
unique(rawTest$Renew.Offer.Type)
sum(is.na(rawTest$Renew.Offer.Type))

unique(rawTest$Sales.Channel)
sum(is.na(rawTest$Sales.Channel))
#RenewOffertype and sales channel have same fields with na

unique(rawTest$Total.Claim.Amount)
sum(is.na(rawTest$Total.Claim.Amount))

unique(rawTest$Vehicle.Class)
sum(is.na(rawTest$Vehicle.Class))

unique(rawTest$Vehicle.Size)
sum(is.na(rawTest$Vehicle.Size))
str(rawTest)

#str(rawTest$Vehicle.Size)
#rawTest$Vehicle.Size <- ifelse(rawTest$Vehicle.Size == 1, "Large", ifelse(rawTest$Vehicle.Size == 2, "Medsize", "Small"))
rawTest$Vehicle.Size <- as.factor(rawTest$Vehicle.Size)

sum_nas <- function(x) {
    return(sum(is.na(x)))
}

check_col_na_count_test <- data.frame(apply(rawTest, 2, FUN = sum_nas))
#test_check_col_na_count <- data.frame(apply(test, 2, FUN = sum_nas))

sum(is.na(rawTest$Number.of.Open.Complaints))
rawTest$Number.of.Open.Complaints[is.na(rawTest$Number.of.Open.Complaints)] <- 0

sum(is.na(rawTest$Monthly.Premium.Auto))
rawTest$Monthly.Premium.Auto[is.na(rawTest$Monthly.Premium.Auto)] <- 0

#max(rawTest$Customer.Lifetime.Value)

#raw <- subset(raw, raw$Customer.Lifetime.Value < 20000)

levels(rawTest$Education1)
rawTest$Education1 <- factor(rawTest$Education, levels = c("High School or Below", "College", "Bachelor", "Doctor", "Master"), ordered = TRUE)
rawTest$Education <- NULL

rawTest$Sales.Channel <- ifelse(rawTest$Sales.Channel == "Branch", "Branch", "Remote")
rawTest$Sales.Channel <- as.factor(rawTest$Sales.Channel)
str(rawTest$Sales.Channel)

rawTest$RecentlyClaimed <- ifelse(rawTest$Months.Since.Last.Claim < 12, "Yes", "No")
rawTest$RecentlyClaimed <- as.factor(rawTest$RecentlyClaimed)
str(rawTest$RecentlyClaimed)
#rawTest$Months.Since.Last.Claim <- NULL
sum(is.na(rawTest))
levels(rawTest$Vehicle.Size)
levels(raw$Vehicle.Size)
levels(rawTest$EmploymentStatus) <-  levels(raw$EmploymentStatus)
custId <- rawTest$CustomerID
rawTest$CustomerID <- NULL
rawTest$Location.Geo <- NULL
rawTest$Months.Since.Last.Claim <-  NULL
colnames(rawTest)

rawTest <- knnImputation(rawTest, 5)
dim(rawTest)
pred_testFinal <- predict(model_rf2, rawTest)

finalSumbission <- data.frame(custId,pred_testFinal)