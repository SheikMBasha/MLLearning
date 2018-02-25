#rm(list = ls(all = T))

getwd()
setwd("E://Insofe//Mith")

rawTest <- read.csv("Test.csv", header = T, sep = ",")

dim(rawTest)
#raw <- subset(raw, raw$Customer.Lifetime.Value < 30000)
sum(is.na(rawTest))
#sum(is.na(test))

library(DMwR)
sum(is.na(rawTest))

str(rawTest)

rawTest$Income <- as.numeric(as.character(rawTest$Income))
#Replace NA complaints with 0
rawTest$Number.of.Open.Complaints[is.na(rawTest$Number.of.Open.Complaints)] <- 0


str(rawTest$Vehicle.Size)
rawTest$Vehicle.Size <- ifelse(rawTest$Vehicle.Size == 1, "Large", ifelse(rawTest$Vehicle.Size == 2, "Medsize", "Small"))
rawTest$Vehicle.Size <- as.factor(rawTest$Vehicle.Size)

sum_nas <- function(x) {
    return(sum(is.na(x)))
}

check_col_na_count_test <- data.frame(apply(rawTest, 2, FUN = sum_nas))
#test_check_col_na_count <- data.frame(apply(test, 2, FUN = sum_nas))

#Replace Open complaints having  NA with 0
sum(is.na(rawTest$Number.of.Open.Complaints))
rawTest$Number.of.Open.Complaints[is.na(rawTest$Number.of.Open.Complaints)] <- 0

#Replace Premium Auto having  NA with 0
sum(is.na(rawTest$Monthly.Premium.Auto))
rawTest$Monthly.Premium.Auto[is.na(rawTest$Monthly.Premium.Auto)] <- 0

#Update REcentlyClaimed to Yes or No based on last claim value less than 12
rawTest$RecentlyClaimed <- ifelse(rawTest$Months.Since.Last.Claim < 12, "Yes", "No")
rawTest$RecentlyClaimed <- as.factor(rawTest$RecentlyClaimed)
str(rawTest$RecentlyClaimed)

#Make sure levels are same in test and train

levels(rawTest$Vehicle.Size)
levels(raw$Vehicle.Size)
levels(rawTest$EmploymentStatus) <- levels(raw$EmploymentStatus)
#levels(rawTest$Vehicle.Size) <- levels(raw$Vehicle.Size)
custId <- rawTest$CustomerID
rawTest$CustomerID <- NULL
rawTest$Location.Geo <- NULL
#rawTest$Months.Since.Last.Claim <- NULL
colnames(rawTest)

#Check NA and impute
sum(is.na(rawTest))
rawTest <- knnImputation(rawTest, 5)

#Separate numeric variables
numericVarsTest <- rawTest[, sapply(rawTest, is.numeric)]

rawTest$Vehicle.Size <-  NULL
catVarsTest <- rawTest[, sapply(rawTest, is.factor)]
#Dummification

catVars1Test <- data.frame((model.matrix(~., data = catVarsTest, contrasts.arg = lapply(catVars, contrasts, contrasts = FALSE))[, -1]))


#Create a final dataframe with numeric, categorical and target variables
final_df_test <- data.frame(numericVarsTest, catVars1Test)

dim(final_df_test)
colnames(final_df_test)
dim(rawTest)
colnames(rawTest)
colnames(raw)
dim(pre_train)
colnames(pre_train)

#rawTest <- subset(rawTest, select = c(Number.of.Policies, Monthly.Premium.Auto, Total.Claim.Amount, Income, Months.Since.Last.Claim, Months.Since.Policy.Inception, Vehicle.ClassFour.Door.Car, Vehicle.ClassLuxury.Car, Vehicle.ClassLuxury.SUV, Vehicle.ClassSports.Car, Vehicle.ClassSUV, Vehicle.ClassTwo.Door.Car, CoverageBasic, CoverageExtended, CoveragePremium, Marital.StatusDivorced,
#Marital.StatusMarried, Marital.StatusSingle))

#rawTestStd <- predict(std_method, rawTest)
#rm(final_df_test_std,pred_testFinal,finalSumbission)
final_df_test_std <- predict(std_method, final_df_test)

pred_testFinal <- predict(model, as.matrix(final_df_test_std))

finalSumbission <- data.frame(custId, pred_testFinal)