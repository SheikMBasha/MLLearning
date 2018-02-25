#remove all variables
rm(list = ls(all = T))

#Library references
library(DMwR)
library(caret)
library(randomForest)

#GET and SET working directory
getwd()
setwd("E://Insofe//Mith")

#Read the file
raw <- read.csv("Train.csv", header = T, sep = ",")

#check total na's
sum(is.na(raw))

#remove rows which have more than 2 na's
raw <- raw[(rowSums(is.na(raw)) <= 2),]
sum(is.na(raw))

#Check structure of variables , get unique count and sum of NA's
str(raw)
unique(raw$Education)
sum(is.na(raw$Education))
unique(raw$EmploymentStatus)
sum(is.na(raw$EmploymentStatus)) 
unique(raw$Income)

#convert income to numeric
raw$Income <- as.numeric(as.character(raw$Income))

#If no. of complaints are NA, replace with 0
sum(is.na(raw$Number.of.Open.Complaints))
raw$Number.of.Open.Complaints[is.na(raw$Number.of.Open.Complaints)] <- 0

#Check na and unique of policy
unique(raw$Policy)
sum(is.na(raw$Policy)) 

#Check na and unique of Renew offer type
str(raw)
unique(raw$Renew.Offer.Type)
sum(is.na(raw$Renew.Offer.Type))

#Check na and unique of Channel
unique(raw$Sales.Channel)
sum(is.na(raw$Sales.Channel))

#RenewOffertype and sales channel have same fields with na
#Check na and unique of Claim Amount
unique(raw$Total.Claim.Amount)
sum(is.na(raw$Total.Claim.Amount))

#Check na and unique of Class
unique(raw$Vehicle.Class)
sum(is.na(raw$Vehicle.Class))

#Check na and unique of Vehicle Size
unique(raw$Vehicle.Size)
sum(is.na(raw$Vehicle.Size))
str(raw)

str(raw$Vehicle.Size)
#Classify vehicle size into Large, Med and small
raw$Vehicle.Size <- ifelse(raw$Vehicle.Size == 1, "Large", ifelse(raw$Vehicle.Size == 2, "Medsize", "Small"))
raw$Vehicle.Size <- as.factor(raw$Vehicle.Size)

#function to check na per column
sum_nas <- function(x) {
    return(sum(is.na(x)))
}

#check na per column in dataframe
check_col_na_count <- data.frame(apply(raw, 2, FUN = sum_nas))

#Check na and unique of Open Complaints
sum(is.na(raw$Number.of.Open.Complaints))
#REplace open complaints with 0
raw$Number.of.Open.Complaints[is.na(raw$Number.of.Open.Complaints)] <- 0

#REplace premium with 0
sum(is.na(raw$Monthly.Premium.Auto))
raw$Monthly.Premium.Auto[is.na(raw$Monthly.Premium.Auto)] <- 0

max(raw$Customer.Lifetime.Value)


#Ordering factors of education
levels(raw$Education1)
raw$Education1 <- factor(raw$Education, levels = c("High School or Below", "College", "Bachelor", "Doctor", "Master"), ordered = TRUE)
raw$Education <- NULL

#Feature Engg
#Reducing Channel to only Branch and Remote
raw$Sales.Channel <- ifelse(raw$Sales.Channel == "Branch", "Branch", "Remote")
raw$Sales.Channel <- as.factor(raw$Sales.Channel)
str(raw$Sales.Channel)

#Categorizing customer as recentClaimer 
raw$RecentlyClaimed <- ifelse(raw$Months.Since.Last.Claim < 12, "Yes", "No")
raw$RecentlyClaimed <- as.factor(raw$RecentlyClaimed)
str(raw$RecentlyClaimed)
raw$Months.Since.Last.Claim <- NULL
sum(is.na(raw))

#KNN Imputation with 5 nearest neighbours to fill NA's
raw <- knnImputation(raw, 5)

#Remove GeoLocation and CustomerID
raw$Location.Geo <- NULL
raw$CustomerID <- NULL

#Data separation in train and validation 
set.seed(123) #setting seed value

#Startified sampling in 70:30 ratio
rows <- createDataPartition(raw$Customer.Lifetime.Value, times = 1, p = 0.7, list = FALSE)

#Get data into train and test
train <- raw[rows,]
test <- raw[-rows,]

#Check correlation
c <- cor(raw[, sapply(raw, is.numeric)])

##########RF##################
#Removing target variable from dataset for train and test
train_target <- train$Customer.Lifetime.Value
train$Customer.Lifetime.Value <- NULL
test_target <- test$Customer.Lifetime.Value
test$Customer.Lifetime.Value <- NULL



##Build model with Random Forest  with 8 random variable and 100 trees i.e.
#ntree = 20, mtry = 20


model_rf2 <- randomForest(train_target ~ ., train, ntree = 100, mtry = 8)
#check for important variables
importance(model_rf2)
#Plot the important variables
varImpPlot(model_rf2)

#Predict train and test values from model
pred_train2 <- predict(model_rf2, train)
pred_test2 <- predict(model_rf2, test)

dim(train)
colnames(train)

#Calculate Error metrics and capture in Error frame
ErrorFrame = data.frame(Train20_20 = regr.eval(train_target, pred_train2))
ErrorFrame = data.frame(ErrorFrame, Test20_20 = regr.eval(test_target, pred_test2))

#Calculate R2 using correlation
r2_train <- cor(pred_train2, train_target)
r2_train <- r2_train ^ 2

r2_test <- cor(pred_test2, test_target)
r2_test <- r2_test ^ 2
actual <- train_target
perdicted <- pred_train2

#Calculating R2 with formula  1 - SSE/SST
R2_train <- 1 - (sum((train_target - pred_train2) ^ 2) / sum((train_target - mean(train_target)) ^ 2))
R2 <- 1 - (sum((test_target - pred_test2) ^ 2) / sum((test_target - mean(test_target)) ^ 2))

