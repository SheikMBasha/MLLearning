#remove all variables
rm(list = ls(all = T))

#Library references
library(DMwR)
library(caret)
library(xgboost)
library(caret)

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

#sum(raw$Customer.Lifetime.Value > 40000)

#remove variables having target more than 30k, sensing those are outliers
raw <-  subset(raw,raw$Customer.Lifetime.Value < 40000)

#Check structure of variables , get unique count and sum of NA's
str(raw)
unique(raw$Education)
sum(is.na(raw$Education))
unique(raw$EmploymentStatus)
sum(is.na(raw$EmploymentStatus)) 

#convert income to numeric
raw$Income <- as.numeric(as.character(raw$Income))
str(raw)

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

#Update REcentlyClaimed to Yes or No based on last claim value less than 12
raw$RecentlyClaimed <- ifelse(raw$Months.Since.Last.Claim < 12, "Yes", "No")
raw$RecentlyClaimed <- as.factor(raw$RecentlyClaimed)
str(raw$RecentlyClaimed)

#Update VehicleSize into categories
str(raw$Vehicle.Size)
raw$Vehicle.Size <- ifelse(raw$Vehicle.Size == 1, "Large", ifelse(raw$Vehicle.Size == 2, "Medsize", "Small"))
raw$Vehicle.Size <- as.factor(raw$Vehicle.Size)

#which(rownames(raw) %in% c(2681, 4183, 8548))
#raw <- raw[-c(2592, 4050, 8279),]

#KNN imputation
raw <- knnImputation(raw, 3)


#Remove ID and GeoLocation Column
raw <- subset(raw, select = -c(CustomerID, Location.Geo))
#below is taken from importance
raw <- subset(raw, select = c(Customer.Lifetime.Value, Number.of.Policies, Monthly.Premium.Auto, Total.Claim.Amount,
              Income, Months.Since.Last.Claim,
                Months.Since.Policy.Inception, Vehicle.Class, Coverage, Marital.Status))

#below is taken from stepAIC
#raw <- subset(raw, select = c(Customer.Lifetime.Value,Coverage, EmploymentStatus,
    #Gender, Monthly.Premium.Auto, Number.of.Open.Complaints, RecentlyClaimed,
    #Number.of.Policies, Renew.Offer.Type, Vehicle.Class, Vehicle.Size))

#separate numeric variables
numericVars <- raw[, sapply(raw, is.numeric)]

#separate target from numeric variables
numericVarsWithoutTarget <- subset(numericVars, select = -c(Customer.Lifetime.Value))
#Get Target variable
target <- data.frame(Customer.Lifetime.Value = raw$Customer.Lifetime.Value)

#Categorical variables
catVars <- raw[, sapply(raw, is.factor)]
#Dummification
catVars1 <- data.frame((model.matrix(~., data = catVars, contrasts.arg = lapply(catVars, contrasts, contrasts = FALSE))[, -1]))


#Create a final dataframe with numeric, categorical and target variables
final_df <- data.frame(numericVarsWithoutTarget, catVars1, target)

#y <- findCorrelation(cor(numericVarsWithoutTarget), cutoff = .50, verbose = FALSE)
#new_list <- numericVarsWithoutTarget[, - y]

str(final_df)
summary(final_df)

#############################################################

#set seed and split into train and validation
set.seed(1234)

index_train <- createDataPartition(final_df$Customer.Lifetime.Value, p = 0.7, list = F)

pre_train <- final_df[index_train,]
pre_test <- final_df[-index_train,]

# Decoupling target column
train_target <- pre_train$Customer.Lifetime.Value
test_target <- pre_test$Customer.Lifetime.Value
pre_train$Customer.Lifetime.Value <- NULL
pre_test$Customer.Lifetime.Value <- NULL

# Standardize all the real valued variables in the dataset as some models we use might be impacted due to non standardized variables
std_method <- preProcess(pre_train, method = c("center", "scale"))
train_Data <- predict(std_method, pre_train)
test_Data <- predict(std_method, pre_test)
dtrain = xgb.DMatrix(data = as.matrix(train_Data), label = train_target)
dtest = xgb.DMatrix(data = as.matrix(test_Data), label = test_target)


watchlist = list(train = dtrain, test = dtest)
model = xgb.train(data = dtrain, max.depth = 2,
                  eta = 0.2, nthread = 2, nround = 500,
                  watchlist = watchlist,
                  eval.metric = "rmse",
                  objective = "reg:linear", verbose = 1)


summary(model)
importance <- xgb.importance(feature_names = names(train_Data), model = model)
print(importance)
xgb.plot.importance(importance_matrix = importance)

pred_train <- predict(model, as.matrix(train_Data))
pred_test <- predict(model, as.matrix(test_Data))

Error_df <- data.frame(TrainError = regr.eval(train_target, pred_train))
Error_df <- data.frame(Error_df, TestErrror = regr.eval(test_target, pred_test))


r2_train <- cor(pred_train, train_target)
r2_train <- r2_train ^ 2

r2_test <- cor(pred_test, test_target)
r2_test <- r2_test ^ 2

R2_train <- 1 - (sum((train_target - pred_train) ^ 2) / sum((train_target - mean(train_target)) ^ 2))
R2_test <- 1 - (sum((test_target - pred_test) ^ 2) / sum((test_target - mean(test_target)) ^ 2))


