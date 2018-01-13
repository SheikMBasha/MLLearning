rm(list=ls(all=T))


getwd()
setwd("E://Insofe//Week6//Day2//Labwork//20170107_Batch37_CSE7302c_LogisticRegression_Activity")

getwd()


bank_data <- read.table("bank.txt", header = T, sep=";")

str(bank_data)
summary(bank_data)

# no missing values, concluded from summary
is.na(bank_data)
bank_data[!complete.cases(bank_data$age),]

?sample
rows = seq(1,nrow(bank_data),1)
set.seed(786)
trainrows <- sample(rows,(70*nrow(bank_data))/100)

bank_data_train <- bank_data[trainrows,]
bank_data_test <- bank_data[-trainrows,]

log_reg <- glm(y ~ age, data = bank_data_train, family = "binomial")

summary(log_reg)               
# age value in sumary is 0.020, so we will use exp(0.020) = 1.014
#null deviance is 2242.3 less than 2225, that means there is some vaiable missing
# refer video for more details

bank_data_test[1,]
logit <- -2.931194 + 0.020948 * 35
odds = exp(logit)
d <- 1 + odds
odds/d # 0.0999

?predict
predict(log_reg, bank_data_test[1,], type = "response") # 0.099

#building the model on entire dataset
log_reg <- glm(y ~ ., data= bank_data_train, family = "binomial")
summary(log_reg)

#By default if no dataset is given, it takes training data

prob_train <- predict(log_reg, type="response")
head(prob_train)

# to choose a threshold value, use package ROCR
library(ROCR)

pred <- prediction(prob_train, bank_data_train$y)
pred
# The performance() function from the ROCR package helps us extract metrics 
# such as True positive rate, False positive rate etc. from the prediction object, 
# we created above.

# Two measures (y-axis = tpr, x-axis = fpr) are extracted

perf <- performance(pred, measure="tpr", x.measure="fpr")


# Plot the ROC curve using the extracted performance measures (TPR and FPR)

plot(perf, col=rainbow(10), colorize=T, 
     print.cutoffs.at=seq(0,1,0.05))


# Extract the AUC score of the ROC curve and store it in a 
# variable named "auc"

# Use the performance() function on the prediction object created above using the ROCR package, to extract the AUC score

perf_auc <- performance(pred, measure="auc")

# Access the auc score from the performance object

auc <- perf_auc@y.values[[1]]

print(auc)

pred_class <- ifelse(prob_train > 0.1 ,"yes","no")
table(bank_data_train$y, pred_class)

Precision_train <- 303/(551+303)
Recall_train <-  303/(57+303)
library(caret) # package used to compute confusion matrix
confusionMatrix(pred_class,bank_data_train$y, positive="yes")

prob_test <- predict(log_reg, bank_data_test, type="response")
preds_test <- ifelse(prob_test > 0.1, "yes","no")
table(bank_data_test$y, preds_test)


Precision_test <- 136/(223+136)
Recall_test <-  136/(25+136)
# because we are interesting in yes value, by default it picks no
confusionMatrix(preds_test,bank_data_test$y, positive="yes")
# For different threshold values identifying the tpr and fpr

# For different threshold values identifying the tpr and fpr
cutoffs <- data.frame(cut= perf@alpha.values[[1]], 
                      fpr= perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])

# Sorting the data frame in the decreasing order based on tpr
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]

# Plotting the true positive rate and false negative rate based based on the cutoff 
# increasing from 0.1-1
plot(perf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.2,1.7))

## Choose a Cutoff Value
# Based on the trade off between TPR and FPR depending on the business domain, 
# a call on the cutoff has to be made.

# A cutoff of 0.1 can be chosen



# Naive Bayes

weather <- read.csv("weather_nominal.csv", header = T, sep = ",")
names(weather)
summary(weather)
str(weather)
