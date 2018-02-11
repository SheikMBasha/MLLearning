# Cleaning the work space
rm(list = ls(all=TRUE))

## Agenda 

# * Read in the data
# 
# * Data Pre-processing
# 
# * Build Multiple Models
# 
# * Stack 'em up
# 
# * Report Metrics of the various Models on Test Data

# Reading & Understanding the Data

# Read in the .csv file
# change your working directory using the "setwd()" function, if your dataset is located elsewhere
getwd()
setwd("E:\\Insofe\\RPracticeVS\\RPracticeVS\\Week10\\Day1\\RandomForest")
cancer_data <- read.csv("cancer_diagnosis.csv")

# Get a feel for the data using the str() function 
str(cancer_data)
summary(cancer_data)

table(cancer_data$Cancer)
# Let's look at the head and tail of the dataset

head(cancer_data)
tail(cancer_data)

# Data Pre-processing
# Let's convert the Cancer column into a factor, because it was read in as a numeric attribute (1 is if the patient has cancer and 0 is if the patient does not have cancer)
cancer_data$Cancer <- as.factor(as.character(cancer_data$Cancer))

# Let's now remove the irrelevant column of "id" from the dataset
# cancer_data <- cancer_data[ , !(colnames(cancer_data) %in% "id")]
cancer_data$id <- NULL

# Let's verify if there are any missing values in the dataset
sum(is.na(cancer_data))

# Split the dataset into train and test using using stratified sampling using the caret package
library(caret)

set.seed(1234)

index_train <- createDataPartition(cancer_data$Cancer, p = 0.7, list = F)

pre_train <- cancer_data[index_train, ]

pre_test <- cancer_data[-index_train, ]

# Standardize all the real valued variables in the dataset as some models we use might be impacted due to non standardized variables

# Let's use the preProcess() function from the caret package to standardize the variables, using just the data points in the training data
#?preProcess
std_method <- preProcess(pre_train, method = c("center", "scale"))

train_data <- predict(std_method, pre_train)

test_data <- predict(std_method, pre_test)

train_target <- train_data$Cancer
train_data$Cancer <- NULL
test_target <- test_data$Cancer
test_data$Cancer <- NULL


# Building Multiple Models

## Random Forest

 # We can build a random forest model using the randomForest() function from the randomForest() package

 # Below, we use the default parameters to build the random forest model

library(randomForest)
#sample of 5 variables in each try and create 50 trees
model_rf <- randomForest(train_target ~ . , train_data,ntree = 50,mtry = 5)

 # We can also look at variable importance from the built model using the importance() function and visualise it using the varImpPlot() funcion
importance(model_rf) # gives information on which variable is more imp and gives more information

varImpPlot(model_rf)

 # Store predictions from the model
preds_rf <- predict(model_rf, test_data)
preds_rf_prob <- predict(model_rf, test_data, type = "prob")

confusionMatrix(preds_rf, test_target)

# Predict on the train data
preds_train_rf <- predict(model_rf)
preds_train_rf_prob <- predict(model_rf, type = "prob")

## KNN
library('class')
# KNN is a powerful classifier that can understand local, smaller patterns in the data and hence we'll us it in our ensemble model

# We'll build our KNN model, using the knn3() function from the caret package

preds_k <- knn(train_data, test_data, cl=train_target , k = 3, prob=TRUE)
preds_knn <- as.vector(preds_k)
preds_knn_prob <- attr(preds_k,'prob')

confusionMatrix(preds_knn, test_target)

## Train Predictions for Meta_Learner
train_knn <- knn(train_data, train_data, cl=train_target , k = 3, prob = TRUE)
preds_train_knn <- as.vector(train_knn)
preds_train_knn_prob <- attr(train_knn,'prob')

preds_train_knn_prob <- ifelse(preds_train_knn>0,preds_train_knn_prob,1-preds_train_knn_prob)

## Decision Trees

# * Let's now go ahead and build our CART decision tree using the rpart() function from the rpart() package

library(rpart)

model_dt <- rpart(train_target ~ . , train_data)

# * The predictions here too are probabilities for each of the two classes in the target variable

preds_dt <- predict(model_dt, test_data, type = "class")
preds_dt_prob <- predict(model_dt, test_data)

preds_train_dt_prob <- predict(model_dt)
preds_train_dt <- ifelse(preds_train_dt_prob[, 1] > preds_train_dt_prob[, 2], 0, 1)

confusionMatrix(preds_dt, test_target)

# Building a Stacked Ensemble

# * Before building a stacked ensemble model, we have to coallate all the predictions on the train and validation datasets into a dataframe

# Getting all the predictions on the train data into a dataframe

train_preds_df <- data.frame(rf = preds_train_rf, knn = preds_train_knn,
                             tree = preds_train_dt,
                             Cancer = train_target)

# convert the target variable into a factor
train_preds_df$Cancer <- as.factor(as.character(train_preds_df$Cancer))


# * Use the sapply() function to convert all the variables other than the target variable into a numeric type
numeric_st_df <- sapply(train_preds_df[, !(names(train_preds_df) %in% "Cancer")], 
                        function(x) as.numeric(as.character(x)))

cor(numeric_st_df)

## Probability Correlation for Cancer 1
probability_correlation <- data.frame(rf_1 = preds_train_rf_prob[,2],
                                      knn_1 = preds_train_knn_prob,
                                      dt_1 = preds_train_dt_prob[,2])
cor(probability_correlation)

# * Now, since the outputs of the various models are extremely correlated let's use PCA to reduce the dimensionality of the dataset
pca_stack <- prcomp(numeric_st_df, scale = F)
summary(pca_stack)

# Transform the data into the principal components using the predict() fucntion and keep only 3 of the original components
predicted_stack <- as.data.frame(predict(pca_stack, numeric_st_df))[1:2]

# Now, add those columns to the target variable (Cancer) and convert it to a data frame
stacked_df <- data.frame(predicted_stack, Cancer = train_preds_df$Cancer)

# * We will be building a logistic regression on the dataset to predict the final target variable
stacked_model <- glm(Cancer ~ . , data = stacked_df,family = "binomial")

# Getting all the predictions from the validation data into a dataframe
stack_df_test <- data.frame(rf = preds_rf, knn = preds_knn,
                            tree = preds_dt, Cancer = test_target)

# Convert the target variable into a factor
stack_df_test$Cancer <- as.factor(stack_df_test$Cancer)

# Convert all other variables into numeric
numeric_st_df_test <- sapply(stack_df_test[, !(names(stack_df_test) %in% "Cancer")],
function(x) as.numeric(as.character(x)))

# Apply dimensionality reduction on the numeric attributes
predicted_stack_test <- as.data.frame(predict(pca_stack, numeric_st_df_test))[1:2]

# Combine the target variable along with the reduced dataset
stacked_df_test <- data.frame(predicted_stack_test, Cancer = stack_df_test$Cancer)

# * Now, apply the stacked model on the above dataframe
preds_st_test <-  predict(stacked_model, stacked_df_test,type = "response")
preds_st_test <- ifelse(preds_st_test > 0.5,"1","0")


## Using the confusionMatrix() function from the caret package to get the evaluation metrics on the test data for the various models built today

# Random Forest
confusionMatrix(preds_rf, test_target)

# KNN
confusionMatrix(preds_knn, test_target)

# CART Tree
confusionMatrix(preds_dt, test_target)

# Stacked Model
confusionMatrix(preds_st_test, stacked_df_test$Cancer)


