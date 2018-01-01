
##----------------------------------------------------------------------------------##
##--- SIMPLE LINEAR REGRESSION MODEL building on BigMac dataset - ASSIGNMENT -------##
##----------------------------------------------------------------------------------##


##--- Step 1: Clear environment variables ------------------------------------------##


##__________________________________________________________________________________##


##--- Step 2: Set working Directory ------------------------------------------------##


##__________________________________________________________________________________##


##--- Step 3: Read the data from the csv file --------------------------------------##


##__________________________________________________________________________________##


##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop any irrelevant attribute(s):


## Summary of the data and look for any missing values:



## Correlation and Covariance between the attributes:




#Describe how the covarainace and correlation coefficients are for the BigMac dataset


#Do the attributes have a good enough correlation coefficient to support linear regression model building?


##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (80:20) ratio





##__________________________________________________________________________________##


##--- Step 6: Linear regression model building--------------------------------------##


## Summary of model:


#Extract the intercept coefficient from the linear regression model



#Extract the residual values


##__________________________________________________________________________________##


##--- Step 7: Check for validity of linear regression assumptions ------------------##
#HINT: plot the 4 graphs to check. Write your comments




##__________________________________________________________________________________##


##--- Step 8: Predict on testdata --------------------------------------------------##



##__________________________________________________________________________________##


##--- Step 9: Error Metrics --------------------------------------------------------##


#Error verification on train data



#Error verification on test data



##__________________________________________________________________________________##


##--- Step 10: Confidence and Prediction Intervals----------------------------------##
#Find the confidence and prediction intervals and plot them for the WHOLE dataset







##__________________________________________________________________________________##
#-----------------------end---------------------------------------------------------##