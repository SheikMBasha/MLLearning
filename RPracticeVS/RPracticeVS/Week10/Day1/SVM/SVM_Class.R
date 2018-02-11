#SVM can handle higher dimensions (compared to logistic)
# we have margins here in SVM.

# Estimate for weights and bias.
# there would be some noise points in data and in order to make model more
# generalised we use C parameters.
# to get it generalised we introduced Slag variables and C is the weightage of SLAG variables.

# C increases, Margin decreases

#Kernel tricks:
# if data is not linearly separable, we move into higher dimensions
# for polynomial and radial bias we have gamma paramter,
# higher the gamma paramter more ovverfitting would occour.


#Grid search (Parameter tuning)
#for c we search for 1,2,3
#for gamma its 4,5

# for each combination it does k-fold and does cross validation
# ex: 1-4, 1-5, 2-4,2-5,3-4, 3-5

# preprocessing - scaling and dummification
# X marks on plot are support vectors
# round shapes are data points which are not support vectors

#misclassification error - values for which original values dont match with predicted
# values
#Ex: actual : MFMM, predicted : MMMM to misclassification error is 0.25 or 25%
# value same as 1-Accuracy


# tune.svm
# gama = 10^(-6:-1), goes incremetnal 10 power -6 , 10 poewer -5 and goes on till -1
# dispersion in tune.svm is standard deviation
# error is mean error in tune.svm
# higher the gamma value model overfits

#create a model with tune.svm and create a new svm model with tuning parameters got in tune.svm
