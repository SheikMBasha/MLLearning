# Bagging is used  for variance.
# Boosting is for bias.

# baggings
# random sample data with replacment and create model.
# each selection made is independent of each other. Uniform probability of each data point.
# Boosting
# ADA boost and XG boost
# XG boost - algorithm performance wise is better and much optimised.

# in ADA boost
# learn model 1 and take out point which is wrongly classified say X1
# increase probabiliyt of X1, its chances of geting probability is higher and will be accurate on M2

# in XG boost

#(w/z)e power ( -alpha * h(i) * yi)
# check notes for detail 

#In ADA boost we go with weights
# In XG boost - takes L1 and L2 and can also parallelize,
# Model 1 makes prediction y cap, actual is y, residual is y-ycap

# In Gradient Boost - its not parallelize
# If accuracy is already 90, no need to use, because its affinity to overfit is very high.
# XG boost with classification, use misclassification error.