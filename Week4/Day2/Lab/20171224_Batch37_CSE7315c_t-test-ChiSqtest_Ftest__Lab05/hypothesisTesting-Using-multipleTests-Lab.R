###### Two sample mean Test 

# Method -1 manual compuations 
# read the data set into "machines" data frame 
machines <- read.csv()
# compute the mean for both the samples 
mean(machines$New.machine)
mean(machines$Old.machine)
# compute the sd for both the samples 

# find the difference 
diff <- mean(machines$New.machine) - mean(machines$Old.machine)


# compute the pooled sd 
pooledSd <- sqrt(((sd(machines$New.machine))^2*9+(sd(machines$Old.machine))^2*9)/(10+10-2))

# compute the test statistic 
diff/(pooledSd*sqrt(1/10+1/10))

# getting the critical value 
qt(0.05,10+10-2,lower.tail = T)

  ### Method -2 
t.test(machines$New.machine,machines$Old.machine)

#### Two sample t- test for paired Data 
reg = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28) 
prem = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)

######Chi-square Test##########
observedFrequencies<- c(21,109,62,15)
expectedProportions<- c(0.08,0.47,0.34,0.11) 

# To compare withthe observed frquencies, let us compute the the expected frequencies 

# Computing the calculated value 


# Method-1 : selecting the critical region using p value and
# compare  this value with level of significance (alpha)
# if this value is less than alpha , we reject Null Hypothesis
?pchisq()

# Used Lower tail = F, because, we need the need the area P(X>x)
# For the critical region

# Since the alpha is = 0.05, what will be action ?


# Method 2 : 
# selecting the critical region using critical value and
# compare  this value with the calculate value
# if the critical value is less than tabulated we reject Null Hypothesis
qchisq()


# Method -3 

# Conclusion : 
# All the 3 methods says that , there is no sufficient evidence to reject the Null hypothesis

##########################################################################################################
### Game Vs Gender preference - Chisq 
genderPreference <- matrix(c(200,150,50,250,300,50),nrow=2,byrow = T)

#### Car Car Type Vs Crash scores 


# What is the test I need to apply 

# method -1 

#Manual Calculation:
x1 <- c(643, 655, 702)
x2 <- c(469, 427, 525)
x3 <- c(484, 456, 402)

x <- c(x1,x2,x3)

# computing the totalSS


# computing the withinSS
withinSS1 <- sum((x1-mean(x1))^2 )
withinSS2 <- sum((x2-mean(x2))^2 )
withinSS3 <- sum((x3-mean(x3))^2 )
withinSS <- withinSS1+withinSS2+withinSS3

# betweenSS
betweenSS <- 

# F stat using the critical value method
nr <- 
dr <- 
crticalF <- nr/dr
crticalF
# computing the tabulated value 
?qf()
dfbetweenSS <- 
dfwithinSS <- 
qf(0.05,dfbetweenSS,dfwithinSS,lower.tail = F)

### Conclusion : if the calculated value is less than the tabulated value 
# then we reject Null hypothesis 

# Method-2 
# creating the data frame 
scores = c(643,655,702,469,427,525,484,456,402)
data <- data.frame(scores,group)


# Applying our learning on mtcars data set 

# Q1 Does the variables am and gears are indendent ?
# Which test should you use ? 

# What is the conclusion ? 

## Q2 Does the mpg varies on the am ? 
# create the two vectors one for manual and another for automatic 

# apply the test 

## Q3 Does the mpg depends on am and gear ? 

## Q4 Does the mpg depends on am and cyl ? 

### Summary 
# chi-square test is used to check if there are only categorical variables 
# t test is to check one or two sample tests 
# ANOVA is to apply multiple tests 
