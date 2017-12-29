rm(list=ls(all=TRUE))

#Q1

q1 <- c(68, 42, 51, 57, 56, 80, 45, 39, 36, 79)
m <- mean(q1)
s <- sd(q1)
criti <- qt((0.05/2), length(q1)-1, lower.tail = F)

lowLimit <- m - criti* s/sqrt(length(q1))
upperLimit <- m + criti* s/sqrt(length(q1))
print(c(lowLimit,upperLimit))

t.test(q1, alternative = "two.sided")


#Q2
# H0: mean (new machine) > mean (old machine)
# H1 : mean(new machine ) <= mean(old machine)

getwd()
setwd('E://Insofe//Week4//Day2//Lab//20171224_Batch37_CSE7315c_t-test-ChiSqtest_Ftest__Lab05')
machines <- read.csv("machines.csv", sep = ",", header = T)
mean(machines$New.machine)
mean(machines$Old.machine)
sdNew <- sd(machines$New.machine)
sdOld <- sd(machines$Old.machine)
#samples are two. We are doing two sample mean test.

#find th difference
diff <- mean(machines$New.machine) - mean(machines$Old.machine)
diff

#compute the pooledSd
pooledSd <- sqrt((sdNew^2*9) + (sdOld^2*9))/(10+10-2)

#compute the test statistic
diff/(pooledSd* sqrt(1/10+1/10))

qt(0.05,10+10-2, lower.tail = T)

# Method 2
t.test(machines$New.machine, machines$Old.machine,alternative = "less")

# p value is less p-alpha = 0.05, we reject H0

#########################

#Q3

#### Two sample t- test for paired Data 
reg = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28) 
prem = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
?t.test
# why used less, because alternative hupothesis is prem < reg
t.test(prem,reg,alternative = "less", paired = T, conf.level = 0.95)
or 
t.test(reg,prem,alternative = "greater", paired = T, conf.level = 0.95)

# p value is 0.99 > 0.05, accpet null hypothsis, premium is better than regular

#################################

#Q4
#expectation is given, and survey response is also given. We need to find if survey is 
# as per expectation. It is goodness of test.

# HO: expected values are equal to observed values
# H1 : E values not equals observed values

observedFrequencies<- c(21,109,62,15)
expectedProportions<- c(0.08,0.47,0.34,0.11) 

# To compare withthe observed frquencies, let us compute the the expected frequencies 
expectedFrequencies <-  expectedProportions * sum(observedFrequencies)


calvalue <- sum((observedFrequencies - expectedFrequencies )^2 / expectedFrequencies)
calvalue

# two ways to find
#method 1
# find critical region with P value
pchisq(calvalue,3, lower.tail = F)
# alpha is 0.05, and pchisq value is 0.10010 which is 10% and greater than alpha
# we will accept null hypothesis

#method2
qchisq(0.05,3,lower.tail = F)

#calvalue 6.24 < qcisq value 7.81 , so cannot reject null hypothesis

#method3
?chisq.test

chisq.test(observedFrequencies,p = expectedProportions)
# p value = 0.1001, alpha = 0.5, pvalue > alpha so accept null hypothesis

#############################################

#Q5
#Ask is gender vs game preferences
# does men and women have difference preference, 2 factors
# chisqaure independence test
genderPreference <- matrix(c(200,150,50,250,300,50),nrow=2,byrow = T)
genderPreference
chisq.test(genderPreference)



#degrees of freedom
#(m-1)*(n-1) = (2-1)*(3-1) = 2

# p value = 0.03%
#null hypothesis - no preference for game depends on gender
#alternative hypothesis -  preference for game depends on gender
# alpha = 0.05
# p < alpha, reject H0

###########################################

#Q6 - use Anova as 3 differnt types of casrs

#Manual Calculation:
x1 <- c(643, 655, 702)
x2 <- c(469, 427, 525)
x3 <- c(484, 456, 402)

x <- c(x1,x2,x3)
x

#computing totals
ssT <- sum((x - mean(x))^2)

#computing within sum of squares
withinSS1 <- sum((x1-mean(x1))^2)
withinSS2 <- sum((x2-mean(x2))^2)
withinSS3 <- sum((x3-mean(x3))^2)
withinSS <- withinSS1+withinSS2+withinSS3

#between Sum of squares
betweenSS <- ssT - withinSS

# F stat using the critical value method
nr <- betweenSS / (3-1) # 3 is number of rows
  dr <- withinSS/(3*2) # 3 rows and 3 columns but substract 1 for degrees of freedom
  # we dont know the total and if we go by that, each row is independent.
  # for each row count is 3, degree of freedom is 3-1 = 2
  # for each row is 2
  # for all rows is 2+2+2 = 6
  # n * (m-1)
  crticalF <- nr/dr
crticalF
# computing the tabulated value 
qf(0.05,2,6) #
dfbetweenSS <- 2
  dfwithinSS <- 6
  qf(0.05,dfbetweenSS,dfwithinSS,lower.tail = F)
  # above line gives 5.14 but critical value is 25
  # critical value > 5.14 reject null hypothesis

#Method2
  # creating the data frame 
  scores = c(643,655,702,469,427,525,484,456,402)
  group <- c(rep(c("compact","inter","fullsize"),c(3,3,3)))
  data <- data.frame(scores,group)
  aov(scores~group, data=data)
  summary(aov(scores~group, data=data))

  #probabilty 0.00121 < 0.05, we can reject the null hypothesis  
  
  plot(data$group, data$scores)
  #######################################
  
  genderPreference_hobbies <- matrix(c(100,120,60,350,200,90),nrow=2,byrow = T)
  genderPreference_hobbies
  chisq.test(genderPreference_hobbies)
  
  
  #Q8
  
  attach(mtcars)
carsData <- mtcars
str(carsData)
  #which test to use
  # chisq as am is categorical and gears is numeric
  q8 <- table(mtcars$am, mtcars$gear)
  q8
  chisq.test(q8) # assuming significance level is 0.05
  # H0 : Automatic are equal to manual
  #as p-value < 0.05 , reject null hypothesis
  
  #II.does mpg varies on am
  # create vectors ansd pass to t-test
  
  #III. mpg varies on am and gear - anova
  #IV. does mpg depends on am and cyl
  