#Q1
Distance - Numerical
PeakTime(surging) - Nominal
TravelType(Sharing or non sharing travel type) - Nominal
TimeOfTravel(day/night) - Nominal
Weather - - Nominal
UserType (Prime/Premium user or normal user) - Nominal
CabType (Sedan/Mini/Micro) - Nominal
JourneyTime - Numerical

Few more examples that you come across daily:
  
Blood pressure reading: Numerical
Number of stocks traded: Numeric
Education background: Nominal
Type of groceries purchased : Nominal
Price of petrol: Numeric
Rating a Restaurant: Ordinal
Buy a car or not: Nominal
Lifetime of a battery: Numeric


#Q2
A company has 10 employees and their average salary is 1 lpa. Now they have recruited a CEO who salary is normally very high. Which of the following measure of central tendencies will be impacted
	Median - Change
	Mode - No Change
	Mean - Change


getwd()
setwd('E:\\Insofe\\Week2')

getwd()

# 3. Groups Problem  
groupsData <- read.csv("Groups.csv",header = T, sep=",")

# i. mean of each group
mean(groupsData[,1])
mean(groupsData[,2])
mean(groupsData[,3])
mean(groupsData[,4])
mean(groupsData[,5])
mean(groupsData[,6])

apply(groupsData,2,mean)

x <- summary(groupsData)
x[4,]

#ii.  median of each group
median(groupsData[,1])
median(groupsData[,2])
median(groupsData[,3])
median(groupsData[,4])
median(groupsData[,5])
median(groupsData[,6])
apply(groupsData,2,median)
x[3,]

#iii. What is the average experience across all groups?
mean(apply(groupsData,2, mean))

groupsData
findMoreThan3Exp <- function(group){
  return (length(table(which(group>3))))
}
apply(groupsData,2,findMoreThan3Exp)
apply(groupsData,2,function(x) length(x[x>3]))

#4. Food delivery
foodDelivery <- read.csv('FoodDelivery.csv',header = T, sep = ",")

#i. 1.	Now that you know central measures help you understand data better 
#you go ahead with computing the central measures.
# (mean, median, mode, quartiles, range, inter-quartile range, standard deviation)

#mode
#foodDelivery$EagleBoys[which.max(table(foodDelivery$EagleBoys))]
myMode <- function(x){
    return (x[which.max(table(x))])
}
#mean
apply(foodDelivery, 2, mean)
apply(foodDelivery,2, median)
#apply(foodDelivery,2,)
apply(foodDelivery,2,quantile)
#range 
apply(foodDelivery,2,range)
apply(foodDelivery,2,IQR)
apply(foodDelivery,2,sd)

apply(foodDelivery,2,FUN=myMode)

#--
Observation:
  1. Mean and median are same for all.
  2. Range is very odd for PizzaHut and Swiggy 0-60 
  As standard deviation is less, EagleBoys data is closely distributed around mean.
    EagelBoys takes less time.

  3.	Do you still believe that all the service providers are prompt in their services? 
      No all service providers are not prompt in their services.


#5. Two people work in a factory making parts for cars. 
# The table shows how many complete parts they make in one week.

q4 <- read.csv('Q4.csv', header = T, sep = ",")
#a.
# other way of doing - apply(q4[,-1],1,mean)

apply(q4[,2:6],1,mean)
apply(q4[,2:6],1,median)
apply(q4[,2:6],1,range)
apply(q4[,2:6],1,sd)

#b.
Philip is more consistent. as SD is around mean and low SD.

#6.	Find the mode for 8,6,2,4,6,8,10,8 

tab1 <- as.table(8,6,2,4,6,8,10,8 )
tab1[which.max(tab1)]


#7.
WUQ <- c(11, 7.5, 8.5, 10, 10, 10.5, 5.5, 10, 9, 9.5, 5.25, 8, 6.5, 10.5, 8.75, 0, 6, 6, 6.75,
         8.75, 0, 9.5, 7.5, 8.5, 7)
range(WUQ)
sd(WUQ) # SD
sd(WUQ) ** 2 # variance

#quantiel
quantile(WUQ)

#8. large retailer
Q7 <- read.csv('Q7.csv',header=F, sep = ",")
apply(Q7[,2:6],1,range)
#mean
apply(Q7[,2:6],1,mean)
apply(Q7[,2:6],1,sd)
#1st supplier is better as sd is less.

#Q9
weather <- read.csv('city.csv', header=F, sep = ",")
weather
apply(weather[,2:13],1,mean)
apply(weather[,2:13],1,median)
apply(weather[,2:13],1,range)
apply(weather[,2:13],1,sd)
#City 2 and 5 have similar temp (because of similar  mean and median)

#anotehr way to check
boxplot(t(weather[,-1]))

#10.	What is the probability that we get a 5th Tuesday in a 30-day month?

Probability - month should start on mon and tuesday
so - P(E) - 2/7

#11. 9.	Let us suppose, you tossed two two-sided fair coins:
#a.	What are total # of events  in the sample space?
P(E1) - outcomes of coin 1
P(E2) - outcomes of coin 2
as both are independent events
total outcome = p(E1) * P(E2) = 2*2 = 4
#b.	Compute the PMF for heads in this experiment
#c.	Compute Expectation of heads


#12. 10.	Let three fair coins be tossed.
#Let Event A = {all heads or all tails}, Event B = {at least two heads}, 
#and Event C = {at most two tails}.
# Of the pairs of events, (A,B), (A,C), and (B,C), which are independent and which are dependent? 
#Are there any mutually exlcusive events? (Justify).

A = {HHH,TTT}
B = { HHT, HTH, THH, HHH}
C = { THH, HTH, HHT, TTH, THT, HTT, HHH}

P(A) = 2/8 = 1/4
P(B) = 4/8 = 1/2
P(c) = 7/8

A,B are independent  because
p( A intesect B) = P(A)*P(B)
1/8 = (1/4)*(1/2)
# 1/8 because HHH happens once in A and B

A,C are dependent events
p(A intersect C) != P(A)*P(C)
1/8 != (1/4)*(7/8)

B,C are dependent events
P(B interset C) != p(B)*p(C)
4/8 != (1/2)*(7/8)

none of the events are not mutually exclusive as
for any two given events A,B - P( A intesect B) != 0

