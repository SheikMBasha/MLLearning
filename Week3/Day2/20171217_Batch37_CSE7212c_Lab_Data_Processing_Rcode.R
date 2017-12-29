# R memory requirement 
# A data set with 1 million records and 100 attributes
# let us assume all variables are numeric (8 byes)
# how much RAM will it demand in GB's?
((10^6*100*8)/(2^20))/1000
rm(list=(ls(all=TRUE)))
#Setting working directory path
setwd("D:/Data-processing/to/")

#Loading required libraries
install.packages("XLConnect")

library(XLConnect)

# Reading raw data
part1 <-  readWorksheetFromFile("German_Credit2.xls",sheet = "Part1")
names(part1) <- tolower(names(part1))
str(part1)
part2 <- readWorksheetFromFile("German_Credit2.xls",sheet = "Part2")
names(part2) <- tolower(names(part2))

# merging two dataframes
combined1 <- merge(part1,part2,by="obs",all=TRUE)
# combined1 <- merge(part1,mstatus2,by="obs",all=TRUE)
names(combined1)
# reading the gender data and applying cast to re organize 
mstatus1 <- read.table('mstatus.csv',header=T,sep=",")
summary(mstatus1)
dim(mstatus1)

library(reshape2)
mstatus2<- dcast(mstatus1,OBS~mstatus)
length(mstatus2)
names(mstatus2) <- tolower(names(mstatus2))
mstatus <- data.frame(mstatus2)
head(mstatus)
# names(combined) <- tolower(names(combined))
# combined <- combined[,-1]
# Outer join again
names(combined1)
names(mstatus)
combined2 <- merge(combined1,mstatus,by="obs",all=TRUE)
names(combined2)
combined2 <- combined2[,-1]
summary(combined2)
str(combined2)
# Converting into factors
attach(combined2)
#detach(combined2)
combined2$chk_acct <- as.factor(chk_acct)
combined2$history <- as.factor(history)
combined2$new_car <- as.factor(new_car)
combined2$used_car <- as.factor(used_car)
combined2$furniture <- as.factor(furniture)
combined2$radio_tv <- as.factor(radio_tv)
combined2$education <- as.factor(education)
combined2$retraining <- as.factor(retraining)
combined2$sav_acct <- as.factor(sav_acct)
combined2$employment <- as.factor(employment)
combined2$male_div <- as.factor(male_div)
combined2$male_single <- as.factor(male_single)
combined2$male_mar_or_wid <- as.factor(male_mar_or_wid)
combined2$co_applicant <- as.factor(co_applicant)
combined2$guarantor <- as.factor(guarantor)
combined2$present_resident <- as.factor(present_resident)
combined2$real_estate <- as.factor(real_estate)
combined2$prop_unkn_none <- as.factor(prop_unkn_none)
combined2$other_install <- as.factor(other_install)
combined2$rent <- as.factor(rent)
combined2$own_res <- as.factor(own_res)
combined2$job <- as.factor(job)
combined2$telephone <- as.factor(telephone)
combined2$foreign <- as.factor(foreign)
combined2$response <- as.factor(response)
detach(combined2)

# Check whether all the cateorical variables are done properly
summary(combined2)

# taking only numeric data as a separate data frame
numeric.data <- subset(combined2,select=c(duration,amount,
                          install_rate,age,num_credits,num_dependents))                  
common.var <- names(combined2) %in% names(numeric.data)
cat.data <- combined2[!common.var]
cat.data <- data.frame(apply(cat.data,2,function(x)as.factor(x)))
names(cat.data)
str(cat.data)
str(mstatus)
combined2 <- cbind(cat.data,numeric.data)
summary(combined2)
##### Delaing with missing values ##### 
# to Get the count of rows with missing values
nrow(combined2[!complete.cases(combined2),])

#We can omit missing values.  using the following command.
#But, in general it is a bad practice if there are too many
#of them.
data.for.feature.selection = na.omit(combined2)
summary(data.for.feature.selection)

# Saving the R code till omiting the missing values
save.image("till_omitting_miss_values.Rdata")

#Let us identify rows where more than 10% (10) attributes are missing
#We can do it using apply function or for loop.  But, DMwR offers a 
#nice function
load("till_omitting_miss_values.Rdata")

library(DMwR)
nomissing = na.omit(combined2)
manyNAs(combined2, 0.1)
write.csv(combined2,"final.csv")
length(manyNAs(combined2, 0.1))

summary(combined2)
#Replacing with a central statistic.  Let us replace with median and mode.
combined4=centralImputation(combined2)
summary(combined4)
#Let us check the combined4 to check our work 
# The combined4 should not have NULL
sum(is.na(combined4))

correlation <- cor(numeric.data, use="complete.obs", method="kendall") 
symnum(correlation)
#Based on correlation, duration and amount are 
#highly correlated.  Let us use that factor.

#Build linear observations.  lm stands 
#for linear regression
attach(numeric.data)
summary(numeric.data)

model <- lm(amount~duration, data=numeric.data)
summary(model)$coefficients[1,1]
summary(model)$coefficients[1,2]
#A function to compute the missing value

fillamount <- function(x) {
  if (is.na(x)) return(NA)
  else return(206.4 + 146.7 * x)
}

#Apply function to execute it on all values of a row.
# is.na yields a vector of true and false.  
amount1 <- sapply(numeric.data[is.na(numeric.data$amount),"duration"], fillamount)
numeric.data[is.na(numeric.data$amount), "amount"] <- amount1
sum(is.na(amount1))
summary(numeric.data)
detach(numeric.data)


#KNNimputation
knn_impute1 <- knnImputation(combined2, 
                             k = 10, meth = "median")
knn_impute2 <- knnImputation(combined2, k = 10)
summary(knn_impute1)
str(knn_impute1)
sum(is.na(knn_impute1))
sum(is.na(knn_impute2))

#Multiple Imputation
# library(Amelia)
# attach(combined2)
# impute.amelia <- amelia(combined2, ords=names(cat.data))
# # plot(impute.amelia)
# # summary(impute.amelia)
# write.amelia(impute.amelia, separate = TRUE, 
#              file.stem="impute.amelia", format = "csv")
rm(duration)

# Data discretization for numeric variables using discretize
library(infotheo)
attach(knn_impute1)
summary(combined2$duration)
hist(combined2$duration)
duration <- discretize(knn_impute1$duration, disc="equalfreq", 
                       nbins=10) #Discretizing the variable 'age'
duration<- as.factor(duration$X)
summary(duration)
summary(amount)
duration.eqw <- discretize(knn_impute1$duration, disc="equalwidth", 
                       nbins=10) #Discretizing the variable 'age'
names(duration.eqw)
duration.eqw <- as.factor(duration.eqw$X)
summary(duration.eqw)

amount <- discretize(numeric.data$amount, disc="equalwidth", 
                     nbins=10) #Discretizing the variable 'age'
amount<- as.factor(amount$X)
summary(amount)

detach(knn_impute1)

# Manual binning
hist(numeric.data$amount)

AdultUCI<- data.frame(id = c(1:100),hours_wk=c(1:100))
summary(AdultUCI)
hist(AdultUCI$hours_wk)
bin_manual <- function(x)
{
  if(x>0 & x<=25) { 
    bin = "1"
  } else if(x>25 & x<=50) { 
    bin =  "2"
  } else if (x>50){ 
    bin =  "3"
  }
  return(bin)
}

res <- sapply(AdultUCI$hours_wk,bin_manual)
res <- as.factor(res)
table(res)

#Install & Load the package "dummies" to create dummy variables for categorical variables
library(dummies) 
attach(cat.data)
table(cat.data$chk_acct)

chk_acct <- dummy(cat.data$chk_acct)
head(chk_acct)
cat.dummy <- dummy.data.frame(cat.data)
summary(chk_acct)
history <- dummy(cat.data$history)
furniture <- dummy(cat.data$furniture)
sav_acct <- dummy(cat.data$sav_acct)
employment <- dummy(cat.data$employment)
present_resident <- dummy(cat.data$present_resident)

dummy.catdata <- cbind(chk_acct,history,new_car,used_car,furniture,radio_tv,education,retraining,
                       sav_acct,employment,male_div,male_single,male_mar_or_wid,co_applicant,
                       guarantor,present_resident,real_estate,prop_unkn_none,other_install,
                       rent,own_res,job,telephone,foreign,response)
input.std <- cbind(dummy.catdata,numeric.data)
head(input.std)

#Data standardization
library(vegan)
attach(numeric.data)
numeric.data <- na.omit(numeric.data)
std.data <- decostand(numeric.data,"range",na.rm=TRUE)
head(std.data)
std.data <- decostand(numeric.data,"standardize",na.rm=TRUE)
head(std.data)
# Data normalization
#using sapply and apply commands
norm.fn <- function(y){
sapply(y,function(x){(x-mean(y))/sd(y)})
}
norm.data <- apply(numeric.data,2, norm.fn)
head(norm.data)

#Let us divide the data into training (600, testing 
#and cross validation sets)
rm(part1,part2,mstatus1,mstatus2,combined1,combined2)
rows=seq(1,1000,1)
set.seed(123)
trainRows=sample(rows,600)
set.seed(123)
remainingRows=rows[-(trainRows)]
testRows=sample(remainingRows, 300)
evalRows=rows[-c(trainRows,testRows)]

train = data.for.feature.selection[trainRows,] 
test=data.for.feature.selection[testRows,] 
eval=data.for.feature.selection[evalRows,]

library(C50)
dtC50= C5.0(response ~ ., 
            data = train, 
            rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)

a=table(test$response, 
        predict(dtC50, 
                newdata=test, 
                type="class"))
accuracy <- sum(diag(a))/sum(a)*100;accuracy
recall=(a[2,2])/(a[2,1]+a[2,2])*100;recall
precision=(a[2,2])/(a[1,2]+a[2,2])*100;precision
F1 <- 2*precision*recall/(precision+recall);F1

# Classification and Regression Trees
library(rpart)
dtCart=rpart(response ~.,data=train, 
             method="class")    
plot(dtCart,main="Classification Tree for Response",margin=0.15,uniform=TRUE)
text(dtCart,use.n=T)
summary(dtCart)
plotcp(dtCart)
predCartTrain=predict(dtCart, 
                      newdata=train, 
                      type="vector")
predCartTest=predict(dtCart, 
                     newdata=test, 
                     type="vector")
predCartEval=predict(dtCart, 
                     newdata=eval, 
                     type="vector")

a=table(test$response, 
        predict(dtCart, 
                newdata=test, 
                type="class"))
accuracy <- sum(diag(a))/sum(a)*100;accuracy
recall=(a[2,2])/(a[2,1]+a[2,2])*100;recall
precision=(a[2,2])/(a[1,2]+a[2,2])*100;precision
F1 <- 2*precision*recall/(precision+recall);F1

cp=   dtCart$cptable[which.min(dtCart$cptable[,"xerror"]),"CP"]
dtCart.prune=rpart(response ~.,data=train, 
             method="class",cp=0.02513966)    
# plot the pruned tree 
plot(dtCart.prune,main="Pruned Classification Treecfor Response",margin=0.15,uniform=TRUE)
text(dtCart.prune,use.n=T,all=TRUE, cex=.8)

dtCart=rpart(inc ~.,data=train, 
             method="anova")    
plot(dtCart,main="Regression Tree for Income",margin=0.15,uniform=TRUE)
text(dtCart,use.n=T)
write.csv(train,"train.csv",row.names=FALSE)
predCartTrain=predict(dtCart, 
                      newdata=train, 
                      type="vector")
predCartTest=predict(dtCart, 
                     newdata=test, 
                     type="vector")
predCartEval=predict(dtCart, 
                     newdata=eval, 
                     type="vector")



rm(list = ls(all = T))
#setwd("D:/INSOFE/Consultancy/Motorist/final_r_code/rcode")
#####Calling all the libraries used in this program

#Function to check the installation and loading required package
FnLoadPkg<-function(pkg){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    require(pkg,character.only = TRUE)
  }else {
    require(pkg,character.only = TRUE)
  }
}

FnLoadPkg("reshape2")        ##Tweaking rows and columns in a dataframe
FnLoadPkg("data.table")        ##Plotting Graphs
AgencyData <- read.csv(file = "agency_final.csv",head=TRUE, sep = ",")
# filtering year 2013 only
AgencyData13to14 <- subset(AgencyData, AgencyData$STAT_PROFILE_DATE_YEAR %in% c(2013))
AgencyData13to14 <- subset(AgencyData13to14, select=c(AGENCY_ID,STAT_PROFILE_DATE_YEAR,GROWTH_RATE_3YR))
rm(AgencyData)

### Making 99999 into NA's
AgencyData13to14$GROWTH_RATE_3YR[AgencyData13to14$GROWTH_RATE_3YR == 99999] = NA
s <- AgencyData13to14[which(AgencyData13to14$AGENCY_ID==11),]
write.csv(s,"one-agent.csv")
##Aggregating Growthrate for each agency in that year.
x1 <- aggregate(GROWTH_RATE_3YR ~ AGENCY_ID + STAT_PROFILE_DATE_YEAR , AgencyData13to14, FUN = mean,na.action = NULL)
x2 <- dcast(x1, AGENCY_ID  ~  STAT_PROFILE_DATE_YEAR )
