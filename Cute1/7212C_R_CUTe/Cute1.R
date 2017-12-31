rm(list=ls(all=TRUE))

getwd()
setwd("E://Insofe//Cute1//7212C_R_CUTe")
getwd()

#A. Read the CSV file
DJI <- read.csv("weekly_adjusted_DJI.csv", header = T, sep = ",")
A <- read.csv("weekly_adjusted_DJI.csv", header = T, sep = ",")

#B.Convert the timestamp column to appropriate datetime format

B <- A
B$timestamp <- as.Date(B$timestamp)
str(B)

#C. Remove the column "dividend amount" from the dataframe
C <- subset(B,select=-c(dividend.amount))

#E. Read the file "to_merge.csv" as "anonymous" DataFrame and merge with DJI with appropriate key and suffixes

anonymous <- read.csv('to_merge.csv', header = T, sep = ",")
anonymous$timestamp <- as.Date(anonymous$timestamp)
str(anonymous)

?merge
E <- merge(B, anonymous, by = "timestamp", all = TRUE, suffixes = c("_DJI","_ANoN"))


#F.  What is the earliest year in our datasets?

F <- min(format(B$timestamp,"%Y"))

#G. On what date did we witness maximum traded "volume" for DJI?
G <- B[B$volume == max(B$volume),c('timestamp')]

#H.  What was the 'volume' on the day when DJI 'close' price was minimum?
H <- subset(B, B$close == min(B$close), select = c('volume'))

#I. Do the columns "close" and "adjusted close" have same value throughout for DJI?
I <- all(B$close == B$adjusted.close)

#J. Append a column "percentage_change" that reports change in price compared to previous days price

B[2:nrow(B),'close']
Jtemp <- cbind(B,c(NA,B[1:nrow(B)-1,'close']))
Jtemp  <- cbind(Jtemp, (Jtemp$close - Jtemp$`c(NA, B[1:nrow(B) - 1, "close"])`)/Jtemp$`c(NA, B[1:nrow(B) - 1, "close"])`)
colnames(Jtemp)[9] <- "temp"
colnames(Jtemp)[10] <- "percentage_change"
J <- subset(Jtemp, select = -c(temp))

#K. Extract year and month value from timestamp and append that as two columns "year" and "month"
format(B$timestamp,"%Y")
format(B$timestamp,"%m")

YearMonthTemp <- data.frame(cbind(format(B$timestamp,"%Y"),format(B$timestamp,"%m")))
class(YearMonthTemp)
colnames(YearMonthTemp)[1] <- "year"
colnames(YearMonthTemp)[2] <- "month"

K <- cbind(B,YearMonthTemp)

#L. Aggregate the data at "year" level and report the traded "volume" for each year
library(sqldf)
L <- data.frame(sqldf("select year,sum(volume) as volumePerYear from K group by year"))

#M. Using previous dataframe "L" state which year observed maximum volume?
M <- subset(L, L$volumePerYear == max(L$volumePerYear))


#N. Sort the Primary DataFrame in ascending order of timestamp
N <- sqldf("select * from B order by timestamp")


#S. In your Primary DataFrame append a new column "movement" that bins the "percentage_change" column into 3 bins

CustomBin <- function(x){
  if(x < -0.015){
    bin = "negative"
  }else if(x >= -0.015 & x<= 0.015) {
    bin ="neutral"
  }else {
    bin ="positive"
  }
  return (bin)
}

movement <- sapply(na.omit(J$percentage_change),CustomBin)
S <- cbind(na.omit(J),movement)

#Y.  Standardize [open,high,low,close,adjusted_close,volume,percentage_change] with the method of your choice

library(vegan)
?decostand
Y <- subset(J,select=c(open,high,low,close,adjusted.close,volume,percentage_change))

Y <- decostand(na.omit(Y),"standardize")

#Z. Feedback
  #Did you find the test useful?
  #  Yes test was useful.
   
#What did you like about the test?
# It covered all basic subsetting techniques and binding techniques

#What can be improved?
# This test looks good, but we need instruction in detail like omit NA's etc.