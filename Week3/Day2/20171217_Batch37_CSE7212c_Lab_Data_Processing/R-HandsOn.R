
rm(list = ls(all = TRUE))
getwd()
setwd('E:\\Insofe\\Week3\\Day2\\20171217_Batch37_CSE7212c_Lab_Data_Processing\\')
getwd()

#Objective: understand who r the potential customers

# if reading excel has a problem, then try reading csv file

library(XLConnect)
require(XLConnect)

#German_Credit2 <- loadWorkbook("German_Credit2.xls")
# or use readWorkSheetFromFile

part1 <- readWorksheetFromFile("German_Credit2.xls",sheet="Part1")

names(part1) # gives column name
#change case of column names
tolower(names(part1))
names(part1) <- tolower(names(part1))

part2 <- readWorksheetFromFile("German_Credit2.xls", sheet="Part2")

summary(part1)
# new_car, user-car and furniture are numerics, but they are categorical as per description
#set categorical data

CatData <- subset(part1, select = -c(duration, amount,install_rate,age,num_credits,num_dependents))
# make these data types as categorical - which is same as factors
CatData <- apply(CatData,2,factor)
summary(CatData)
str(CatData)

CatData1 <- data.frame(CatData)
summary(CatData1)

NumericData <- subset(part1, select = c(duration, amount,install_rate,age,num_credits,num_dependents))

# bind this data again back to original data
combinedData <- data.frame(NumericData, CatData1)


# replace missing values
# 3. approaches 
#( a. ignore)
#we ommitted all rows where even one NA is there
nomiss <- na.omit(combinedData)

# how to know missing value rows
missingRows <- combinedData[!complete.cases(combinedData),]


library(DMwR)
#if only 1 column has NA we can still have data
#we can see how many columns in a row
#suppose we are checking for 2 NA in a row with 15 columns

manyNAs(combinedData[1:15],0.2) # 2% rows and output is rowIds
combinedData[manyNAs(combinedData,0.4),]
# for first 15 rows
combinedData[manyNAs(combinedData[1:15,],0.4),]
# we get below output
# it means for 1st 15 rows we dont have any data which have more than 40% data with NA
#Warning message:
 # In manyNAs(combinedData[1:15, ], 0.4) :
#  Empty index generated, no rows with many NAs. Undesirable effects may be caused if indexing a data frame with this.

x <- combinedData[manyNAs(combinedData[1:100,],0.4),]
nrow(x)

#b. replace missing values

# i.) centralImputation function, if numerical value replace with median,
#if categorical value its with mode
centralImpite <- centralImputation(combinedData)

manyNAs(centralImpite) # gives 0 as values are replaced
# if we want to replace with specific value
  # first identify missing values and for those rows, replace with our own value

# ii. ) KNN imputation
knnmpute <- knnImputation(combinedData,3) # 3 is the number of nearest numbers
knnmpute

manyNAs(knnmpute)
summary(knnmpute)


# if dataset is large ex: 20 columns and 1 lakh records, dont use knnImpute,
#it will compute distance for each record, go for centralImputation

# if we want to specify columns to find neighbours for distance
knnImputation(combinedData[,c(1,3,4)],2) # it chooses neighbours based on 1,3,4th column


# standardization

# install a library vegan
library(vegan)

range <- decostand(NumericData,"range",) # gives error
range <- decostand(na.omit(NumericData), "range")
summary(range)

stde <- decostand(na.omit(NumericData), "standardize")
summary(stde)

############################################

#Binning
install.packages("infotheo")
library(infotheo)

# to convert numerical to categorical - binning

amntBin <- discretize(NumericData$amount, disc = "equalfreq")
amntBin
table(amntBin$X)

# if we want to decide number of bins
amntBin <- discretize(NumericData$amount,5,disc = "equalfreq")
table(amntBin)

#equalwidth

amntWidth <- discretize(NumericData$amount,5,disc="equalwidth")
table(amntWidth$X)

# values are not coming properly, that is because of junk data

amntWidth <- discretize(na.omit(NumericData$amount),5,disc="equalwidth")
table(amntWidth$X)

#Manual Binning

na.omit(NumericData$amount)
summary(na.omit(NumericData$amount))

# 250 - 2500 Bin1
# 2500 - 4000 Bin2
# Else Bin3

CreateCustomizeBin <- function(x){
  if(x  >= 250 & x <2500) {
    bin = "1"
  } else if(x >= 2500 & x < 4000){
    bin = "2"
  }
  else {
    bin = "3"
  }
  return(bin)
}

# sapply is used on a vector
sapply(na.omit(NumericData$amount), CreateCustomizeBin)
binAmnt <- sapply(na.omit(NumericData$amount), CreateCustomizeBin)
table(binAmnt)


#Categorical to numeric conversion

install.packages("dummies")
library(dummies)


edu <- dummy(CatData1$education)
edu
head(edu)
demo <- cbind(CatData1$education, edu)
head(demo)
