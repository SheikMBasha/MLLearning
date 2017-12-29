# Library to process Data
# install.packages(dplyr) - command to install
library(dplyr)

# Why need R for data preprocessig?
# 80% time is spent on cleaning or processing data.

#Command to clear global Environment

rm(list = ls(all = TRUE))

#Gets current working directory
getwd()

# difference between read.csv and read.table is - 
# table can read anything, csv is a subset of read.table

# we can use either library(XLConnect) or require(XLConnect)

#write.csv is used to write content into a excel file, if file does not exists, it creates it
# else overrides the existing file with a warning

#write.table can be used to write content to  a file with a differenet separator other than comma (,)
#write.xlsx to write to a excel file


#Built in and user-defined functions


#Useful functions for Data manipulations

#some predefined dataSets are available with R to play with data
# ex mtCars

#to use the dataSet use the command "attach"

attach(mtcars)
data <- mtcars
data

# to check data types of all columns in dataSEt use Str - means structure
str(data)

#gives descriptive statistics about every column in dataSEt

summary(data)

# if numeric value is not there for a certain row in a column for cylinder
# it will not give any data
#It always works with numeric data and coercion doesnt work

#Gets min of mpg from mtcars dataset
min(data$mpg)

# Suppose if we want to find min of all columns, we will have to write for loop and work
# we have alternates to forloop

#APPLY functions

a <- apply(data,2, min) # second argumnet is margin 1- means row ,2 means column
a

#now apply for each row, Gives min value for each row
a <- apply(data, 1, min)
class(a)
#GEt min value from each column except the first one
A <- data.frame(min = apply(data[,2:11], 2, min))
A

#Apply is faster than for loop, hence apply is preferred

stat <- function(x){
  "Min" = min(x)
  "Mean" = mean(x)
  "Max" = max(x)
  
  A <- data.frame(Min, Mean, Max)
  return (A)
}


stats <- apply(data[,2:11],2,FUN=stat) # output wil be a list
class(stats)

class(a)

is.vector(a) # its a vector
is.list(a) # not a list, its a list

# if we want it to be list, use lapply
#difference between apply and lapply is margin is not specified
# bedeafult margin is column for lapply

l <- lapply(data[,2:11], mean)
l
class(l)
# gives mean value of mpg based on cyl(group by cylinders)
# cylinder 4,6 and 8 (unique groups)
#for all 4 cylinders, take mpg value and give mean of it
#tapply groups by second parameters and performs third parameter as function
tapply <- tapply(mtcars$mpg,mtcars$cyl, mean)
tapply
class(tapply)
is.vector(tapply)
is.array(tapply)


#Subsetting data

data <- mtcars
data
data1 <- data[,2:11]
data1

data1 <- data[1:10,2:11]
#alternate of above statement
data1 <- data[1:10,-1]

data1 <- data[,c("mpg","cyl")]
data1

#Gives all columns specified from mpg to carb with mpg value >25
data1 <- subset(data, mpg>25, select=mpg:carb)
data1

#alternate is below as we are selecting all columns

data1 <- data[mpg>25,]
data1

#anotehr alternate

data1 <- data[data$mpg > 25,]
data1


#mutliple conditions

data2 <- data[mpg>25 & hp > 75,]
data2

#--------------------------------

#Joins

#head gives first 6 recors of dataframe
# tail gives last 6 records of dataframe

#as.character(columnname ) -> converts numeric to character
#as.numeric -> typeOf value of numeric gives double

#Merge dataframes based on common columns (Ex studentId), so the common data is merged and single column

#if no common column is there , values will be NA
# it's always outer join

