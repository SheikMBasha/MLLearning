#Get working directory

getwd()


#Set working directory, I can read excel/csv files from here

setwd("E:\\Insofe\\Week1\\R\\Day2 Piazza\\20171203_Batch37_CSE7212c_R_lab01_Basics02\\")
getwd()


#Clear global environment

rm(list = ls(all= TRUE))

#Read Grade.csv from working directory

Grade <- read.csv("Grade1.csv",header=T, sep = ",")
Grade
dim(Grade)

#Read table command

read <- read.table("greek.txt", sep="\t", header=T)
read
dim(read)

#To Read excels need to install library xlconnect

library(XLConnect)

install.packages("XLConnect")
library(XLConnect)

require(XLConnect)
wb <- loadWorkbook("Book3.xlsx")
wb
sheet <- readWorksheet(wb,"Sheet1",header = T)
sheet

# Write to a file

write.csv(Grade,"data.csv",row.names = F)
write.csv(Grade,"data.csv",row.names = T)

library(xlsx)

install.packages("xlsx")
library(xlsx)

write.xlsx(sheet,"outputexcel.xlsx",row.names = F)

# Conditional statements
 x <- 30
 if(is.numeric(x)){
   print (" x is numeric ")
 }
 
 age <- 45
if(age > 45 ){
  print("Middle aged person")
} else {
  print("Not middle aged")
}
 
 x <- c("what", "is","truth")
 if("Truth" %in% x){
   print("Truth is found")
 }  else {
   print("Truth not found")
 }
 
 v <- LETTERS[1:4]
v 
for(i in v){
  print(i)
}

for(i in 1:length(v))
{
  print( v[i])
}

v <- c("Hello","while loop")
cnt <- 0
while(cnt < 4)
{
  print(v)
  cnt <- cnt + 1
}

v <- c("Hello","while loop")
cnt <- 0
while(cnt < 4)
{
  print(v[cnt])
  cnt <- cnt + 1
}

#--------------------

Grade$Class <- NA
for(i in 1:nrow(Grade)){
  if(Grade$OverallPct1[i] < 40){
    Grade$Class[i] <- "C"
  } else{
    if(Grade$OverallPct1[i] < 60){
      Grade$Class[i] <-"B" 
    } else {
      Grade$Class[i] <- "A"
    }
  }
}

Grade$Class1 <- ifelse(Grade$OverallPct1 < 40 , "C", ifelse(Grade$OverallPct1 <60, "B","A"))


Grade$class2 <- ifelse(Grade$OverallPct1<40 | Grade$Math1 < 60 , "C", ifelse(Grade$OverallPct1<60 | Grade$Math1<80,"B","A"))


#--------------

#Functions

v <- c(1,2,3,4,5)

sum(v)
mean(v)
sd(v)


square <- function(x){
  return(x^2)
}
square(4)

square(c(2,3,4))

y <- data.frame(c(1,2,3), c(4,5,6))
y


y <- data.frame(A=c(1,2,3), B = c(4,5,6))
y
square(y)

#------------------------

# mtcars is a predefined dataset, to use it use the attach function

attach(mtcars)
data <- mtcars

str(data)
summary(data)


# To check all the datasets available type datasets::
datasets::mtcars


min(data$mpg)
a <- apply(data, 2, min)
a

A <- apply(data[,2:11], 2, min)
A
class(A)
A <- data.frame(min = apply(data[,2:11],2,min))
A
class(A)

stat <- function(x){
  "Min"= min(x)
  "Max"= max(x)
  "Mean"= mean(x)
  A <- data.frame(Min,Max,Mean)
  return (A)
}

stats <- apply(data,2,FUN=stat)
stats

class(stats)
result <- do.call(rbind,stats)
result
class(result)

#--------------

1:10 # sequence of number

seq(1,20,0.2) # sequence with customized intervals
