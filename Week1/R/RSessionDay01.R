# Mathematical operations
2+2

library(rJava)

2+ 3^2
55/5
sqrt(25)
###########

#Vectors

age <- c(20,25,30,35,40)
length(age)
age
age2 <- c(20,25,30,35,40,"Twenty Five")
age2
class(age)
class(age2)

age3 <- c("Basha","sheik",TRUE)
age3
class(age3)

team <- c("Basha","Sheik",25,"Triveni Nagar", FALSE)
team
class(team)
len(team)
length(team)

# Matricses

#Student S1,S2,S3 taking 4 quizes Q1,Q2,Q3,Q4

marks <- c(12,13,14,15,14,13,12,13,14,15,14,13)
length(marks)
students <- matrix(marks, nrow = 3)
dim(students)

# Create a matrix with other order having quizes in rows and Students in rows, perform transpose
T(students) # error, function should be small case
t(students)

# to find number of rows and cols of a matrix
nrow(students)
ncol(students)

# change the order of rows and columns on the same matrix
# default is column wise

students <- matrix(marks, nrow= 4)
students

#flip the order
students <- matrix(marks, nrow=4, byrow = T)
students

# Rows are students s1,s2,s3,S4 and marks(Q1,Q2,Q3) are column values
# now add gender to columns (M = 1, F = 0)
# Matrix all elements should be of same data type
#Suppose qualification needs to be added of type character.

#This cant be done, so we need to go to DataFrames.
#DataFrames: Data can be of any dataType

#Interview Questions:
#1. Difference between matrix and dataframe.
#2. Difference between matrix and vectors
#a. Vectors cannot have multiple data types. If added it coerces (converts number to string - refer Rday01 session)
#b. Matrix is a multidimensional vector.

# dataframe - number of rows shuold be same for each column

#Create a DataFrame(above matrix with EducationQualification added)
students <- matrix(marks, nrow=4)
students
education <- c("Btech", "Msc", "Phd","BA")
res <- c(students, education)
res # now matrix is of type string, so convert it to dataframe

res <- data.frame(students,education)
res


#------------------------
#Lists are much powerful and robust
#List can combine vectors, matrix and dataframe.

# List doesnt need dimenstion like matrix, 1st row can have 4 elements, 2nd row can have 5 elements
# 3rd row can have 0 elements.
listResult <- list(marks, students, res) 
# collection of heterogeneous data structers vector, matrix and dataframe
listResult

#Refer 1st element of list
listResult[[1]]
listResult[[2]]
listResult[[3]]

listResult[1]
#output of single square brakcet and second square bracket is same

#listResult[1] is a pointer and gives data but we cannot perform any operation on it.
listResult[1] + 1 # it gives error, generally not used

#always used double brackets one



listResult[[1]] + 1 # it addes 1 to each element.

listResult[[1]][1] # Read 1st element from the vector which is 1st element of list.

marks[5] # index starts from 1


#----------------------------------------------------------
#Accessing elements from DataStructures

marks
marks[3]
#4th and 7th Element
marks[c(4,7)]

#Got more than 14
marks >= 14

#create vector with marks >= 14
marks[marks >= 14]


# A company has 5 employees, (Age, Gender, Salary). Create a data frames for 5 different employess
# Salarys are 25/M/ 10000$, 35/F/15000, 28/F/25000, 29/M/30000, 30/F/40000

#Q1 - how many emp  age > 26
#Q2 - how many female employess
#Q3 - how many are more than their average salary

empAge <- c(25,35,28,29,30)
empGender <- c("M","F","F","M","F")
empSal <- c(10000,15000,25000,30000,40000)

emp <- data.frame(empAge,empGender,empSal)
emp

#Q1
emp[[3]][empAge>25]
length(emp[[3]][empAge>25])



#Q2
emp[[2]][empGender = "F"]
length(emp[[2]][empGender == "F"])

#avg salary
avgSal <- mean(emp[[3]])
avgSal
#Q3

emp[[3]][empSal > avgSal]



#Q4
emp[[3]][(empSal > avgSal) & (empGender == "F")]




# In a 5*3 matrix
#D[1,1] - refers 1st row and 1st column
# D[1,] - refer elements of 1st rows
# D[,1] - refers elemtns of 1st column

# get second row
emp[2,]

# get second and fifth row
emp[c(2,5),]

# get second column
emp[,2]

# get 1st nd 3rd column
emp[,c(1,3)]
#solve above quiz with this

# or alternate solution
#Q1

emp$empAge > 26
emp[emp$empAge > 26,]

#Q2

#single or double quotes both works
emp$empGender == "F"
emp[emp$empGender == "F",]
nrow(emp[emp$empGender == 'F',])

#Q3
avgSal

mean(emp$empSal)

emp$empSal > mean(emp$empSal)

emp[emp$empSal > mean(emp$empSal),]

#Q4

#double variable filters
emp$empSal > mean(emp$empSal)

emp[emp$empSal > mean(emp$empSal)& emp$empGender == 'M',]
emp[emp$empSal > mean(emp$empSal)& emp$empGender == 'F',]
