rm(list = ls(all = TRUE))

setwd("...")
getwd()

read.csv(Filename, header, sep)
read.table (Filename, header, sep)
readfilefromsheet(workbookname, sheetname, header)

#or read from x <- loadworkbook(workbookname) then readWorksheet("x", sheetname, header)

write.csv(data, filename , rowname)

#library(XLConnect) and library(xlsx)

#head(), tail(), summary(), str(), typeof(), class(), as.characer(),

#order <- character, numerical, logical

#naming columns of dataframe
data.frame(A=c(1,2,3,4),B=c(5,6,7,8))

v <- LETTERS[1:4]

#for(i in 1:length(v))
#for(i in 1:nrow(v))

conditions:
  #can apply min, max, mean or custom function to rows/columns
  
  a <- apply(data, row(1)/column(2), inbuilt function/ FUN = custom function) #results in list
  data.frame(a)
  
# its a groupby function : column1 mean result based on column2 groups
  tapply(column1, column2, mean / inbuilt function / FUN = custom function)
  
  lapply(data[,2:11],mean)  # for list
  
  
  #subset can be done in many ways
  
  
  
  
  #1.subset(data, condition, columns to be displayed)
  subset(data,mpg>25,select=mpg:carb)
  
  data[data$mpg > 25,]
  

  data[mpg >25,]
  
  data[which(mpg > 25),]
  
  data[which(data$mpg > 25),]
  
  data2 <- data[mpg > 25 & hp > 75,]
  data2 <- subset(data,mpg > 25 | gear == 5,select = mpg:carb)
  
  #Merging dataframes
  mergeddata <- merge(Grade1,Grade2,by=c("StudentId"),all=TRUE)
  
