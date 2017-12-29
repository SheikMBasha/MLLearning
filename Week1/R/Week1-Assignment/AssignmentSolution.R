#setting working directory

getwd()
setwd("E:\\Insofe\\R\\Week1-Assignment")
getwd()

#cleaning environment
rm(list = ls(all=TRUE))

#1. Read the Ipl_matches.csv file in to iplmatches.
iplmatches <- read.csv("Ipl_Matches.csv", header = T, sep = ",")

#2. Check the dimensions of iplmatches.
dim(iplmatches)

#output: 577 * 18

#3. Check the type (character, factor,integer) of two variables(columns).

class(iplmatches$season)
class(iplmatches$team1)

  # for all columns
  str(iplmatches)

#4. Display the first 20 records and last 15 records.
  
  #first 20
  head(iplmatches,20)

  #last 15
  tail(iplmatches,20)

#5. Display the seasons in which ipl was conducted.
  
  unique(iplmatches$season)

#6.  Display the seasons in which ipl was conducted.
  
  unique(iplmatches$season)
  
#7. Create a data frame with id,team1,team2, and winner by subsetting on ipldata.
  
  x <- subset(iplmatches,select = c(id, team1, team2,winner))
  class(x)

#8. Display the no of matches played in season
  
  IPLMatcesPerSeason <- function(x){
    return (length(iplmatches[iplmatches$season == x,1]))
  }

  #i. 2008
    #1st solution
    IPLMatcesPerSeason(2008)
    
    #2nd solution
    nrow(iplmatches[iplmatches$season==2008,])
    
  #ii. 2010
    #1st solution
    IPLMatcesPerSeason(2010)
   
  #iii. 2008
    #1st solution
    IPLMatcesPerSeason(2016)

#9. Get the team names which are participated in ipl , and store in 'teams' and display it.
    #Observe the output which you got
    
   teams <- iplmatches[, 'team2']
    teams   
    str(teams)   
#10. How many number of teams participated in ipl.

    length(unique(iplmatches[,"team1"]))

#11. Display the City names where ipl matches were played. And get the no of cities.
    unique(iplmatches[,"city"])
    length(unique(iplmatches[,"city"]))

#12. List out the Stadiums in Pune.
    
    unique(subset(iplmatches, city == 'Pune', select = venue))
  
#13. In Which City Eden gardens stadium is.
    unique(subset(iplmatches, venue=='Eden Gardens', select = city) )

#14.What is the percentage of games in which a team has won both toss and game.
    TossNGameWinnerCount <- nrow(subset(iplmatches, as.character(toss_winner) == as.character(winner), select= city))
    
    (TossNGameWinnerCount/nrow(iplmatches))*100
    
#15.    Find the matches in which D/L has applied. Create data frame and write to a csv file. 
    
    DLFrame <- subset(iplmatches, dl_applied == 1, select = season:umpire3)
    write.csv(DLFrame, file="DLFrame.csv", row.names = T)    
    
#16. Create data frame with the matches which are tie. And write to a csv file.
    TieFrame <- subset(iplmatches, result == 'tie')
    write.csv(TieFrame, file="TieFrame.csv")    


#17. Find the teams which has won highest e no of matches in ipl season 2008,2010 and 2016.
    HighestWinnerBasedOnSeason <- function(x){
      y <- subset(iplmatches, season == x, select = winner)
      y <- table(y)
      return(y[which.max(y)])
    }
    
    #i. 2008
    
    HighestWinnerBasedOnSeason(2008)
    
    #ii. 2010
    HighestWinnerBasedOnSeason(2010)
    
    #iii. 2016
    HighestWinnerBasedOnSeason(2016)
    
