############ Plotting the data ###########
# Load library to run aggregations using sql
library(sqldf)

# Read final csv file
final_data <- read.csv("Group13-Cute2-Final-Data.csv", header = T, sep = ",")

# Filter columns to build plots
plot_data <- subset(final_data, select = c(CONTACT_WID, Country, TotalTimeGamePlay, TotalRevenueGenerated))

# Creating first bar chart of distribution of customers against country
counts <- table(plot_data$Country)
barplot(counts, xlab="Country")

# Binning Users based on TotalTimeGamePlay
plot_data$UserType <- ifelse(plot_data$TotalTimeGamePlay > 10000, "Addictive", 
                  ifelse(plot_data$TotalTimeGamePlay > 3000, "Gamer", 
                         ifelse(plot_data$TotalTimeGamePlay > 1000, "Regular",
                                ifelse(plot_data$TotalTimeGamePlay > 100, "Low", 
                                       ifelse(plot_data$TotalTimeGamePlay > 10, "VeryLess", "NoPlay")))))
write.csv(plot_data, "Group13-Cute2-Plot-Data.csv", row.names = F)

# Creating box plot on TotalRevenueGenerated aaginst bins of Total Game Time
boxplot(TotalRevenueGenerated~UserType, data = plot_data, xlab="UserType",
                   ylab="TotalRevenueGenerated")

# Creating bar chart of distribution of customers based on their game type bin
counts <- table(plot_data$UserType)
barplot(counts, xlab="UserType")

# Get aggregation of total revenue based on user type on games
plot_data1 <- sqldf("select UserType, sum(TotalRevenueGenerated) as TotalUserTypeRevenue from plot_data group by UserType")

# Creating bar chart of distribution of customers based on their game type bin
barplot(plot_data1$TotalUserTypeRevenue, main = "Revenue based on User type", xlab = "UserType", ylab = "Revenue",
        names.arg = c("Addictive", "Gamer", "Low", "NoPlay", "Regular", "VeryLess"))