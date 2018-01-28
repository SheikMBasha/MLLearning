### Association Rule mining in R - Demo 
getwd()
## ARules on Unsupervised data set

# Installing packages
#install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

#.	Read 'Transactions.csv' data into R such that the arules package treats the input csv file as "transaction" data.
trans = read.transactions(file="Transactions.csv", rm.duplicates = FALSE,
                          format = "single", sep = ",", cols = c(1, 2))
# if excel has mulitple items in single row, we go with format=basket
# row1 - item1,item2
# if excel has single item in single row, we go with format=single
#row1 - item1
#row2 - item2
# we are looking for unique items frequency so removing duplicates
#.	Explore and understand the data and items of transaction data
inspect(trans)
trans
# input i - 0 means choclate, 1 means coke all arranged alphabatic
image(trans)
trans@itemInfo

itemFrequency(trans)

#. Frequent itemsets
itemFrequencyPlot(trans)


#. Rule induction
rules <- apriori(trans, parameter = list(sup = 0.1, conf = 0.1, target = "rules"))
# 1st arg - transaction object
# 2nd arg - target = rules means it generates rules
rules

summary(rules)
inspect(rules) #
# inspect gives rules
#empty lhs means we brought items individually without any pattern
# lift if we brought milk, cookies and glass
# without milk and cookies what is the probability or support of glass alone - suppose 1
# with milk and cookies, probability increases to 1.2, then lift is 1.2
#so it means if customer buys milk and cookies then the probability of glass increases to 20% when compared to buying glass alone
library(arulesViz)
plot(rules)
plot(rules, method="graph", control=list(type="items"))

#. Subset Rules
top_rules = sort(rules, by = c("confidence", "support"))
head(as(top_rules, "data.frame"), n=5)

# check rules of  a prodcut when they are in lhs and when they are in RHS
rules.itemfilter1 <- as(subset(rules, subset = rhs %in%  "Choclates"), "data.frame")
rules.itemfilter1 
rules.itemfilter2 <- as(subset(rules, subset = lhs %in% "Pencil"),  "data.frame")
rules.itemfilter2 

rules_Lift <- as(subset(rules, subset = rhs %in% "Pencil" & lift > 1.01),"data.frame")
rules_Lift


## ARules on Supervised data set

rm(list=ls(all=T))
library(arules)
library(arulesViz)

# Read in the titanic survival data set and see if its a categorical dataset
titanic_data <- read.csv(file = "titanic_data.csv")
head(titanic_data,10)
str(titanic_data)

# Apply Apriori on the data to find associations amongst all the attributes
rules <- apriori(titanic_data)
inspect(rules)

rules <- apriori(titanic_data,parameter = list(minlen=2, supp=0.005, conf=0.7),
                 appearance = list(rhs = c("Survived=No", "Survived=Yes"), default = "lhs"))
#minlength means rule length
inspect(rules)
#[1] if passenger is female, she should have survived
# Sort the rules based on "lift"
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# Visualizing the pruned rules
library(arulesViz)
plot(rules.sorted, method="graph", control=list(type="items"))

