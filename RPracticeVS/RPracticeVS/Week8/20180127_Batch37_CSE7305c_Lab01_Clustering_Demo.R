rm(list=ls(all=TRUE))

#Consider mtacrs data of R-datasets
data(mtcars)
mydata <- data.frame(mtcars)
sum(is.na(mydata))
summary(mydata)
str(mydata)

mydata <- scale(mydata) # standardize variables 
# simple distance calculation between HOnda Civic and Camaro Z28
x <-mydata["Honda Civic",] 
y <- mydata["Camaro Z28",] 
dist(rbind(x, y)) 
# # distance between Camaroz28 and Firebird
# z <- mydata["Pontiac Firebird",] 
# dist(rbind(y, z))
# summary(mydata)

###-------------------------    Hierarchical Clustering     ------------------------###
# Ward's method 
# distance matrix euclidean
d <- dist(mydata, method = "euclidean")
?hclust
fit <- hclust(d, method="ward.D") #WARD is a min variance method to find compact clusters
plot(fit) # display dendogram
fit$merge
fit$height

groups <- cutree(fit, k=5) # cut tree into 5 clusters
groups

# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red") 
mydata_clusters=data.frame(mydata,groups)

fit1 <-hclust(d, method="complete")
fit2 <- hclust(d, method="single")
fit3 <- hclust(d, method='average')

############
# dev.off()
par(mfrow = c(2, 2))
plot(fit3, leaflab = "textlike", main = "Average", xlab = "")
plot(fit, leaflab = "textlike", main = "Ward", xlab = "")
plot(fit2, leaflab = "textlike", main = "Single", xlab = "")
plot(fit1, leaflab = "textlike", main = "Complete", xlab = "")
###-------------------------    K- means Clustering     ------------------------###

# K-Means Cluster Analysis with k = 5
# 5 cluster solution
# nstart = 20, it will randomly select centroids and run 20 times
#iter.max = 5 for each selected centroid, we will run 5 times and calculates variance.
fit <- kmeans(mydata,centers =  5, iter.max = 20)
# for each n if suppose we give max=5 and if it finds converging point at 3 it stops and goes to next n
#by default iter.max is 10
fit$withinss # includes different values stores
fit$betweenss
#study the mdoel
fit$cluster
fit$tot.withinss
fit
fit$centers
#or # get cluster means
#aggregate(mydata,by=list(fit$cluster),FUN=mean)

library(cluster)
clusplot(mydata, fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

# append cluster label to the actual data frame
mydata <- data.frame(mydata, 
                     fit$cluster)
write.csv(mydata,"kmeans_2.csv")
head(mydata)

# K-means:  Determine number of clusters by considering the withinness measure
wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
}

plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 



# For unseen data, we compute its distance from all the cluster centroids
# and assigns it to that cluster that is nearest to it

test_datapoint <- mtcars[sample(1:nrow(mtcars),1),]
closest.cluster <- function(x) {
        cluster.dist <- apply(fit$centers, 1, function(y) sqrt(sum((x-y)^2)))
        print(cluster.dist)
        return(which.min(cluster.dist)[1])
}

closest.cluster(test_datapoint)

## stability check
set.seed(12)
index <- (sample(nrow(mydata),.90*nrow(mydata)))
dataTest <- mydata[index,]
StabClus <- kmeans(dataTest,5)
dataTest$clusters <- StabClus$cluster

group1 <- mydata[index,12]
group2 <- dataTest$clusters
group <- cbind(group1, group2)
write.csv(group, "clusgroup.csv")

#install.packages("fossil")
library(fossil)
stabilitycheck <- adj.rand.index(group1, group2)
stabilitycheck

#install.packages("clusteval")
library(clusteval)
Stabindex <- cluster_similarity(group1, group2, similarity = "jaccard", method="independence")
Stabindex

## how to deal with mixture of attribute types

mydata2<-mtcars
names(mydata2)
#categorical data 
data1<-as.data.frame(apply(mydata2[,c(2,8,9,10)],2,as.factor))
#Numeric data - standardization
data2<-scale(mydata2[,-c(2,8,9,10)],scale=T,center = T)
data_allnum<-scale(mydata2,scale=T,center=T)

# combine
dat_gower_numcat<-cbind(data2,data1)
# distance using gower measure
distMat <- daisy(dat_gower_numcat, metric = "gower")
# hierarchical clustering
fitGower<-hclust(distMat,method="single")

plot(fitGower)

## naming the clusters - refer to the activity sheet.


## Kmediods implementation

library(cluster)
#Pam: partitioning around medoids
set.seed(12)
pamx <- pam(mydata, 5)
summary(pamx)
pamx$medoids
plot(pamx)





