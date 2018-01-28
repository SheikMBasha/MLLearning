rm(list = ls(all = T))

getwd()
setwd('E://Insofe//Week8//Day1//Lab')

cerealsRaw <- read.csv('Cereals2.csv', header = T, sep = ",")

str(cerealsRaw)
summary(cerealsRaw)
cerealsRaw[is.na(cerealsRaw),]
sum(is.na(cerealsRaw))
library(DMwR)
cerealsImputed <- as.data.frame(knnImputation(cerealsRaw, k = 3))
rownames(cerealsImputed) <- cerealsImputed$name
cerealsImputed <- subset(cerealsImputed, select = -c(name))
sum(is.na(cerealsImputed))

#Hierrarchial Clustering
d <- dist(cerealsImputed, method = "euclidean")
fit <- hclust(d, method = "ward.D")
plot(fit)
fit$merge
fit$height

groups <- cutree(fit, k = 5)
groups

rect.hclust(fit, k = 5, border = "brown")
DataWithHistCluster <- data.frame(cerealsImputed, groups)

#K Means Clustering
? kmeans
cerealsNumeric <- cerealsImputed[, sapply(cerealsImputed, is.numeric)]
cerealsCategorical <- as.data.frame(cerealsImputed[,sapply(cerealsImputed, is.factor)])
library(dummies)
colnames(cerealsCategorical) <- c('manufactorer')
manufactorDummies <- dummy(cerealsCategorical$manufactorer)
cerealsNumericScale <- scale(cerealsNumeric)
library(caret)

cerealsScaled <- data.frame(cerealsNumericScale, manufactorDummies)

KMeanClusters <- kmeans(cerealsScaled, centers = 5, iter.max = 10)
KMeanClusters
KMeanClusters$withinss
KMeanClusters$betweenss

library(cluster)
clusplot(cerealsScaled, KMeanClusters$cluster, main = '2D representation of the Cluster solution',
         color = TRUE, shade = TRUE,
         labels = 2, lines = 0)

cerealsScaled <- data.frame(cerealsScaled,KMeanClusters$cluster)


wss <- 0
for (i in 1:15) {
    wss[i] <- sum(kmeans(cerealsScaled, centers = i)$withinss)
}

plot(1:15, wss,
     type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

testData <- cerealsScaled[sample(1:nrow(cerealsScaled), 1),]
closest.cluster <- function(x) {
    cluster.dist <- apply(KMeanClusters$centers, 1, function(y) sqrt(sum((x - y) ^ 2)))
    print(cluster.dist)
    return(which.min(cluster.dist)[1])
}

closest.cluster(testData)

set.seed(12)
index <- (sample(nrow(cerealsScaled), .90 * nrow(cerealsScaled)))
dataTest <- cerealsScaled[index,]
StabClus <- kmeans(dataTest, 5)
dataTest$clusters <- StabClus$cluster

ncol(cerealsScaled)
group1 <- cerealsScaled[index, 21]
group2 <- dataTest$clusters
group <- cbind(group1, group2)
write.csv(group, "clusgroup.csv")

#install.packages("fossil")
library(fossil)
stabilitycheck <- adj.rand.index(group1, group2)
stabilitycheck

#install.packages("clusteval")
library(clusteval)
Stabindex <- cluster_similarity(group1, group2, similarity = "jaccard", method = "independence")
Stabindex


## Kmediods implementation

library(cluster)
#Pam: partitioning around medoids
set.seed(12)
pamx <- pam(cerealsScaled, 5)
summary(pamx)
pamx$medoids
plot(pamx)

