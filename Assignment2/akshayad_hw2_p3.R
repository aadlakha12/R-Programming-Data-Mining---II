##########################################
# Homework 2 Problem 3
##########################################

rm(list=ls())

# importing dataset
dataset <- read.table('seeds_dataset.txt',header = TRUE, sep='\t')

# seedgroup column of data
seedgroup <- dataset[,ncol(dataset)]

# removing seedgroup column from data
dataset$Seed.Group <- NULL

set.seed(29)
# importing libraries
library('cluster')
library("fpc")
library("fossil")

dataset1 <- scale(dataset)
summary(dataset1)
# Hierarchical clustering with complete linkage
hc.complete =hclust(dist(dataset1), method="complete")
plot(hc.complete ,main="Complete Linkage ",hang=-1)

# complete linkage rand index and adjusted rand index
table(cutree(hc.complete,3), seedgroup)
completerand <- rand.index(cutree(hc.complete,3), as.numeric(seedgroup))
completeadj <- adj.rand.index(cutree(hc.complete,3), as.numeric(seedgroup))

# Hierarchical clustering with single linkage
hc.single =hclust(dist(dataset1), method="single")
plot(hc.single ,main="Single Linkage ",hang=-1)

# single linkage rand index and adjusted rand index
table(cutree(hc.single,3), seedgroup)
singlerand <- rand.index(cutree(hc.single,3), as.numeric(seedgroup))
singleadj <- adj.rand.index(cutree(hc.single,3), as.numeric(seedgroup))

# Hierarchical clustering with average linkage
hc.average =hclust(dist(dataset1), method="average")
plot(hc.average ,main="Average Linkage ",hang=-1)

# average linkage rand index and adjusted rand index
table(cutree(hc.average,3), seedgroup)
averagerand <- rand.index(cutree(hc.average,3), as.numeric(seedgroup))
averageadj <- adj.rand.index(cutree(hc.average,3), as.numeric(seedgroup))

# rand index and adjusted rand index table.
table <- matrix(c(completerand,singlerand,averagerand,completeadj,singleadj, averageadj),ncol=3,byrow=TRUE)
colnames(table) <- c("Complete","Single","Average")
rownames(table) <- c("Rand Index","Adjusted Rand Index")
table <- as.table(table)
table

# within clusters sum of squared
wcss<- vector()
for (i in 1:10) wcss[i] <- sum(kmeans(dataset1,i)$withinss)

plot(1:10,wcss,type="b",main = paste('Clusters of clients'),xlab= 'Number of Clusters', ylab ='WCSS')

# kmeans with 3 clusters
kmeans <-kmeans(dataset1,3,nstart=10)

# rand index and adjusted rand index for k means
kmeansrand <- rand.index(kmeans$cluster, as.numeric(seedgroup))
kmeansadj <- adj.rand.index(kmeans$cluster, as.numeric(seedgroup))


# k-mediods
kmed<-pamk(dataset1)
#checking optimal value of k.
kmed$nc 
table(kmed$pamobject$clustering, seedgroup)

# trying for k=3
kmed3<-pamk(dataset1,3)
table(kmed3$pamobject$clustering, seedgroup)
layout(matrix(c(1,2), 1, 2))
plot(kmed3$pamobject)

# rand index and adjusted rand index for k-medoids
kmedoidrand <- rand.index(kmed3$pamobject$clustering, as.numeric(seedgroup))
kmedoidadj <- adj.rand.index(kmed3$pamobject$clustering, as.numeric(seedgroup))

# Comparison table for all the algorithms
table1 <- matrix(c(completerand,singlerand,averagerand,kmeansrand,kmedoidrand,
completeadj,singleadj,averageadj,kmeansadj,kmedoidadj),ncol=5,byrow=TRUE)
colnames(table1) <- c("Complete","Single","Average","K-Means", "K-medoids")
rownames(table1) <- c("Rand Index","Adjusted Rand Index")
table1 <- as.table(table1)
table1