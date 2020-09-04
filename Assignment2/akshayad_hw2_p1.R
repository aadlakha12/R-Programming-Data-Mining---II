#####################################################
# Homework 2 Problem 1
#####################################################

rm(list=ls())

# loading and exploring USArrests dataset
data <- USArrests
summary(USArrests)
sum(is.na(data))
head(data)
pairs(USArrests, main = "USArrests data")

# importing clustering library
library('cluster')

set.seed(6)

# Hierarchical clustering with complete linkage
hc.complete = hclust(dist(data), method="complete")

# plotting the clusters
plot(hc.complete ,main="Complete Linkage ",hang=-1)


# determing the cluster states
rect.hclust(hc.complete, k = 3, border = 2:4)

clusters <- cutree(hc.complete , h=150)

# scaling data
xsc=scale(data) 
summary(xsc)

# Hierarchical clustering with scaled features
hc.scale <-hclust(dist(xsc), method ="complete")
plot(hc.scale, main="Hierarchical Clustering with Scaled Features",hang=-1)

#determining cluster states of sclaed data
rect.hclust(hc.scale, k = 3, border = 2:4)
scaledClusters <- cutree(hc.scale , k=3)