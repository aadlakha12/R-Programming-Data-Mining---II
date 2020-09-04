####################################################
# Homework 2 Problem 2
####################################################

rm(list=ls())

# loading data 
data <- read.csv('Ch10Ex11.csv',header=F)
head(data)
sum(is.na(data))

# importing cluster library
library('cluster')

# Distance
dist <- as.dist(1-cor((data))) 

# Hierarchical clustering with complete linkage
hc.complete <- hclust(dist, method ="complete")
plot(hc.complete, main="Complete Linkage with Correlation-Based Distance")

# Hierarchical clustering with single linkage
hc.single <- hclust(dist, method ="single")
plot(hc.single, main="Single Linkage with Correlation-Based Distance")

# Hierarchical clustering with average linkage
hc.average <- hclust(dist, method ="average")
plot(hc.average, main="Average Linkage with Correlation-Based Distance")

# part(c) pca 
pcacomponents <- prcomp(t(data))
head(pcacomponents$rotation)
cumsum <- apply(pcacomponents$rotation, 1, sum)
pcs <- order(abs(cumsum), decreasing = TRUE)

# 10 most different genes
pcs[1:10]

