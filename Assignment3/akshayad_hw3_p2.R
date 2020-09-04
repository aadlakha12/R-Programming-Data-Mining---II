#####################################################
# Homework 3 Problem 2
####################################################

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

# scaling data
xsc=scale(data) 
summary(xsc)

# Hierarchical clustering with scaled features
hc.scale <-hclust(dist(xsc), method ="complete")
plot(hc.scale, main="Hierarchical Clustering with Scaled Features",hang=-1)

#determining cluster states of sclaed data
rect.hclust(hc.scale, k=3, border = 2:4)
scaledClusters <- cutree(hc.scale , k=3)

# importing kohonen package
library(kohonen)
# performing SOM 
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
somdata <- som(xsc, grid = som_grid, rlen = 500)

# plotting results
quartz()
plot(somdata, main = "USArrests Data")

codes <- somdata$codes[[1]]

# training progress plot
quartz()
plot(somdata, type = "changes", main = "USArrests Data")

# count plot
quartz()
plot(somdata, type = "count")

# mapping plot
quartz()
plot(somdata, type = "mapping")

# colored pallette
coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}

# U-matrix dist.neighbors plot
quartz()
plot(somdata, type = "dist.neighbours", palette.name = coolBlueHotRed)

# component plane plots
for (i in 1:4){
    quartz()
    plot(somdata, type = "property", property=codes[,i], main = colnames(codes)[i])
}

d <- dist(codes)
hc <- hclust(d)

# clustering
quartz()
plot(hc)

som_cluster <- cutree(hc, k=3)

# plot the SOM with the found clusters
my_pal <- c("red", "blue", "yellow")
my_bhcol <- my_pal[som_cluster]

graphics.off()

quartz()
plot(somdata, type = "mapping", col = "black", bgcol = my_bhcol)
add.cluster.boundaries(somdata, som_cluster)
