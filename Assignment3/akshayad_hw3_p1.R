########################################################
# Homework 3 Problem 1
#######################################################

rm(list = ls())

# importing library
library(kohonen)
library(ElemStatLearn) 

# loading data and exploring it
data(nci) 
sum(is.na(nci))
colnames(nci)
head(nci)
dim(nci)
unique(colnames(nci))

# fitting an SOM
nci.scaled <- scale(t(nci))
set.seed(64)
som_grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
nci.som <- som(nci.scaled, grid = som_grid, rlen = 500)

codes <- nci.som$codes[[1]]

#plotting results
quartz()
plot(nci.som, main = "NCI Data")

# training progress plot
quartz()
plot(nci.som, type = "changes", main = "NCI Data")

# count plot
quartz()
plot(nci.som, type = "count")

# mapping plot
quartz()
plot(nci.som, type = "mapping")

coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}

# distance neighnours plot
quartz()
plot(nci.som, type = "dist.neighbours", palette.name = coolBlueHotRed)


# component plane plots
for (i in 1:64){
    quartz()
    plot(nci.som, type = "property", property=codes[,i], main = colnames(codes)[i])
}

d <- dist(codes)
hc <- hclust(d)

# clustering
quartz()
plot(hc)

som_cluster <- cutree(hc, k = 3)

my_pal <- c("red", "blue", "yellow")
my_bhcol <- my_pal[som_cluster]

graphics.off()

quartz()
plot(nci.som, type = "mapping", col = "black", bgcol = my_bhcol, labels = colnames(nci),main='NCI data clusters - SOM')
add.cluster.boundaries(nci.som, som_cluster)