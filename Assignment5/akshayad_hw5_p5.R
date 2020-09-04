####################################################
# Homework 5 Problem 5
####################################################

# importing data
data(state)
statedata <- data.frame(state.x77)
head(statedata)

# removing the states and keeping them in different variables
labeldata <- rownames(statedata)
labeldata
rownames(statedata) <- NULL
statedata

# exploring data and checking NA values
summary(statedata)
sum(is.na(statedata))

# pairs plot
pairs(statedata, main = "Bereau data")

# scaling data
scaledstate=scale(statedata) 
summary(scaledstate)

# a) Hierarchical Clustering

# cluster library
library('cluster')

set.seed(6)

# hierarchial clustering
hclustering <-hclust(dist(scaledstate), method ="complete")
plot(hclustering, main="Hierarchical Clustering with Scaled Features",hang=-1)

rect.hclust(hclustering, k = 3, border = 2:5)
scaledClusters <- cutree(hclustering , k=3)

# b) SOM 

# kohonen package
library(kohonen)
set.seed(6)

# som grid
somgrid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
som <- som(scaledstate, grid = somgrid, rlen = 700)

quartz()
plot(som, main = "Bereau Data")

codes <- somdata$codes[[1]]

# change plot
quartz()
plot(som, type = "changes", main = "Bereau Data")

# count plot
quartz()
plot(som, type = "count")

# mapping plot
quartz()
plot(som, type = "mapping")

# U martix plot distance neighbor plot
coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}

quartz()
plot(som, type = "dist.neighbours", palette.name = coolBlueHotRed)

for (i in 1:4){
    quartz()
    plot(som, type = "property", property=codes[,i], main = colnames(codes)[i])
}

d <- dist(codes)
hc <- hclust(d)

quartz()
plot(hc)
# cluster som
clustersom <- cutree(hc, k=3)

my_pal <- c("red", "blue", "yellow")
my_bhcol <- my_pal[clustersom]

graphics.off()

quartz()
plot(som, type = "mapping", col = "black", bgcol = my_bhcol,labels =rownames(statedata))
add.cluster.boundaries(som, clustersom)

#c) Graphical lasso model

# importing libraries
library(gRbase) 
library(gRim) 
library(gRain) 
library(glasso) 
library(graph) 

# pca 
pcadata <- prcomp(scale(statedata))
xlim1 <- min(pcadata $x[,1])-1
xlim2 <- max(pcadata $x[,1])+1
ylim1 <- min(pcadata $x[,2])-1
ylim2 <- max(pcadata $x[,2])+1

# plotting results
names(pcadata)
summary(pcadata)
plot(pcadata,main="PCA")

# score plot
library(ggfortify)
autoplot(pcadata)

# scree plot
pcadata $sdev
pr.var1= pcadata $sdev^2
pve1= pr.var1/sum(pr.var1)
par(mfrow=c(1,2))
plot(pve1 , type="o", xlab=" Principal Component ",  ylab="Proportion of Variance Explained ",  ylim=c(0,1), col="blue")
plot(cumsum(pve1), , type="o", xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), col="brown3")

# biplot
quartz()
biplot(pcadata, main ="PCA plot" ,choices = c(1,2), scale = 0, xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2))


# removing Alaska data 
newstatedata <- statedata[-c(2), ]
newstatedata

# partial correlation
S.body <- cov.wt(newstatedata, method = "ML")
PC.body <- cov2pcor(S.body$cov)
diag(PC.body) <- 0

quartz()
heatmap(PC.body)

# glasso package
ls("package:glasso")

# graphical lasso model
S <- S.body$cov
lassoresult <- glasso(S, rho = 10) 
names(lassoresult)
edges <- lassoresult$wi != 0 
diag(edges) <- 0 
glasso <- as(edges, "graphNEL") 
nodes(glasso) <- names(newstatedata)

quartz()
plot(glasso,main ="Graphical Lasso with Rho=10")

# trying different rho values to see the behaviour
graphics.off()
rhosvalue <- c(2,5,15,30,50,100)
lassoresult <- glassopath(S, rho = rhosvalue)
for (i in 1:length(rhosvalue)){
    edges <- lassoresult$wi[ , , i] != 0 
    diag(edges) <- 0 
    glasso <- as(edges, "graphNEL")
    nodes(glasso) <- names(newstatedata)
    quartz()
    plot(glasso, main = paste("Graphical Lasso with Rho", rhosvalue[i]))
 }

