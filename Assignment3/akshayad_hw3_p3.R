########################################################
# Homework 3 Problem 3
#########################################################

rm(list = ls())

# loading dataset
load('SwissBankNotes.rdata')
dataset = SwissBankNotes; 
rm(SwissBankNotes)

# checking NA values and exploring the data.
sum(is.na(dataset))
head(dataset)
dim(dataset)

# adding class 0 for genuine notes and class 1 for counterfeit notes in the dataset.
dataset$note[1:100] <- 0
dataset$note[101:200] <- 1
dataset

# checking mean and variance in the dataset to check if scaling is required.
apply(dataset , 2, mean)
apply(dataset , 2, var)
library(kohonen)
dataset <- scale(dataset)

# performing PCA on the genuine notes
data1 <- dataset[1:100,]
pc_ex1 <- prcomp(data1[,1:6])

# plotting results
names(pc_ex1)
summary(pc_ex1)
plot(pc_ex1)

# score plot
library(ggfortify)
autoplot(pc_ex1)

# scree plot
pc_ex1$sdev
pr.var1= pc_ex1$sdev^2
pve1= pr.var1/sum(pr.var1)
par(mfrow=c(1,2))
plot(pve1 , type="o", xlab=" Principal Component ",  ylab="Proportion of Variance Explained ",  ylim=c(0,1), col="blue")
plot(cumsum(pve1), , type="o", xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), col="brown3")

# biplot
quartz()
biplot(pc_ex1,scale =0)

# performing PCA on the counterfeit notes
data2 <- dataset[101:200,]
pc_ex2 <- princomp(data2[,1:6])

# plotting result of counterfeit data.
summary(pc_ex2)
plot(pc_ex2)

# score plot
autoplot(pc_ex2)
pc_ex2$sdev

# scree plot
pr.var2= pc_ex2$sdev^2
pve2= pr.var2/sum(pr.var2)
par(mfrow=c(1,2))
plot(pve2 , type="o", xlab=" Principal Component ",  ylab="Proportion of Variance Explained ",  ylim=c(0,1), col="blue")
plot(cumsum(pve2), , type="o", xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), col="brown3")

# biplot
quartz()
biplot(pc_ex2)

# performing PCA on the combined data.
data3 <- dataset
pc_ex3 <- princomp(data3[,1:6])

# plotting result
summary(pc_ex3)
plot(pc_ex3)

# scree plot
pc_ex3$sdev
pr.var3= pc_ex3$sdev^2
pve3= pr.var3/sum(pr.var3)
par(mfrow=c(1,2))
plot(pve3 , type="o", xlab=" Principal Component ",  ylab="Proportion of Variance Explained ",  ylim=c(0,1), col="blue")

plot(cumsum(pve3), , type="o", xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), col="brown3")

# score plot
autoplot(pc_ex3, data = dataset, colour = 'note', shape = FALSE, label.size = 2)
autoplot(pc_ex3, data = dataset, colour = 'note', shape = FALSE, label.size = 2,loadings = TRUE,label=TRUE,loadings.label = TRUE)

# biplot
quartz()
biplot(pc_ex3)


