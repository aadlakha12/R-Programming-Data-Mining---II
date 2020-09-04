#################################################
# Homework 1 Problem 2
#################################################

#install.packages('arules')
rm(list = ls())

# Importing MASS Library and Boston dataset
library(MASS)
data('Boston')

# Exploring dataset and checking NA values
sum(is.na(Boston))
summary(Boston)
pairs(Boston)

# Importing ggplot library to plot histogram of features
library(ggplot2)
library(tidyr)
ggplot(gather(Boston, cols, value), aes(x = value)) + 
       geom_histogram(binwidth = 80) + facet_grid(.~cols)
summary(Boston$ptratioValue)
# Transforming data in binary form # low class labeled as 1, high as 0     
Boston$ageValue <- sapply(Boston$age, function(x) ifelse(x > 75, 0, 1))  
Boston$blackValue <- sapply(Boston$black, function(x) ifelse(x >= 396, 0, 1))     
Boston$crimValue <- sapply(Boston$crim, function(x) ifelse(x > 0.08, 0, 1))
Boston$disValue <- sapply(Boston$dis, function(x) ifelse(x >= 3.7, 0, 1))
Boston$indusValue <- sapply(Boston$indus, function(x) ifelse(x > 10, 0, 1))
Boston$medvValue <- sapply(Boston$medv, function(x) ifelse(x >= 30, 0, 1))
Boston$noxValue <- sapply(Boston$nox, function(x) ifelse(x >= 0.7, 0, 1))
Boston$ptratioValue <- sapply(Boston$ptratio, function(x) ifelse(x >= 19, 0, 1))
Boston$taxValue <- sapply(Boston$tax, function(x) ifelse(x >= 500, 0, 1))

binaryData <- Boston[,c(15:23)]
binaryData <- data.frame(lapply(binaryData, as.factor))
transactionData <- as(binaryData, "transactions")

# finding the frequency using itemFrequencyPlot
library(arules)
itemFrequencyPlot(transactionData, xlab = 'Labels', ylab = 'Item Frequency')

# Building a Apriori
rules <- apriori(data = transactionData, parameter = list(support = 0.05, confidence = 0.6))
summary(rules)
inspect(sort(rules, by = 'lift')[1:10])

#c) A student is interested is a low crime area, but wants to be as close to the city as possible (as measured by “dis”).  

crimeRules <- subset(rules, subset = rhs %in% "crimValue=1")
inspect(sort(crimeRules, by = "lift")[1:5])

disRules <- subset(rules, subset = rhs %in% "disValue=1")
inspect(sort(disRules, by = "lift")[1:5])

# d) A family is moving to the area, and has made schooling a priority. They want schools with low pupil-teacher ratios.
ptratioRules <- subset(rules, subset = rhs %in% "ptratioValue=1")
inspect(sort(ptratioRules, by = "lift")[1:5])

# e) Implementing a regression model.
lmdata <- data.frame(lapply(binaryData, function(x) as.numeric(as.character(x))))
linearmodel <- lm(ptratioValue ~., lmdata)
summary(linearmodel)



