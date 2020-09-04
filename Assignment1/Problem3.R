#####################################################
# HOMEWORK 1 Problem 3
#####################################################

rm(list = ls())

# Importing  ElemStatLearn library and marketing data
library(ElemStatLearn)
data('marketing')

# exploring data and checking NA values in dataset
sum(is.na(marketing))
summary(marketing)

# replacing NA values with median value
for(i in 1:ncol(marketing)){
  marketing[is.na(marketing[,i]), i] <- median(marketing[,i], na.rm = TRUE)
}

sum(is.na(marketing))

# creating training and reference sample of marketing data
trainingSample = marketing
referenceSample = marketing

# training sample class 1
trainingSample$target = 1

for(i in 1:ncol(referenceSample)){
  referenceSample[,i] = sample(referenceSample[,i])
}

# reference sample class 0
referenceSample$target = 0

# combining both training sample and reference sample
dataset = rbind(referenceSample, trainingSample)

# importing rpart library
library(rpart)

dataset$target = as.factor(as.character(dataset$target))
classifier = rpart(target~., dataset)

summary(classifier)
predictedValue = predict(classifier, dataset[,-c(15)])
predictedValue

