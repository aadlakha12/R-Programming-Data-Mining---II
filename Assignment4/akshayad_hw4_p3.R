#######################################################
# Homework 4 Problem 3
#######################################################

rm(list = ls())

# loading packages
library(gRain)
library(Rgraphviz)
library(gRbase)
library(ggm)

# importing and exploring cad1 data
data(cad1)
names(cad1)

# preparing DAG network
g <- list(~Sex, ~Smoker|Sex, ~SuffHeartF, ~Inherit|Smoker, ~Hyperchol|SuffHeartF:Smoker, ~CAD|Inherit:Hyperchol)
dag <- dagList(g)
plot(dag)

# checking dseparation from built network
dSep(as(dag, "matrix"),"Sex", "SuffHeartF",cond = NULL)
dSep(as(dag, "matrix"),"Sex", "Hyperchol", c("Smoker", "SuffHeartF"))
dSep(as(dag, "matrix"), "Sex", "Inherit", c("Smoker"))
dSep(as(dag, "matrix"),"Inherit", "SuffHeartF",cond = NULL)
dSep(as(dag, "matrix"),"Smoker","CAD",c("Inherit","Hyperchol"))

# CPT
cad.cpt <- extractCPT(cad1, dag, smooth = 0.1)
grn1 <- grain(compileCPT(cad.cpt))
summary(grn1)

# compile
grn1c <- compile(grn1)
summary(grn1c)

# propagation
grn1c <- propagate(grn1c)
summary(grn1c)

grn1c.ev = setFinding(grn1c, nodes = c("Sex", "Hyperchol"), states = c("Female", "Yes"))

# probabilistic query, given evidence
querygrain(grn1c.ev, nodes = c("SuffHeartF", "CAD"), type = "marginal")

# probabilistic query, given NO evidence
querygrain(grn1c, nodes = c("SuffHeartF", "CAD"), type = "marginal")

# Calculate the probabilty of observing evidence
getFinding(grn1c.ev)

# probabilistic queries, given evidence, joint distribution
querygrain(grn1c.ev, nodes = c("SuffHeartF", "CAD"), type = "joint")

# probabilistic queries, no evidence, joint distribution
querygrain(grn1c, nodes = c("SuffHeartF", "CAD"), type = "joint")

# probabilistic queries, given evidence, conditional distribution
querygrain(grn1c.ev, nodes = c("SuffHeartF", "CAD"), type = "conditional")

# probabilistic queries, no evidence, conditional distribution
querygrain(grn1c, nodes = c("SuffHeartF", "CAD"), type = "conditional")

# simulated data 25 observations
simulateData = simulate.grain(grn1c.ev, nsim = 25)
simulateData
result25 <- predict(grn1c, response = c("Smoker","CAD"),newdata = simulateData, predictors = c("Sex","SuffHeartF","Hyperchol","Inherit"),type = "class")
result25
save(result25, file = 'data25.RData')

# 500 observations
simulateData500 = simulate.grain(grn1c.ev, nsim = 500)
result500 <- predict(grn1c, response = c("Smoker","CAD"),newdata = simulateData500, predictors = c("Sex","SuffHeartF","Hyperchol","Inherit"),type = "class")
save(result500, file = 'data500.RData')
