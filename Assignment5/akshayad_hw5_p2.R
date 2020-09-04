#############################################
# Homework 5 problem 2
############################################

# importing graph libraries
library(gRain)
library(gRbase)
library(ggm)
library(RBGL)

# building graph 
graph=list(~W,~X,~Y|W:X,~Z|Y)

# dag list from graph
graphdag= dagList(graph)

# plotting graph
quartz()
plot(graphdag)

# W and X independent given no condition
dSep(as(graphdag, "matrix"),"W","X",cond = NULL)

# W and Z independent given X
dSep(as(graphdag, "matrix"),"W","Z",c("X"))

# W and Z given Y
dSep(as(graphdag, "matrix"),"W","Z",c("Y"))

# W and Y given no condition
dSep(as(graphdag, "matrix"),"W","Y",cond = NULL)

# Y and X given no condition
dSep(as(graphdag, "matrix"),"Y","X",cond = NULL)

# W and X given Z
dSep(as(graphdag, "matrix"),"W","X",c("Z"))

# X and Z given W and Y
dSep(as(graphdag, "matrix"),"X","Z",c("W","Y"))
