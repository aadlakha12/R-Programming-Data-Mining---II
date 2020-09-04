###################################################
# Homework 4 Problem 2 
###################################################

# importing library
library(gRain)
library(Rgraphviz)
library(gRbase)
library(ggm)

# preparing the network
g <- list(~A, ~C|A, ~B, ~D|A:B, ~E|B, ~F|A:C:E,~G|D:E,~H|F:G)
dagnetwork <- dagList(g1)
plot(dagnetwork)

#a) checking d-separation between C and G
dSep(as(dagnetwork, "matrix"),"C", "G",cond = NULL)

#b) checking d-separation between C and E
dSep(as(dagnetwork, "matrix"),"C", "E",cond = NULL)

#c) checking d-separation between C and E given about G
dSep(as(dagnetwork, "matrix"),"C", "E",c("G"))

#d) checking d-separation between A and G given about D and E
dSep(as(dagnetwork, "matrix"),"A", "G",c("D","E"))

#e) checking d-separation between A and G given about D
dSep(as(dagnetwork, "matrix"),"A", "G",c("D"))