#################################################
# Homework 4 Problem 1
#################################################

# importing libraries
library(igraph)
library(igraphdata)

#loading karate data
data(karate)
set.seed(7)

# finding 5% of total edges
percentkarate<- ceiling(0.05*gsize(karate))
g = karate
quartz()
plot(g)

# deleting edges from network
g1 <- delete_edges(g,sample(E(karate), percentkarate)) 
g1
gsize(g)
gsize(g1)
quartz()
plot(g1)

# keeping edges for reference after prediction.
deletededges <- g-g1
deletededges

# hierarchical random graphs
ghrg <- fit_hrg(g1)
quartz()
plot_dendrogram(ghrg)

# prediction with top10 edges
pred <- predict_edges(g1)
quartz()
plot(pred$prob)

# prediction edges
pred$edges
E(g1)$color <- "gray"
lay <- layout_nicely(g1)
g2 <- add_edges(g1, t(pred$edges[1:10, ]), color = "red")

# plotting predicted network
quartz()
plot(g2)

#b)
# loading kite data.
data(kite)
gsize(kite)

# 5% kite data
percentkite<- ceiling(0.05*gsize(kite))
ki = kite
quartz()
plot(ki)
ki1 <- delete_edges(ki,sample(E(kite), percentkite)) 
ki1
gsize(ki1)
quartz()
plot(ki1)

# deleted edges from kite
deletededgeskite <- ki-ki1
deletededgeskite

# hierarchical random graphs
ghrgki <- fit_hrg(ki1)
quartz()
plot_dendrogram(ghrgki)

# predicting kite edges
pred_kite <- predict_edges(ki1)

quartz()
plot(pred_kite$prob)

#plotting the predicted edges
E(ki1)$color <- "gray"
lay1 <- layout_nicely(ki1)
ki2 <- add_edges(ki1, t(pred_kite$edges[1:10, ]), color = "red")

# plotting predicted network
quartz()
plot(ki2)

#c)

# 15% deleting edges from network
percentkarate15<- ceiling(0.15*gsize(karate))

# kite network after deleting
g115 <- delete_edges(g,sample(E(karate), percentkarate15)) 
g115
gsize(g115)
quartz()
plot(g115)

# deleted edges from karate network
deletededges15 <- g-g115
deletededges15

# hierarchical random graphs
ghrg15 <- fit_hrg(g115)
print(ghrg15)
quartz()
plot_dendrogram(ghrg15)

# edge prediction
pred15 <- predict_edges(g115)
quartz()
plot(pred15$prob)

# plotting edges
E(g115)$color <- "gray"
lay15 <- layout_nicely(g115)
g215 <- add_edges(g115, t(pred15$edges[1:20, ]), color = "red")

# plotting predicted network
quartz()
plot(g215)

# 40% karate data
percentkarate40<- ceiling(0.40*gsize(karate))

# karate network after 40% deletion
g140 <- delete_edges(g,sample(E(karate), percentkarate40)) 
g140
gsize(g140)
quartz()
plot(g140)

# deleted edges 
deletededges40 <- g-g140
deletededges40
gsize(g)
gsize(g140)

# hierarchical random graphs
ghrg40 <- fit_hrg(g140)
quartz()
plot_dendrogram(ghrg40)

# edge prediction 
pred40 <- predict_edges(g140)
quartz()
plot(pred40$prob)
E(g140)$color <- "gray"
lay40 <- layout_nicely(g140)
g240 <- add_edges(g140, t(pred40$edges[1:40, ]), color = "red")

# plotting predicted network
quartz()
plot(g240)

# 15 % kite network deletion
percentkite15<- ceiling(0.15*gsize(kite))

ki115 <- delete_edges(ki,sample(E(kite), percentkite15)) 
ki115
gsize(ki115)
quartz()
plot(ki115)

# deleted edges
deletededgeskite15 <- ki-ki115
deletededgeskite15

# hierarchical random graphs
ghrgki15 <- fit_hrg(ki115)
quartz()
plot_dendrogram(ghrgki15)

# edge prediction
pred_kite15 <- predict_edges(ki115)
quartz()
plot(pred_kite15$prob)
E(ki115)$color <- "gray"
lay115 <- layout_nicely(ki1)
ki215 <- add_edges(ki115, t(pred_kite15$edges[1:10, ]), color = "red")

# plotting predicted network
quartz()
plot(ki215)

# 40 % kite network deletion
percentkite40<- ceiling(0.40*gsize(kite))

ki140 <- delete_edges(ki,sample(E(kite), percentkite40)) 
ki140
gsize(ki140)

quartz()
plot(ki115)

# deleted edges
deletededgeskite40 <- ki-ki140
deletededgeskite40

# hierarchical random graphs
ghrgki40 <- fit_hrg(ki140)
quartz()
plot_dendrogram(ghrgki40)

# edge predictiom
pred_kite40 <- predict_edges(ki140)
quartz()
plot(pred_kite40$prob)

E(ki140)$color <- "gray"
lay1401 <- layout_nicely(ki1)
ki240 <- add_edges(ki140, t(pred_kite40$edges[1:10, ]), color = "red")

# plotting predicted network
quartz()
plot(ki240)
