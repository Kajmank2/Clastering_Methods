# Hierarchiczna klasteryzacja
#DATA TO READ
adult <- read.csv(file = 'C:/Users/PC/Desktop/Porównanie Metod Klasteryzacji R/Projekt R/adult.csv')
adult <-na.omit(adult)
head(adult)
library(mlbench)
obj_org <- mlbench.spirals(1000,2,0.025)
my.data <-  4 * obj_org$x

#INFOO
?hclust
?dist
#IRIS
head(iris)
hc <- hclust(dist(iris[, -5])^2, "ward.D")
hc1 <- hclust(dist(iris[, -5])^2, "centr")
hc2 <- hclust(dist(iris[, -5])^2, "single")
hc3 <- hclust(dist(iris[, -5])^2, "complete")

opar <- par(mfrow = c(2, 2))
plot(hc,  labels = FALSE, hang = -1)
plot(hc1, labels = FALSE, hang = -1)
plot(hc2,  labels = FALSE, hang = -1)
plot(hc3, labels = FALSE, hang = -1)

#KLASTRY IN TABLE
h3=hclust(dist(iris[,-5]), "centr")
plot(h3,labels = FALSE, hang = -1)
table(iris$Species, cutree(h3, k =2))
h3=hclust(dist(iris[,-5]), "ward.D")
plot(h3,labels = FALSE, hang = -1)
table(iris$Species, cutree(h3, k =2))
h3=hclust(dist(iris[,-5]), "single")
plot(h3,labels = FALSE, hang = -1)
table(iris$Species, cutree(h3, k =2))
h3=hclust(dist(iris[,-5]), "complete")
plot(h3,labels = FALSE, hang = -1)
table(iris$Species, cutree(h3, k =2))

#ADULT CLEANUP
adult.data <- adult[,which(names(adult) != "workclass" )]
adult.data <- adult.data[,which(names(adult.data) != "education" )]
adult.data <- adult.data[,which(names(adult.data) != "workclass" )]
adult.data <- adult.data[,which(names(adult.data) != "sex" )]
adult.data <- adult.data[,which(names(adult.data) != "race" )]
adult.data <- adult.data[,which(names(adult.data) != "marital.status" )]
adult.data <- adult.data[,which(names(adult.data) != "occupation" )]
adult.data <- adult.data[,which(names(adult.data) != "relationship")]
adult.data <- adult.data[,which(names(adult.data) != "income" )]
adult.data <- adult.data[,which(names(adult.data) != "native.country" )]
adult.data

#ADULT
head(adult)
had <- hclust(dist(adult.data), "ward.D")
had1 <- hclust(dist(adult.data), "centr")
had2 <- hclust(dist(adult.data), "single")
had3 <- hclust(dist(adult.data), "complete")
oparAdults <- par(mfrow = c(2, 2))
plot(had,  labels = FALSE, hang = -1)
plot(had1, labels = FALSE, hang = -1)
plot(had2,  labels = FALSE, hang = -1)
plot(had3, labels = FALSE, hang = -1)


h3=hclust(dist(adult.data), "ward.D")
plot(h3,labels = FALSE, hang = -1)
table(adult$sex, cutree(h3, k =2))

h3=hclust(dist(adult$age), "centr")
plot(h3,labels = FALSE, hang = -1)
table(adult$sex, cutree(h3, k =2))

h3=hclust(dist(adult$age), "single")
plot(h3,labels = FALSE, hang = -1)
table(adult$sex, cutree(h3, k =2))

h3=hclust(dist(adult$age), "complete")
plot(h3,labels = FALSE, hang = -1)
table(adult$sex, cutree(h3, k =2))
#===========# MY DATA #============#

ho <- hclust(dist(my.data), "ward.D")
ho1 <- hclust(dist(my.data), "centr")
ho2 <- hclust(dist(my.data), "single")
ho3 <- hclust(dist(my.data), "complete")
                              oparAdults <- par(mfrow = c(2, 2))
                              plot(ho,  labels = FALSE, hang = -1)
                              plot(ho1, labels = FALSE, hang = -1)
                              plot(ho2,  labels = FALSE, hang = -1)
                              plot(ho3, labels = FALSE, hang = -1)

                              
                              
                              h3=hclust(dist(obj_org$x), "ward.D")
                              plot(h3,labels = FALSE, hang = -1)
                              plot(obj_org$x, col=cutree(h3, k =2))
                              table(obj_org$classes, cutree(h3, k =2))
                              h3=hclust(dist(obj_org$x), "centr")
                              plot(h3,labels = FALSE, hang = -1)
                              plot(obj_org$x, col=cutree(h3, k =2))
                              table(obj_org$classes, cutree(h3, k =2))
                              h3=hclust(dist(obj_org$x), "single")
                              plot(h3,labels = FALSE, hang = -1)
                              plot(obj_org$x, col=cutree(h3, k =2))
                              table(obj_org$classes, cutree(h3, k =2))
                              h3=hclust(dist(obj_org$x), "complete")
                              plot(h3,labels = FALSE, hang = -1)
                              plot(obj_org$x, col=cutree(h3, k =2))
                              table(obj_org$classes, cutree(h3, k =2))

          
library(clusterSim)
?cluster.Sim
#IRIS
sim_cl= cluster.Sim(iris[, c(-5)], 1, 2, 4, "G1", 
                    outputHtml="resultsIRIS")
table(sim_cl$optClustering, iris$Species)
#ADULT
sim_clAdult= cluster.Sim(adult.data, 7, 2, 4, "G1", 
                          outputHtml="resultsADULT")
table(sim_clAdult$optClustering, adult$sex)
#MyDAta
sim_my= cluster.Sim(my.data, 2, 2, 4, "G1", 
                        outputHtml="resultsMyDATA")
table(sim_my$optClustering, obj_org$classes)

#speccalt klasteryzacja spectralna
library(speccalt)
?speccalt
#IRIS
kern <- local.rbfdot(iris[, -5])
clustiris <- speccalt(kern, maxk = 8)
table(iris$Species, clustiris)
#Adult
kern2 <- local.rbfdot(adult.data)
clustadult <- speccalt(kern2, maxk = 8)
table(adult$sex, clustadult)
#MyData
kern3 <- local.rbfdot(my.data)
clustmydata <- speccalt(kern3, maxk = 8)
plot(x=my.data, col=clustmydata)
table(obj_org$classes, clustmydata)
############TESTOWE############
cl <- kmeans(my.data, centers = 2)
plot(x=my.data, col = cl$cluster)
table(obj_org$classes, cl$cluster)

library(dbscan)
?dbscan
?kNNdistplot
kNNdistplot(iris[, -5], k = 5)
abline(h=0.75, col = "red", lty=2)
#iris
res <- dbscan(iris[, -5], eps = 0.6, minPts = 3)
table(iris$Species,res$cluster)
plot(x=iris[, -5], col = res$cluster+1)
#adult
kNNdistplot(adult.data, k = 1)
abline(h=3000, col = "red", lty=4)
res <- dbscan(adult.data, eps = 3000, minPts = 3)
table(adult$sex,res$cluster)
plot(x=adult.data, col = res$cluster+1)

#myData
kNNdistplot(my.data, k = 3)
abline(h=0.35, col = "red", lty=2)
res <- dbscan(my.data, eps = 0.5, minPts = 3)
table(obj_org$classes,res$cluster)
plot(x=my.data, col = res$cluster+1)

