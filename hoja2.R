library(dplyr)
library(cluster)
library(NbClust) #Para determinar el no de clusters optimos
library(fpc)
library(mclust)
#Cargando data
movies <- read.csv("tmdb-movies.csv")

#Para obtener las graficas de codo 
data <- select(movies, popularity, budget, revenue, runtime, vote_count, release_year)

#transformando data
data = as.data.frame(unclass(data))
summary(data)

# Eliminando todos los NAs
myData = na.omit(data)

#escalar y centrar data
scaled_data = as.matrix(scale(myData))

#Elbow method sin escalar data
wss <- (nrow(myData)-1)*sum(apply(myData,2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(myData, centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")


#Elbow Method con data escalada
set.seed(123)
k.max <- 15
dati <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(dati, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Otros Metodos

#clusGap
clusGap(data, kmeans, 10, B = 100, verbose = interactive())

#Kmeans
kmm = kmeans(scaled_data,12,nstart = 50,iter.max = 15)
kmm

#Bayesian Inference Criterion for k means
d_clust <- Mclust(as.matrix(scaled_data), G=1:15, 
                  modelNames = mclust.options("emModelNames"))
d_clust$BIC
plot(d_clust)

#NbClust
#Elbow method version 2
nb <- NbClust(scaled_data, diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=5, method = "kmeans", 
              index = "all", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))
