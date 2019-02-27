library(dplyr)
library(cluster)
library(NbClust) #Para determinar el no de clusters optimos
library(fpc)
library(mclust)
library(e1071)#para cmeans
library(corrplot)
library(factoextra)
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

#Otro método más 
for (i in 1:2) {
  #
  # Estimar cluster optimo - conteo y perform de k means
  #
  gap <- clusGap(scaled_data, kmeans, K.max=10, B=500)
  k <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method="Tibs2001SEmax")
  fit <- kmeans(scaled_data, k)
  #
  # Graficar
  #
  pch <- ifelse(fit$cluster==1,24,16); col <- ifelse(fit$cluster==1,"Red", "Black")
  plot(scaled_data, asp=1, main=title, pch=pch, col=col)
  plot(gap, main=paste("Gap stats,", title))
  abline(v=k, lty=3, lwd=2, col="Blue")
  #
  # Siguiente paso 
  #
  scaled_data <- apply(scaled_data, 2, scale)
  title <- "Standardized data"
}

#ALGORITMOS DE AGRUPAMIENTO

#1.- KMeans
km<-kmeans(myData,2)
myData$grupo<-as.factor(km$cluster)

g1<- myData[myData$grupo==1,]
g2<- myData[myData$grupo==2,]

clusplot(myData,km$cluster, color=TRUE, shade=TRUE, xlab="Component 1", ylab = "Component 2" , lines=0, main = "K Means")


#Clustering jerarquico
hc<-hclust(dist(myData)) #Genera el clustering jerarquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=2) #Dibuja el corte de los grupos en el grÃ¡fico
groups<-cutree(hc,k=2) #corta el dendograma, determinando el grupo de cada fila
myData$gruposHC<-groups

g1HC<-myData[myData$gruposHC==1,]
g2HC<-myData[myData$gruposHC==2,]


#Fuzzy C-Means
fcm<-cmeans(myData,2)
myData$FCGrupos<-fcm$cluster
myData<-cbind(myData,fcm$membership)
# Membership coefficient
head(fcm$membership)
corrplot(fcm$membership, is.corr = FALSE, main = "Fuzzy C-Means (correlation)")
fcm$cluster

#SILUETAS
#Metodo de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(myData))
mean(silkm[,2]) #0.88
plot(silkm, col=1:8, border=NA)

#Metodo de la silueta para clustering jerarquico
silch<-silhouette(groups,dist(myData))
mean(silch[,2]) #0.86
plot(silch, col=1:8, border=NA)

#Metodo de la silueta para fuzzy cmeans
silfcm<-silhouette(fcm$cluster,dist(myData))
mean(silfcm[,2]) #0.88
plot(silfcm, col=1:8, border=NA)

#Medias
with(movies, mean(popularity))
with(movies, mean(budget))
with(movies, mean(revenue))
with(movies, mean(runtime))
with(movies, mean(vote_count))

#Medianas
with(movies, median(popularity))
with(movies, median(budget))
with(movies, median(revenue))
with(movies, median(runtime))
with(movies, median(vote_count))

#Modas

#Funcion para la moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modaP <- getmode(movies$popularity)
print(modaP)
modaB <- getmode(movies$budget)
print(modaB)
modaR <- getmode(movies$revenue)
print(modaR)
modaRU <- getmode(movies$runtime)
print(modaRU)
modaVC <- getmode(movies$vote_count)
print(modaVC)

#Tabla de frecuencia release_year
t = as.data.frame(table(movies$release_year))
