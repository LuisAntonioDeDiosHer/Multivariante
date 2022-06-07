library(tidyr)
library(cluster)
library(tidyverse)
library(factoextra)
library(NbClust)

X<- USArrests

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Muertes"

X[,2]<-log(X[,2])
colnames(X)[3]<-"Log-Asaltos"


X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Urbanpop"

X[,4]<-log(X[,4])
colnames(X)[4]<-"Log-Violaciones"

#---------------------------------
#    Metodo k-means
#---------------------------------

#1.- Separacion de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X[2])

# 2.- Estandarizacion univariante.
X.s<-scale(X)
#calcular la matriz de distacias
m.distancia <- get_dist(X, method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

# 3.- Algoritmo k-medias (3 grupos)
# cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
Kmeans.4<-kmeans(X.s, 4, nstart=25)

# centroides
Kmeans.4$centers

# cluster de pertenencia
Kmeans.4$cluster

#estimar el número de clústers
#Elbow, silhouette o gap_stat  method
fviz_nbclust(X, kmeans, method = "wss")
fviz_nbclust(X, kmeans, method = "silhouette")
fviz_nbclust(X, kmeans, method = "gap_stat")

# 4.- SCDG
SCDG<-sum(Kmeans.4$withinss)
SCDG

# 5.- Clusters
cl.kmeans<-Kmeans.4$cluster
cl.kmeans

#6 .-calculamos los dos clústers
k2 <- kmeans(X, centers = 2, nstart = 25)
k2
str(k2)


#7.- Graficar los cluster
fviz_cluster(k2, data = X)
fviz_cluster(k2, data = X, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"



# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("blue", "red", "green")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-means", pch=20)


res2 <- hcut(X, k = 2, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","blue"))

res4 <- hcut(X, k = 4, stand = TRUE)
fviz_dend(res4, rect = TRUE, cex = 0.5,
          k_colors = c("red","blue","green","black"))

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------

library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="purple")
