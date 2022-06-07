#_____ Dendrograma____
install.packages("cluster.datasets")
library(cluster.datasets)

data("all.mammals.milk.1956")
AMM=all.mammals.milk.1956

head(AMM)

# Cálculo de la matriz de distancia
# de Mahalonobis
dist.AMM<-dist(AMM[,2:6])

# Convertir los resultados del 
# cálculo de la distancia a una matriz de datos y
# me indique 3 digitos.

round(as.matrix(dist.AMM)[1:6, 1:6],3)

# Calculo del dendrograma
dend.AMM<-as.dendrogram(hclust(dist.AMM))

# Generacion del dendrograma
plot(dend.AMM)

# Agregar etiquetas al gráfico

AMM.nombres=AMM
rownames(AMM.nombres)= AMM.nombres$name
AMM.nombres=AMM.nombres[,-1]

# Construimos de nuevo el grafico
plot(as.dendrogram(hclust(dist(AMM.nombres))))

#------------------------------
#  Modificar el dendrograma
#-------------------------------

library(dendextend)

# Guardar las etiquetas en un objeto "L"
L=labels(dend.AMM)

labels(dend.AMM)=AMM$name[L]

# cambiar el tamaño de las etiquetas
dend.AMM %>%