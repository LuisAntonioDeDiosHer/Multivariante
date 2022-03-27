
#Analisis de componentes principales
## passo 1 instalar el paquete datos
install.packages("datos")
##Llamar a la libreria
library(datos)
##elegir una matriz
datos::fiel
x<- data.frame(fiel)
names(x)

# 3.- Se definen n (numero de estados) y p (variables)
dim(x)

n<-dim(x)[1]
n
p<-dim(x)[2]
p

pairs(x,col="blue", pch=16, 
      main="Variables originales")

#5.- Obtención de los componentes principales
# con base en la matriz de covarianza muestral
mu<-colMeans(x)
mu
s<-cov(x)
s
#6.- Obtención de los componentes principales 
# con base a la matriz de covarianza muestral
es<-eigen(s)
es

# 7.- Matriz de auto-valores
eigen.val<-es$values

# 8.- Matriz de auto-vectores
eigen.vec<-es$vectors

# Proporción de variabilidad para cada vector
pro.var<-eigen.val/sum(eigen.val)

# Proporción de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)

#----------------------------------
# Obtencion de los componentes principales con
# base en la matriz de correlaciones muestrales
#---------------------------------------

R<-cor(x)
eR<-eigen(R)
eR

# Obtención de auto-valores
eigen.val<-eR$values

# Obtención de auto-vectores
eigen.vec<-eR$vectors

# Proporcion de variablidad
pro.var<-eigen.val/sum(eigen.val)

# Proporcion de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)

# Media de los auto-valores
mean(eigen.val)
#---------------------------
# Obtencion de los coeficientes (nuevas variables)
# 
#--------------------------

# 1.- Centrar los datos con respecto a la media
ones<-matrix(rep(1,n),nrow=n, ncol=1)

# 2.- Construccion de la matriz centrada
X.cen<-as.matrix(x)-ones%*%mu
X.cen
# 3.- Construccion de la matriz diagonal de las 
# varianzas
Dx<-diag(diag(s))
Dx

# 4.- Construccion de la matriz centrada multiplicada
# por Dx^1/2

Y<-X.cen%*%solve(Dx)^(1/2)
Y #datos normalizados

# 5.- Construccion de los coeficientes o scores
# eigen.vec matriz de autovectores
scores<-Y%*%eigen.vec

# Nombramos las columnas PC1...PC8
colnames(scores)<-c("PC1","PC2")

# visualizamos
scores

# Generacion del grafico de los scores
pairs(scores, main="scores", col="blue", pch=19)

3.-Generación del gráfico screeplot.
#--------------------------------------
#  PCA sintetizado
#-------------------------------------

View(x)
head(x)

# Aplicar el cálculo de la varianza a las columnas 
# 1=filas, 2=columnas
apply(x, 2, var)

# centrado por la media y escalada por 
# la desviacion standar (dividir entre sd).

acp<-prcomp(x, center=TRUE, scale=TRUE)
acp

# Generación del gráfico screeplot
plot(acp, type="l")

# Visualizar el resumen
summary(acp)

# Construcción del Biplot
biplot(acp, scale=0)