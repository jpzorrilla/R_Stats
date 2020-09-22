data(iris)
dim(iris)
summary(iris)
table(iris[,5]) #cantidad de casos en la variable de respuesta categorica por clase
#Genero un data frame solo con las variables explicatorias X
X<-iris[,-5]
#muestro los primeros casos
head(X)
summary(X)
boxplot(X)
pairs(X)
install.packages("corrplot")
require(corrplot)
cor(X) #matriz de correlacion
corrplot(cor(X)) #viz la correlacion entre var
#Centro la matriz X (X-Xmedia)- se centra respecto a cada variable
Xc<-scale(X,scale=FALSE)
summary(Xc)
#Funciones para evaluar esfericidad
source("testacp.R") 
#Prueba de esfericidad
Bartlett.sphericity.test(Xc) 
#Indice Kaiser-Meyer-Olkin o medida de adecuacion muestral KMO 
kmo(Xc)
#Siempre evaluen que los test hacen lo que dicen 
#Con datos simulados no correlacionados el test deberia ser no significativo
XnotCorr<- matrix(runif(30, 0,100), nrow=10)
Bartlett.sphericity.test(XnotCorr)
#Comienzo el ACP en la matriz de varianza-covarianza
S<-var(Xc) #La matriz de varianza-covarianza no cambia por centrar la matriz

#Hallo los valores propios
Lambda<-eigen(S)$value

plot(Lambda/sum(Lambda), 
     type="b", ylab= "Varianza explicada",
     cex.lab=1.5, ylim=c(0,1),axes=FALSE, xlab="CP")
axis(1, at=1:4,labels=1:4);box();axis(2)
legend("topright", legend= "Varianza explicada CP1= 0.92", bty="n")
#Observe que la varianza se mantiene en las variables originales y la transformada. El ACP no modifica la estructura de los datos, es una simple rotacion.
#Demostracion, las dos funciones dan el mismo resultado
sum(diag(var(Xc)))
sum(Lambda)
#Matriz de vectores propios (U)
U<- eigen(S)$vectors # Matriz de autovectores
colnames(U)<- c('PC1','PC2','PC3','PC4')
rownames(U)<- colnames(X)
round(U,3)
#Calculo los pesos (valores de los individuos en los nuevos ejes, en ingles loadings)
Z<- Xc%*%U # matriz de pesos o loadings
head(round(Z,2))
head(Z[,1]) # Posicion de los individuos en el nuevo eje 1 (z1)
# grafico los individuos proyectados en el primer eje.
plot(Z[,1],rep(0, 150), xlab="Primer componente PC1", ylab="", axes=FALSE)
axis(1); box()
#Fraccion de Varianza explicada por el primer componente (PC1) y el segundo componente (PC2) cada uno.
eigen(S)$value[1]/sum(eigen(S)$value)
eigen(S)$value[2]/sum(eigen(S)$value)
#Notese que la fraccion de varianza total explicada por estos 2 ejes es de 0.97
plot(Z[,1],Z[,2], 
     xlab="Primer componente PC1, var=0.92", 
     ylab="Segundo componente PC2, var=0.05", 
     axes=TRUE, xlim=c(-5,5), ylim=c(-5,5))
abline(h=0,v=0,lty=2, col="grey")
#Proyeccion de las flores de iris en los dos primeros ejes rotados (PC1 y PC2)
#Calidad de representacion
#La contribucion de los individuos a la generacion de los ejes se mide utilizando el cos2_ind
#Si la relacion es cercana a 1, entonces los organismos estan bien representados, mientras que si es cercana a 0, no estan bien representados en los ejes y su interpretacion no es confiable.
# Norma en R^p Distancia al centro de las variables originales en su medida original
d2_m<-rowSums(Xc^2)
# Computa el cos2 para los individuos- da una idea de la calidad de representacion de los individuos
# Componentes al cuadrado (distancia de las variables proyectadas) sobre la distancia al origen de las variables originales
cos2_ind<-Z^2 / d2_m 
# los valores cercanos a cero quiere decir que estan mal representados en este componente. 
round(cos2_ind[,1],2) # eje 1
round(cos2_ind[,2],2) # eje 2
cos2_ind[,1]>mean(cos2_ind[,1]) # Un criterio es ver los individuos que estan mejor representados que la media
#jpeg(filename = "Componente1_CalRepresentacion.jpeg",width = 6, height = 6, units = "in", pointsize = 12,quality = 100,bg = "white", res = 200)
plot(Z[,1],1:nrow(Z),type="n", axes=F,ylab="",xlab=paste("Primer componente (z1), %var=",round(100*Lambda[1]/sum(Lambda),2)) ,ylim=c(-0.5,0.5))
points(Z[,1], rep(0,nrow(Z)),pch=19, cex=cos2_ind[,1]*2,col="blue")
abline(h=0)
axis(1)
box()
#calidad de la representacion en los dos primeros ejes (z1 y z2)
rowSums(cos2_ind[,1:2])
# organismos mal representados en los primeros dos ejes.
# los puedo identificar usando la funcion which
# Esos son los indices de los casos mal representados
selMalRep<-which(rowSums(cos2_ind[,1:2]) < 0.7)
selMalRep
#En general estan los casos bien representados, pero vimos que incluso utilizando los dos primeros ejes, hay 7 organismos cuya calidad de representacion es menor a 0.7
plot(Z[,1],Z[,2], xlab="Primer componente PC1, var=0.92", ylab="Segundo componente PC2, var=0.05", axes=TRUE, xlim=c(-5,5), ylim=c(-5,5))
abline(h=0,v=0,lty=2, col="grey")
# Marco los mal representados en rojo
points(Z[selMalRep,1],Z[selMalRep,2], col="red", pch=19)
#Calidad de representacion de variables
## Funcion auxiliaria- Tomada de  http://www.sthda.com/english/wiki/principal-component-analysis-in-r-prcomp-vs-princomp-r-software-and-data-mining
var_cor_func <- function(var.loadings, comp.sdev){
  var.loadings*comp.sdev
}

#loadings<- U  
sdev<- sqrt(Lambda)# Recuerden que Lambdas son los valores propios y es la varianza= (desvio)^2

## Matriz con la correlacion entre las variables originales (Altura, Lbd, etc.) y los nuevos ejes U (o componentes principales)
# Rcv= U*desvio estandar de los componentes
var.coord <- t(apply(U, 1, var_cor_func, sdev))
var.cos2<-var.coord^2

# Compute contributions
comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
var.contrib

#Como se observa, el largo del petalo tiene una contribucion importante al primer eje, mientras que el largo del sepalo, casi no contribuye al mismo, pero si contribuye al eje 2 (PC2).
#jpeg(filename = "Componente1_y_2_Orig_Variables.jpeg",width = 6, height = 6, units = "in", pointsize = 12,quality = 100,bg = "white", res = 200)
a <- seq(0, 2*pi, length = 100)
# aqui los ejes x e y son iguales y por lo tanto se ve que el primer componente captura mayor parte de la varianza
plot( cos(a), sin(a), type = 'l', col="gray",xlab = "PC1",  ylab = "PC2") 
abline(h = 0, v = 0, lty = 2,col="grey")
scale=0.2
points(Z[,1]*scale,Z[,2]*scale, pch=19,col=iris[,5])
text(Z[,1]*scale,Z[,2]*scale, rownames(X), pos=1, cex=0.6,col="red")
# Add active variables
arrows(0, 0, var.contrib[, 1], var.contrib[, 2], length = 0.1, angle = 15, code = 2)
# Add labels
text(var.contrib, labels=rownames(var.contrib), cex = 0.9, adj=1,pos=4)

#TODO ESTO ANTERIOR SE HACE EN UNAS POCAS LINEA DE CODIGO EN R

Res.pca<-princomp(Xc)
# el cutoff evita mostrar las que aportan menos que el cutoff seleccionado
loadings(Res.pca, cutoff=0.1)

U

summary(Res.pca)# Equivalente a  rbind(sqrt(Lambda),  Lambda/sum(Lambda))

plot(Res.pca)

# Deben ser cuidadosos con la escala de los ejes.
biplot(Res.pca)

Res.pcaCorr<-princomp(Xc, cor=TRUE)
plot(Res.pcaCorr)

biplot(Res.pcaCorr)

Xnew<-cbind(X, Xs=exp(X[,1]))
Xnewc<-scale(Xnew, scale=FALSE)

apply(Xnewc,2,var)

Res.pcaS<-princomp(Xnewc, cor=FALSE)
# La variable con fuerte sesgo se lleva todo el peso del 
biplot(Res.pcaS)

summary(Res.pcaS)

loadings(Res.pcaS)

Res.pcaCorrS<-princomp(Xnewc, cor=TRUE)
loadings(Res.pcaCorrS, cutoff=0.2)









