install.packages("factoextra")
install.packages("cluster")
install.packages("pvclust")

Jose=c(184,82,81,11,29,18)
Ana=c(163,53,52,8,23,15.5)
Maria=c(168,54.2,54.4,8,25,16.5)
Carlos=c(170,56,53,8,23,16.3)
Sol=c(173,76,72,10.7,26,16.6)
Andres=c(182,74,76,10,29,20)
Florencia=c(156,70,70.8,8.7,24.3,14.7)
Lucia=c(163,68,67.1,7.2,23,13.5)
Mario=c(165,73,72,8.5,23,15.2)
data=rbind(Jose,Ana,Maria,Carlos,Sol,Andres,Florencia,Lucia,Mario) #uno a los casos fila a fila: rbind viene de row bind.
colnames(data)=c("Alt","Lbd","Lbi","Ldm","Lp","Pm")
head(data,5) #Altura, Largo brazo der e izq, Largo dedo mayor, Largo Pie, Perimetro muneca

#estandaricemos la matriz de datos
data.std=scale(data,center=T,scale=T)
summary(data.std)

apply(data.std,2,sd) #me calcula el desvio (sd) por columna
#Al estandarizar las medias valen cero y los desvios estandar, uno.

#Distancia euclidea o L2 entre Ana y Jose y entre Andres y Florencia
sqrt(sum((data.std["Ana",]- data.std["Jose",])^2))
sqrt(sum((data.std["Andres",]- data.std["Florencia",])^2))

#Distancia Manhattan o L1 entre Ana y Jose
sum(abs(data.std["Ana",]- data.std["Jose",]))

#Distancia Minkowski o Lq entre Andres y Florencia (p=3)
(sum((data.std["Andres",]- data.std["Florencia",])^3)^(1/3))

#Matrices de distancia o disimilitud
#Matriz de distancia con distancia euclidea
EuMat=dist(data.std,"euclidean",diag=F) #Matriz con distancia euclidea
round(EuMat,2)

#Que par de individuos son los mas similares de acuerdo a sus caracteristicas biometricas?
which.min(EuMat) #el valor en la posicion 16 = Carlos y Maria

#Y los mas diferentes?
which.max(EuMat) #el valor en la posicion 1 = Ana y Jose

#Matriz de distancia con distancia de Minkowski
MinkMat=dist(data.std,"minkowski",diag=F,p=3) #Matriz con distancia de Minkowski
round(MinkMat,2)

?dist #distancias disponibles con la funcion dist

#grafico
data.std=as.data.frame(data.std)
plot(data.std$Alt,data.std$Lbd, xlab = "Alt", ylab = "Lbd",pch=8,col=1)

#K-medias
#Realicemos un cluster con dos grupos considerando los datos biometricos.
KM2=kmeans(data.std,2,nstart=20) #nstart refiere a las condiciones iniciales, se escoge un numero al azar.

#Grafiquemos los resultados considerando dos dimensiones.
plot(data.std$Alt,data.std$Lbd, col = KM2$cluster,pch = 19,cex=1.5,main="2 groups") #la eleccion de las variables Alt y Lbd para graficar es arbitraria. 
points(KM2$centers, col = 1:2, pch = 8, cex=2.5); 
text(data.std, rownames(data.std), cex=0.7, pos=1)

#A partir del cociente de la suma de cuadrados entre grupos y la suma de cuadrados total de los datos podemos obtener un estimador de pseudo R2
KM2$betweenss/KM2$totss

1-(sum(KM2$withinss)/KM2$totss) #que es lo mismo que 

#Veamos que otros objetos contiene en su interior nuestro objeto KM2 (es una lista que contiene varios objetos que podran ser de diferente naturaleza).
KM2[]

#3 grupos
KM3=kmeans(data.std,3,nstart=20)

#grafico
plot(data.std$Alt,data.std$Lbd, col = KM3$cluster,pch = 19,cex=1.5,main="3 groups"); 
points(KM3$centers, col = 1:3, pch = 8, cex=2.5); 
text(data.std, rownames(data.std), cex=0.7, pos=1)

#pseudo R2
KM3$betweenss/KM3$totss

# Cuantos grupos? Grafico el numero de clusters en funcion de la suma de cuadrados al interior de los grupos (medida que queremos minimizar). A esta medida tambien la llamamos W o SCD
fviz_nbclust(
  data.std, 
  kmeans, 
  k.max = 8,
  method = "wss",
)
# 3 o 4 clusters seria el optimo segun el grafico y met. codo


#PAM Es una version mas robusta de K-medias que se basa en la distancia a individuos representativos o medoids
pam.data<-pam(data.std, k=3, diss=FALSE,metric="euclidean") #diss=FALSE me permite entrar la matriz de datos, de no indicar nada (por defecto diss=TRUE), debo ingresar la matriz de distancias. Si ingreso la matriz de distancias no debo indicar la distancia o metrica.
plot(pam.data)

#Cluster jerarquicos aglomerativos. Utilizaremos la funcion hlcust.
d=dist(data.std,method="euclidean") #matriz de distancias de la matriz centrada y estandarizada

#Realizo los cluster con diferentes metodos
cj_complete=hclust(d,method="complete") 
cj_single=hclust(d,method="single") 
cj_average<-hclust(d,method="average")
cj_centroid<-hclust(d,method="centroid")

#los graficos
plot(cj_complete)
plot(cj_single)
plot(cj_average)
plot(cj_centroid)

#el numero de k grupos escogido lo determina el usuario

plot(cj_single)
rect.hclust(cj_single, k=4, border="red")

plot(cj_single)
rect.hclust(cj_single, k=2, border="green") 

#Asignacion a clases segun donde se desee cortar el dendograma (o dicho de otra forma, segun la cantidad de grupos con que nos queramos quedar)
cutree(cj_single,2)
cutree(cj_single,4)
cutree(cj_single,5)

# evaluacion p-value sig.
pval = pvclust(t(data.std), method.dist="euclidean", method.hclust="complete", nboot=100)
plot(pval)

?hclust


















