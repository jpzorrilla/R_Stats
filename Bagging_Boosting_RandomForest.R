install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("adabag")

require(rpart) #arboles
require(rpart.plot)#gráficos árboles
require(randomForest)#radom forests y bagging
require(adabag)#boosting

setwd("C:/Users/usuario/Desktop/Archivos Escritorio/MAESTRIA EN POLÍTICAS PÚBLICAS/Cursos Maestría/04-01.EstadísticaAvanzada")

#levantamos los datos, recuerden ubicarse en el directorio de trabajo
data=read.csv("vivienda.csv",sep=";", dec=",", header=T)
summary(data)

#Dado que son categóricas, las variablas Precios_clases y Grandes_sup deben estar como factores.
data$Precios_clases=as.factor(data$Precios_clases)
data$Grandes_sup=as.factor(data$Grandes_sup)
summary(data$Precios_clases)
summary(data$Grandes_sup)

#Árboles de regresión
#ajustando un árbol de regresión con la función rpart().

names(data) #Utilizo todas las variables X como variables predictoras
set.seed(16)
arbol=rpart(Precios~Cont_1+Zona_ind+Grandes_sup+Cont_2+Nro_hab+Dist_GCiud+Imp+Hab_per,cp=0.001,data) #Iniciamos con un valor de cp de 0.001

#Lista de arboles anidados
a=printcp(arbol)

which.min(a[,4]) #aquí le pido que me indique que fila corresponde con el menor valor de la columna 4 (xerror) de la tabla de cp

#El árbol 23 es el que tiene menor xerror
#El siguiente plot es útil para visualizar como disminuye el xerror al aumentar la complejidad del árbol
plotcp(arbol)

#Veamos ahora cual cumple la regla 1-SE:

a[23,4]+a[23,5] #sumo el xerror y el sd de la fila 23 de la tabla de cp (que previamente llamamos a)

#El árbol más chico con un valor de xerror menor a 0.3640175 es el árbol 7
#Retengo ese árbol (regla 1-SE).
#Poda:
arbol.pod=prune(arbol,cp=0.011034945) #cp correspondiente al árbol 7
rpart.plot(arbol.pod)
#arbol 7 ver tabla resultado en #Lista de arboles anidados

#Calculemos el error del árbol mediante la metodología de muestra de prueba
set.seed(75)
K=30    
error.arbol.reg.learn=matrix(NA, K)
error.arbol.reg.test=matrix(NA, K)
var.arbol.reg.learn=matrix(NA, K)
var.arbol.reg.test=matrix(NA, K)

n = nrow(data)
for(k in 1:K)   {
  smp=sample(n,round(n/3))
  learn=data[-smp,]
  test=data[smp,]
  arbol.reg.learn=rpart(Precios~Cont_1+Zona_ind+Grandes_sup+Cont_2+Nro_hab+Dist_GCiud+Imp+Hab_per,cp=0.001,learn) #inicio con los mismos parámetros que escogimos arriba que generan una secuencia larga de árboles anidados y luego en cada iteración elijo aquel que cumpla la regla 1-SE
  
  a=printcp(arbol.reg.learn)
  b=which.min(a[,4])
  c=a[b,4]+a[b,5]
  d=min(which(a[,4]<=c))
  e=a[d,1]
  arbol.reg.learn.pod=prune(arbol.reg.learn,cp=e)
  pred.reg.learn=predict(arbol.reg.learn.pod,learn)
  pred.reg.test=predict(arbol.reg.learn.pod,test)
  
  error.arbol.reg.learn[k]=sqrt(mean((pred.reg.learn-learn[,1])^2)) 
  
  error.arbol.reg.test[k] = sqrt(mean((pred.reg.test-test[,1])^2)) 
  
  #Nos podría interesar calcular una pseudo varianza explicada a partir del MSE a modo de obtener resutlados más interpretables del desempeño del modelo.
  
  var.arbol.reg.learn[k]=1-((mean((pred.reg.learn-learn[,1])^2))/var(learn[,1]))
  var.arbol.reg.test[k]=1-((mean((pred.reg.test-test[,1])^2))/var(test[,1]))
  
}

#Resultados del loop:
mean.error.arbol.reg.learn=mean(error.arbol.reg.learn)
sd.error.arbol.reg.learn=sd(error.arbol.reg.learn)

mean.error.arbol.reg.test=mean(error.arbol.reg.test)
sd.error.arbol.reg.test=sd(error.arbol.reg.test)

mean.var.arbol.reg.learn=mean(var.arbol.reg.learn)
sd.var.arbol.reg.learn=sd(var.arbol.reg.learn)

mean.var.arbol.reg.test=mean(var.arbol.reg.test)
sd.var.arbol.reg.test=sd(var.arbol.reg.test)

mean.error.arbol.reg.learn
sd.error.arbol.reg.learn

mean.error.arbol.reg.test
sd.error.arbol.reg.test

mean.var.arbol.reg.learn
sd.var.arbol.reg.learn

mean.var.arbol.reg.test
sd.var.arbol.reg.test

#RANDOM FORESTS
#ntree = num. arboles = 500
set.seed(16)
modelo.rf.reg=randomForest(Precios~Cont_1+Zona_ind+Grandes_sup+Cont_2+Nro_hab+Dist_GCiud+Imp+Hab_per,ntree=500, data) 
modelo.rf.reg

modelo.rf.reg=randomForest(Precios~Cont_1+Zona_ind+Grandes_sup+Cont_2+Nro_hab+Dist_GCiud+Imp+Hab_per,mtry=7,ntree=500, data)
#lo hacemos con el parámetro mtry, observar que en este caso, al escoger 7, que corresponde con el número total de variables X de nuestra data.frame, estamos frente a un modelo Bagging ya que la gran diferencia entre RF y Bagging es el número de variables a escoger en cada nodo! 
#Ajustaremos este modelo enseguida....

plot(modelo.rf.reg)

#El error se encuetra estabilizado a partir de 150-200 árboles aproximadamente, así que 500 es un número más que adecuado.
#Importancia de variables.
varImpPlot(modelo.rf.reg)

#Este gráfico es muy útil para conocer la importacia de las variables (la medida de importancia cuantifica cuanto aporta en términos globales, cada variable en la disminución de impureza de los nodos hijos)
#Las variables que se encuentran más arriba, son más importantes, y las de más abajo, son las menos importantes
#Aquí vemos que hay una variable que es más importante que todas (Nro_hab), luego de la cual, hay un gran salto en relación a la importancia
#Observar que la variable más improtante coincide con la variable que aparece en la primer partición del árbol de regresión.

#Error por muestra de prueba del modelo Random Forests:
set.seed(75)
K=30    
error.rf.reg.learn=matrix(NA, K)
error.rf.reg.test=matrix(NA, K)
var.rf.reg.learn=matrix(NA, K)
var.rf.reg.test=matrix(NA, K)

n = nrow(data)
for(k in 1:K)   {
  smp=sample(n,round(n/3))
  learn=data[-smp,]
  test=data[smp,]
  modelo.rf.reg.learn=randomForest(Precios~Cont_1+Zona_ind+Grandes_sup+Cont_2+Nro_hab+Dist_GCiud+Imp+Hab_per,ntree=500, learn)
  
  pred.rf.reg.learn=predict(modelo.rf.reg.learn,learn)
  pred.rf.reg.test=predict(modelo.rf.reg.learn,test)
  
  error.rf.reg.learn[k]=sqrt(mean((pred.rf.reg.learn-learn[,1])^2)) 
  
  error.rf.reg.test[k] = sqrt(mean((pred.rf.reg.test-test[,1])^2)) 
  
  #pseudo varianza explicada
  
  var.rf.reg.learn[k]=1-((mean((pred.rf.reg.learn-learn[,1])^2))/var(learn[,1]))
  var.rf.reg.test[k]=1-((mean((pred.rf.reg.test-test[,1])^2))/var(test[,1]))
  
}

#Resultados del loop
mean.error.rf.reg.learn=mean(error.rf.reg.learn)
sd.error.rf.reg.learn=sd(error.rf.reg.learn)

mean.error.rf.reg.test=mean(error.rf.reg.test)
sd.error.rf.reg.test=sd(error.rf.reg.test)

mean.var.rf.reg.learn=mean(var.rf.reg.learn)
sd.var.rf.reg.learn=sd(var.rf.reg.learn)

mean.var.rf.reg.test=mean(var.rf.reg.test)
sd.var.rf.reg.test=sd(var.rf.reg.test)

mean.error.rf.reg.learn
sd.error.rf.reg.learn

mean.error.rf.reg.test
sd.error.rf.reg.test

mean.var.rf.reg.learn
sd.var.rf.reg.learn

mean.var.rf.reg.test
sd.var.rf.reg.test

#BAGGING

#Ahora ajsutemos un modelo Bagging pero utilizando la función randomForest(). Si bien hay funciones específicas para ajustar modelos Bagging (ej: ipred), es recomendado usar la función randomForest por ser más flexible.
modelo.bag.reg=randomForest(Precios~Cont_1+Zona_ind+Grandes_sup+Cont_2+Nro_hab+Dist_GCiud+Imp+Hab_per,mtry=7,ntree=500, data)
#como decíamos más arriba, al escoger mtry=7 (número de variables X) estamos frente a un modelo Bagging.
modelo.bag.reg

#Error por muestra de prueba del modelo bagging
set.seed(75)
K=30    
error.bag.reg.learn=matrix(NA, K)
error.bag.reg.test=matrix(NA, K)
var.bag.reg.learn=matrix(NA, K)
var.bag.reg.test=matrix(NA, K)

n = nrow(data)
for(k in 1:K)   {
  smp=sample(n,round(n/3))
  learn=data[-smp,]
  test=data[smp,]
  modelo.bag.reg.learn=randomForest(Precios~Cont_1+Zona_ind+Grandes_sup+Cont_2+Nro_hab+Dist_GCiud+Imp+Hab_per,ntree=500,mtry=7,learn)
  
  pred.bag.reg.learn=predict(modelo.bag.reg.learn,learn)
  pred.bag.reg.test=predict(modelo.bag.reg.learn,test)
  
  error.bag.reg.learn[k]=sqrt(mean((pred.bag.reg.learn-learn[,1])^2)) 
  
  error.bag.reg.test[k] = sqrt(mean((pred.bag.reg.test-test[,1])^2)) 
  
  #pseudo varianza explicada
  
  var.bag.reg.learn[k]=1-((mean((pred.bag.reg.learn-learn[,1])^2))/var(learn[,1]))
  var.bag.reg.test[k]=1-((mean((pred.bag.reg.test-test[,1])^2))/var(test[,1]))
  
}

#Resultados del loop:
mean.error.bag.reg.learn=mean(error.bag.reg.learn)
sd.error.bag.reg.learn=sd(error.bag.reg.learn)

mean.error.bag.reg.test=mean(error.bag.reg.test)
sd.error.bag.reg.test=sd(error.bag.reg.test)

mean.var.bag.reg.learn=mean(var.bag.reg.learn)
sd.var.bag.reg.learn=sd(var.bag.reg.learn)

mean.var.bag.reg.test=mean(var.bag.reg.test)
sd.var.bag.reg.test=sd(var.bag.reg.test)

mean.error.bag.reg.learn
sd.error.bag.reg.learn

mean.error.bag.reg.test
sd.error.bag.reg.test

mean.var.bag.reg.learn
sd.var.bag.reg.learn

mean.var.bag.reg.test
sd.var.bag.reg.test

#Comparación de errores en una tabla: media y desvío estándar de la raíz cuadrada del error cuadrático medio sobre la muestra test

MeanTest<-c(mean.error.arbol.reg.test,mean.error.rf.reg.test,mean.error.bag.reg.test)
SdTest<-c(sd.error.arbol.reg.test,sd.error.rf.reg.test,sd.error.bag.reg.test)
Results<-rbind(MeanTest, SdTest)
colnames(Results)<-c("CART", "RF", "Bagging")
round(Results,3)

##           CART    RF Bagging
## MeanTest 5.541 4.216   4.308
## SdTest   0.457 0.578   0.710

#En general los modelos funcionan bien, el que presenta menor error es el modelo Random Forests (aunque tiene un comportamiento muy similar al modelo Bagging)
#Como es esperable, el árbol de regresión es el que funciona peor, aunque los resultados no son malos
#De todas formas, a fin de escoger un modelo predictivo, seleccionaríamos el modelo Random Forests.

#MODELOS DE CLASIFICACIÓN

#ARBOL DE CLASIFICACION
#var. de respuesta = Precios_clases
set.seed(61)
K=30    
error.arbol.cl.learn=matrix(NA, K)
error.arbol.cl.test=matrix(NA, K)

n = nrow(data)
for(k in 1:K)   {
  smp=sample(n,round(n/3))
  learn=data[-smp,]
  test=data[smp,]
  arbol.cl.learn=rpart(Precios_clases~Cont_1+Zona_ind+Grandes_sup+Cont_2+Nro_hab+Dist_GCiud+Imp+Hab_per,cp=0.001,learn) #inicio con cp bajos y luego podo.
  aa=printcp(arbol.cl.learn)
  bb=which.min(aa[,4])
  cc=aa[bb,4]+aa[bb,5]
  dd=min(which(aa[,4]<=cc))
  ee=aa[dd,1]
  arbol.cl.learn.pod=prune(arbol.cl.learn,cp=ee)
  pred.cl.learn=predict(arbol.cl.learn.pod,learn,type="class")
  pred.cl.test=predict(arbol.cl.learn.pod,test,type="class")
  
  error.arbol.cl.learn[k]=mean(pred.cl.learn!=learn[,2]) 
  error.arbol.cl.test[k] = mean(pred.cl.test!=test[,2])
}             

mean.error.arbol.cl.learn=mean(error.arbol.cl.learn)
sd.error.arbol.cl.learn=sd(error.arbol.cl.learn)
mean.error.arbol.cl.test=mean(error.arbol.cl.test)
sd.error.arbol.cl.test=sd(error.arbol.cl.test)

#RANDOM FORESTS
set.seed(75)
K=30    
error.rf.cl.learn=matrix(NA, K)
error.rf.cl.test=matrix(NA, K)

n = nrow(data)
for(k in 1:K)   {
  smp=sample(n,round(n/3))
  learn=data[-smp,]
  test=data[smp,]
  modelo.rf.cl.learn=randomForest(Precios_clases~Cont_1+Zona_ind+Grandes_sup+Cont_2+Nro_hab+Dist_GCiud+Imp+Hab_per,ntree=500, learn)
  
  pred.rf.cl.learn=predict(modelo.rf.cl.learn,learn,type="class")
  pred.rf.cl.test=predict(modelo.rf.cl.learn,test,type="class")
  
  error.rf.cl.learn[k]=mean(pred.rf.cl.learn!=learn[,2]) 
  
  error.rf.cl.test[k] =mean(pred.rf.cl.test!=test[,2]) 
  
}             

mean.error.rf.cl.learn=mean(error.rf.cl.learn)
sd.error.rf.cl.learn=sd(error.rf.cl.learn)
mean.error.rf.cl.test=mean(error.rf.cl.test)
sd.error.rf.cl.test=sd(error.rf.cl.test)

#BOOSTING

#Los modelos Boosting son caros computacionalmente, este loop puede tomar su tiempo
#Para evitar grandes demoras, a efectos del práctico, fijamos K en 3, aunque en ejemplos reales, recomendamos usar números de K mayores (20 - 30)
#Además, para comparar errores entre modelos, la implementación del loop debe ser la misma, así que, la comparación que haremos entre modelos es únicamaente a fines ilustrativos ya que todos los loops deben presentar el mismo número de interaciones.

set.seed(72)
K=3
error.boost.cl.learn=matrix(NA, K)
error.boost.cl.test=matrix(NA, K)

n = nrow(data)
for(k in 1:K)   {
  smp=sample(n,round(n/3))
  learn=data[-smp,]
  test=data[smp,]
  modelo.boost.cl.learn=boosting(Precios_clases~Cont_1+Zona_ind+Grandes_sup+Cont_2+Nro_hab+Dist_GCiud+Imp+Hab_per,data=learn) #así expresamos un modelo boosting
  
  pred.boost.cl.learn=predict(modelo.boost.cl.learn,learn)
  pred.boost.cl.test=predict(modelo.boost.cl.learn,test)
  
  error.boost.cl.learn[k]=mean(pred.boost.cl.learn$class != learn[,2]) #en el caso de boosting, como el resultado de la función predict() es una lista que contiene varios elementos, debemos espeficarle en este caso que queremos las clases predichas.
  
  error.boost.cl.test[k] = mean(pred.boost.cl.test$class != test[,2]) 
}             

mean.error.boost.cl.learn=mean(error.boost.cl.learn)
sd.error.boost.cl.learn=sd(error.boost.cl.learn)
mean.error.boost.cl.test=mean(error.boost.cl.test)
sd.error.boost.cl.test=sd(error.boost.cl.test)

#Comparación de errores en una tabla: media y desvío estándar de errores de clasificación sobre la muestra test
MeanTest<-c(mean.error.arbol.cl.test,mean.error.rf.cl.test,mean.error.boost.cl.test) 
SdTest<-c(sd.error.arbol.cl.test,sd.error.rf.cl.test,sd.error.boost.cl.test)

Results<-rbind(MeanTest, SdTest)
colnames(Results)<-c("CART", "RF", "Bagging")
round(Results,3)

#Nuevamente, el modelo que tiene mejor desempeño es el RF, pero su error promedio se encuentra muy cerca del boosting (de todas formas, tener en cuenta que se consideraron muy pocas iteraciones)
#El árbol de clasificación es el peor modelo.
#Si bien el error promedio sobre muestra test es el indicador que debemos mirar para comparar la capacidad predictiva de los modelos, al presentar los resultados de los mismos, es recomendado presentar tanto los errores promedio (y sus desvíos) sobre la muestra test como la learn.

MeanLearn<-c(mean.error.arbol.cl.learn,mean.error.rf.cl.learn,mean.error.boost.cl.learn) 
SdLearn<-c(sd.error.arbol.cl.learn,sd.error.rf.cl.learn,sd.error.boost.cl.learn)

MeanTest<-c(mean.error.arbol.cl.test,mean.error.rf.cl.test,mean.error.boost.cl.test) 
SdTest<-c(sd.error.arbol.cl.test,sd.error.rf.cl.test,sd.error.boost.cl.test)

Results<-cbind(MeanLearn, SdLearn,MeanTest, SdTest)
rownames(Results)<-c("CART", "RF", "Bagging")
round(Results,3)