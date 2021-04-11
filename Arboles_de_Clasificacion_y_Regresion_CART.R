install.packages("rpart")
install.packages("rpart.plot")
require(rpart)
require(rpart.plot) #para graficos personalizados

#Help
?rpart
?rpart.control

#rpart = Recursive Partitioning And Regression Trees

summary(iris)

set.seed(20)
arbol.cl1=rpart(Species~.,iris)
#considero como variable de respuesta Species que tiene 3 categorias
#Con el punto indico que considero a las restantes variables de la data.frame como variables predictoras o explicativas
#Hagamos un primer intento con todas las opciones por defecto.

#DataViz
plot(arbol.cl1,margin=0.2)
text(arbol.cl1)

summary(arbol.cl1)

#Segundo arbol
set.seed(20)
arbol.cl2=rpart(Species~.,iris,minsplit=5,cp=0.0001,maxcompete=2)

#Para no desplegar el summary que es extremadamente largo, solicitamos en este caso al R que nos muestre directamente la tabla de cp
printcp(arbol.cl2)

#Poda - podo con la funcion prune indicando el cp del arbol que quiero obtener
arbol.cl2.pr=prune(arbol.cl2,cp=0.02)

rpart.plot(arbol.cl2.pr)

#Obtener valores predichos
pred_arbol.cl2.pr=predict(arbol.cl2.pr,type="class",iris)

#Create a table
t=table(pred_arbol.cl2.pr,iris[,5]) #primer argumento va en filas, segundo argumento en columnas.
t

#Error de clasificacion a partir de la tabla de confusion
1-((sum(diag(t)))/nrow(iris))

#LOOP PARA ERROR DE CLASIFICACION
set.seed(75)
K=30    
error.cl.learn=matrix(NA, K)
error.cl.test=matrix(NA, K)
n = nrow(iris)
for(k in 1:K)   {
  smp=sample(n,round(n/3))
  learn=iris[-smp,]
  test=iris[smp,]
  arbol.cl.learn=rpart(Species~.,cp=0.0001,minsplit=5,learn) #inicio con los mismos parametros que escogimos arriba que generan una secuencia larga de arboles anidados y luego en cada iteracion elijo aquel que minimiza el xerror. Otra opcion podria haber sido, definir el cp escogido arriba (0.02) en la funcion arbol.cl.learn. Son alternativas que persiguen el mismo objetivo.
  a=printcp(arbol.cl.learn)
  pr.cl.learn=prune(arbol.cl.learn,cp=a[which.min(a[,4]),1])
  pred.cl.learn=predict(pr.cl.learn,type="class",learn)
  pred.cl.test=predict(pr.cl.learn,type="class",test)
  error.cl.learn[k] = mean(pred.cl.learn!= learn[,5])
  error.cl.test[k] = mean(pred.cl.test!= test[,5])
}   

#Resultados del loop
mean.error.cl.learn=mean(error.cl.learn)
sd.error.cl.learn=sd(error.cl.learn)

mean.error.cl.test=mean(error.cl.test)
sd.error.cl.test=sd(error.cl.test)

mean.error.cl.learn
sd.error.cl.learn

mean.error.cl.test
sd.error.cl.test

#Arboles de Regresion
summary(airquality)

modelo.rg=rpart(Ozone~Solar.R+Wind+Temp,cp=0.001, minsplit=10,airquality) 
rpart.plot(modelo.rg)

#Arbol anidado
cp.reg=printcp(modelo.rg)

#Poda
modelo.rg.pr=prune(modelo.rg,cp=0.0268930) #cp del arbol 4
rpart.plot(modelo.rg.pr)
summary(modelo.rg.pr)

#R2
modelo.rg.pr
(125143.100-(4226.510+5121.958+11489.000+12046.950+3652.941))/125143.100

#LOOP PARA ERROR DE REGRESION
set.seed(81)
K=30    
error.reg.learn=matrix(NA, K)
error.reg.test=matrix(NA, K)
var.reg.learn=matrix(NA, K)
var.reg.test=matrix(NA, K)
n.r = nrow(airquality)
for(k in 1:K)   {
  smp.r=sample(n.r,round(n.r/3))
  learn.r=airquality[-smp.r,]
  test.r=airquality[smp.r,]
  arbol.reg.learn=rpart(Ozone~Solar.R+Wind+Temp,cp=0.001, minsplit=10,learn.r) 
  a=printcp(arbol.reg.learn)
  pr.reg.learn=prune(arbol.reg.learn,cp=a[which.min(a[,4]),1])
  pred.reg.learn=predict(pr.reg.learn,learn.r)
  pred.reg.test=predict(pr.reg.learn,test.r)
  learn.sin.na=na.omit(cbind(pred.reg.learn,learn.r[,1])) #esto se hace para quitar los NA que luego me da problemas para calcular los errores.
  test.sin.na=na.omit(cbind(pred.reg.test,test.r[,1]))
  error.reg.learn[k] = sqrt(mean((learn.sin.na[,1]-learn.sin.na[,2])^2)) 
  error.reg.test[k] = sqrt(mean((test.sin.na[,1]-test.sin.na[,2])^2)) 
  
  #Nos podria interesar calcular una pseudo varianza explicada a partir del MSE a modo de obtener resutlados mas interpretables del performance del modelo.
  
  var.reg.learn[k]=1-((mean((learn.sin.na[,1]-learn.sin.na[,2])^2)/var(learn.sin.na[,2])))
  var.reg.test[k]=1-((mean((test.sin.na[,1]-test.sin.na[,2])^2)/var(test.sin.na[,2])))
  
}        

#Resultados del loop
mean.error.reg.learn=mean(error.reg.learn)
sd.error.reg.learn=sd(error.reg.learn)
mean.error.reg.test=mean(error.reg.test)
sd.error.reg.test=sd(error.reg.test)

mean.var.reg.learn=mean(var.reg.learn)
sd.var.reg.learn=sd(var.reg.learn)
mean.var.reg.test=mean(var.reg.test)
sd.var.reg.test=sd(var.reg.test)

mean.error.reg.learn
sd.error.reg.learn

mean.error.reg.test
sd.error.reg.test

mean.var.reg.learn
sd.var.reg.learn

mean.var.reg.test
sd.var.reg.test