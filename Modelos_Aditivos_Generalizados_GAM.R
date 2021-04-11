library(mgcv)
library(mgcViz) #paquete para realizar salias graficas mas personalizadas
#https://mfasiolo.github.io/mgcViz/reference/plot.gamViz.html

citation("mgcv")
#citas relacionadas con esta libreria

datos=na.omit(airquality)
datos=datos[,1:4]

summary(datos)

plot(datos,col="green")

#Ajuste primer GAM con todas las opciones x defecto
modelo.gam1=gam(datos$Ozone~s(datos$Solar.R)+s(datos$Wind)+s(datos$Temp)) 
summary(modelo.gam1)

#spline por defecto
modelo.gam1_2=gam(datos$Ozone~s(datos$Solar.R,bs="cr")+s(datos$Wind)+s(datos$Temp,bs="ts")) 

#tensor product o te()
modelo.gam1_3=gam(datos$Ozone~s(datos$Solar.R,bs="cr")+s(datos$Wind)+s(datos$Temp,bs="cr")+te(datos$Temp,datos$Solar.R))

#Dimension de la base = al maximo valor de grados de libertad que puede tomar cada termino
gam.check(modelo.gam1)

#Modificar segun criterio k= dimension de la base
modelo.gam1_4=gam(datos$Ozone~s(datos$Solar.R,k=20)+s(datos$Wind,k=15)+s(datos$Temp))

#GAM semi-parametrico
modelo.gam1_5=gam(datos$Ozone~datos$Solar.R+s(datos$Wind,k=15)+s(datos$Temp))

#Retomamos el GAM1
modelo.gam1=gam(datos$Ozone~s(datos$Solar.R)+s(datos$Wind)+s(datos$Temp)) 

#Verificamos los residuales
par(mfrow=c(2,2))
gam.check(modelo.gam1)

#DataViz
par(mfrow=c(1,3))
plot(modelo.gam1)

#GAM semi-parametrico (donde la var. Solar.R no se suaviza)
modelo.gam1_sp=gam(datos$Ozone~datos$Solar.R+s(datos$Wind)+s(datos$Temp),family="gaussian") 
summary(modelo.gam1_sp)

#check
gam.check(modelo.gam1_sp)

#dataviz
modelo.gam1_sp=getViz(modelo.gam1_sp)
print(plot(modelo.gam1_sp,allTerms=T),pages=1)

#Comparamos a los modelos mediante GCV y AIC
modelo.gam1$gcv.ubre
modelo.gam1_sp$gcv.ubre
AIC(modelo.gam1)
AIC(modelo.gam1_sp)

#El modelo semi-parametrico es levemente peor, equivalente segun el criterio de las dos unidades de AIC.
#Por criterio de parsimonia parece adecuado retener el modelo semi-parametrico

#siempre es recomendable hacer una evaluacion por fuera del modelo.
#Calculemos el error sobre la muestra test para los dos modelos ajustados.
#LOOP PARA ERROR SOBRE LA MUESTRA TEST modelo.gam1
#Este es el mismo loop de la semana 9 adaptado a este ejemplo.

set.seed(100)
K=30    
error.gam1.learn=matrix(NA, K) 
error.gam1.test=matrix(NA, K) 
n.gam1 = nrow(datos) 

#COMIENZA EL LOOP
for(k in 1:K)   { 
  smp.gam1=sample(n.gam1,round(n.gam1/3))
  learn.gam1=datos[-smp.gam1,] 
  test.gam1=datos[smp.gam1,] 
  modelo.gam1.learn=gam(learn.gam1$Ozone~s(learn.gam1$Solar.R)+s(learn.gam1$Wind)+s(learn.gam1$Temp)) 
  pred.gam1.learn=predict(modelo.gam1.learn,data=learn.gam1) #predicciones con los datos de entrenamiento 
  pred.gam1.test=predict(modelo.gam1.learn,data=test.gam1)    #predicciones con los datos de la muestra test
  
  error.gam1.learn[k] = sqrt(mean((learn.gam1[,1]-pred.gam1.learn)^2)) 
  
  error.gam1.test[k] = sqrt(mean((test.gam1[,1]-pred.gam1.test)^2)) 
  
}

#Resultados del loop (media y desvio estandar)
mean.error.gam1.learn=mean(error.gam1.learn) 
sd.error.gam1.learn=sd(error.gam1.learn) 
mean.error.gam1.test=mean(error.gam1.test) 
sd.error.gam1.test=sd(error.gam1.test)
mean.error.gam1.learn
sd.error.gam1.learn
mean.error.gam1.test
sd.error.gam1.test

#Observamos un aumento considerable en el error sobre la muestra test, esto probablemente este relacionado con problemas de sobreajuste.
#Hagamos la misa evaluacion para le modelo semi-parametrico.
#LOOP PARA ERROR SOBRE LA MUESTRA TEST modelo.gam1_sp

set.seed(100)
K=30    
error.gam1sp.learn=matrix(NA, K) 
error.gam1sp.test=matrix(NA, K) 
n.gam1sp = nrow(datos) 

#COMIENZA EL LOOP
for(k in 1:K)   { 
  smp.gam1sp=sample(n.gam1sp,round(n.gam1sp/3))
  learn.gam1sp=datos[-smp.gam1sp,] 
  test.gam1sp=datos[smp.gam1sp,] 
  modelo.gam1sp.learn=gam(learn.gam1$Ozone~learn.gam1sp$Solar.R+s(learn.gam1sp$Wind)+s(learn.gam1sp$Temp)) 
  pred.gam1sp.learn=predict(modelo.gam1sp.learn,data=learn.gam1sp) #predicciones con los datos de entrenamiento, al usar predicto con gam indicar "data=" 
  pred.gam1sp.test=predict(modelo.gam1sp.learn,data=test.gam1sp)  #predicciones con los datos de la muestra test
  
  error.gam1sp.learn[k] = sqrt(mean((learn.gam1sp[,1]-pred.gam1sp.learn)^2)) 
  error.gam1sp.test[k] = sqrt(mean((test.gam1sp[,1]-pred.gam1sp.test)^2)) 
  
}

#Resultados del loop (media y desvio estandar)
mean.error.gam1sp.learn=mean(error.gam1sp.learn) 
sd.error.gam1sp.learn=sd(error.gam1sp.learn) 
mean.error.gam1sp.test=mean(error.gam1sp.test) 
sd.error.gam1sp.test=sd(error.gam1sp.test)
mean.error.gam1sp.learn
sd.error.gam1sp.learn
mean.error.gam1sp.test
sd.error.gam1sp.test

#El error sobre la muestra test sigue siendo elevado en relacion al error sobre la muestra learn, aunque la diferencia no es tanta como antes.
#Si bien el error sobre la muestra learn es bastante superior al del modelo previo, el error sobre la muestra test es algo menor, recuerden que en el contexto de modelos de AA este error es de gran importancia para evaluar y seleccionar modelos.
#En este sentido parece adecuado, quedarnos con el modelo semiparametrico.

##GAM con variable de respuesta categorica, dos categorias = GAM Logistico
datos.cl=read.csv2("datos_precios.csv", sep=";",dec=",")
datos.cl$Precio_cat=as.factor(datos.cl$Precio_cat)
summary(datos.cl)

modelo.gam.cl=gam(datos.cl$Precio_cat~s(datos.cl$Tasa_crim)+s(datos.cl$Area_parques)+s(datos.cl$Emisiones),family = binomial)
summary(modelo.gam.cl)

#En el caso de un GAM logistico, en lugar del GCV se busca minimizar otro valor: el URBE.
#Calculemos el error sobre muestra de prueba.
#LOOP PARA ERROR DE CLASIFICACION modelo.gam.cl

set.seed(100) 
K=30    
error.gam.cl.learn=matrix(NA, K) 
error.gam.cl.test=matrix(NA, K) 
n.cl = nrow(datos.cl) 

#COMIENZA EL LOOP
for(k in 1:K)   { 
  smp.cl=sample(n.cl,round(n.cl/3)) 
  learn.cl=datos.cl[-smp.cl,] 
  test.cl=datos.cl[smp.cl,] 
  
  modelo.gam.cl.learn=gam(learn.cl$Precio_cat~s(learn.cl$Tasa_crim)+s(learn.cl$Area_parques)+s(learn.cl$Emisiones),family = binomial)
  
  pred.gam.learn.cl=predict(modelo.gam.cl.learn,data=learn.cl,type = "response") 
  pred.gam.learn.cl.cat = as.factor(ifelse(pred.gam.learn.cl > 0.5, 1, 0)) 
  pred.gam.test.cl= predict(modelo.gam.cl.learn,data=test.cl,type = "response")   
  pred.gam.test.cl.cat = as.factor(ifelse(pred.gam.test.cl > 0.5, 1, 0))
  
  error.gam.cl.learn[k] = mean(pred.gam.learn.cl.cat!= learn.cl[,1])
  error.gam.cl.test[k] = mean(pred.gam.test.cl.cat!= test.cl[,1])
  
}    

#Resultados del loop
mean.error.cl.learn.gam=mean(error.gam.cl.learn)  
sd.error.cl.learn.gam=sd(error.gam.cl.learn) 
mean.error.cl.test.gam=mean(error.gam.cl.test) 
sd.error.cl.test.gam=sd(error.gam.cl.test)
mean.error.cl.learn.gam
sd.error.cl.learn.gam
mean.error.cl.test.gam
sd.error.cl.test.gam

#Pasamos de un 13% a un 50% de error al considerar la muestra learn y test respectivamente
#Es una gran diferencia. Habria que evaluar posibles problemas. El sobreajuste seguramente sea parte de la explicacion.