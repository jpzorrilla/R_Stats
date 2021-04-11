# error s/muestra entrenamiento (ESME) vs error s/muestra test (ESMT)
# esme alto, esmt alto = mal performance del modelo
# esme bajo (buen performance del modelo) = sobreajustando los datos

# EVALUACION ENTRE TESTS

# esmt < esme = el performance del modelo es bueno
# esmt > esme = es probable que haya sobreajuste de los datos
summary(airquality)

#drop missing values
datos.reg=na.omit(airquality)

#Adjust the model
modelo.reg=lm(Ozone~Solar.R+Temp,datos.reg)

#Check
summary(modelo.reg)

# Training = 70 - Test = 30
# Cuanto mayor sera el MSE, peor el performance del modelo

#ERROR SOBRE LA MUESTRA TEST - PROBLEMA DE REGRESION

set.seed(100) #fijo un punto de inicio para que a todos nos de lo mismo, puede ser cualquier numero.
K=30    #numero de iteraciones (cuantas veces parto la base en dos), este valor puede variar.
error.reg.learn=matrix(NA, K) 
error.reg.test=matrix(NA, K) 
#genero dos matrices vacias donde se iran almacenando los errores (una para los errores sobre la muesta de entrenamiento y otra para los errores sobre la muestra test).
n.reg = nrow(datos.reg) #defino el numero de filas que tienen mis datos.

#COMIENZA EL LOOP
for(k in 1:K)   { #en cada iteracion hago lo que sigue
  smp.reg=sample(n.reg,round(n.reg/3)) #muestreo una tercera parte de mis datos. Esto genera posiciones aleatorias del vector n.reg que corresponden a 1/3 de los datos (37 posiciones).
  learn.reg=datos.reg[-smp.reg,c(1,2,4)] #defino la muestra de entrenamiento (todas las posiciones no sorteadas en smp de las columnas 1,2 y 4 que corresponden a las variables consideradas en el modelo).
  test.reg=datos.reg[smp.reg,c(1,2,4)] #defino la muestra test de manera analoga.
  modelo.reg.learn=lm(Ozone~Solar.R+Temp,learn.reg) #ajusto el modelo con la muestra learn
  pred.learn.reg= predict(modelo.reg.learn,learn.reg) #predicciones con los datos de entrenamiento    
  pred.test.reg= predict(modelo.reg.learn,test.reg)   #predicciones con los datos de la muestra test
  error.reg.learn[k] = sqrt(mean((learn.reg[,1]-pred.learn.reg)^2)) #calculo la raiz cuadrada del error cuadratico medio sobre la muestra de entrenaminto. Tendre k valores de este error (uno por cada iteracion). Elijo la raiz cuadrada para que me quede en las mismas unidades de la variable de respuesta y sea mas directamente interpretable. 
  error.reg.test[k] = sqrt(mean((test.reg[,1]-pred.test.reg)^2)) 
  #calculo la raiz cuadrada del error cuadratico medio sobre la muestra test. Tendre k valores de este error (uno por cada iteracion) 
}   

#Resultados del loop
mean.error.reg.learn=mean(error.reg.learn) #como tengo 30 valores de error, un por iteracion, calculo el promedio de los errores sobre la muestra de entrenamiento
sd.error.reg.learn=sd(error.reg.learn) #calculo el desvio estandar de los errores sobre la muestra de entrenamiento para tener una idea de la variabilidad
mean.error.reg.test=mean(error.reg.test) #promedio de los errores sobre la muestra test
sd.error.reg.test=sd(error.reg.test)#sd de los errores sobre la muestra test


mean.error.reg.learn
sd.error.reg.learn
mean.error.reg.test
sd.error.reg.test

#el error sobre la muestra de entrenamiento es algo menor que el de la muestra test, como es esperable.
#Pero no tanto menor, claramente aqui no hay sobreajuste de los datos lo cual es esperable al trabajar con un modelo lineal

#Estimacion del error en el contexto de clasificacion
#Para ilustrar la aplicacion del error de clasficacion generaremos una var. de respuesta categorica a partir de la var. continua Ozone que llamaremos Ozone_cat
Ozone_cat=ifelse(datos.reg$Ozone<40,0,1)
Ozone_cat=as.factor(Ozone_cat)
summary(Ozone_cat)

#Generamos datos para la classification
datos.cl=cbind(Ozone_cat,datos.reg[,c(2,4)])
summary(datos.cl)

#logit
modelo.cl=glm(Ozone_cat~Solar.R+Temp,family=binomial, data=datos.cl)
summary(modelo.cl)

#Calc error classification

#ERROR SOBRE LA MUESTRA TEST - PROBLEMA DE CLASIFICACIoN

set.seed(100) #fijo un punto de inicio para que a todos nos de mismo, puede ser cualquier numero.
K=30    #numero de iteraciones (cuantas veces separo la base dos), este valor puede variar.
error.cl.learn=matrix(NA, K) 
error.cl.test=matrix(NA, K) 
#genero dos matrices vacias donde se iran almacenando los errores (una para los errores sobre la muesta de entrenamiento y otra para los errores de la muestra test).
n.cl = nrow(datos.cl) #defino el numero de filas que tienen mis datos.

#COMIENZA EL LOOP
for(k in 1:K)   { #en cada iteracion hago lo que sigue
  smp.cl=sample(n.cl,round(n.cl/3)) #muestreo una tercera parte de mis datos. Esto genera posiciones aleatorias del vector n.cl que corresponden a 1/3 de los datos (37 posiciones).
  learn.cl=datos.cl[-smp.cl,,] #defino la muestra de entrenamiento (todas las posiciones no sorteadas en smp.cl y todas las cols de datos.cl).
  test.cl=datos.cl[smp.cl,] #defino la muestra test de manera analoga.
  modelo.cl.learn=glm(as.factor(Ozone_cat)~Solar.R+Temp,family="binomial",learn.cl) #ajusto el modelo con la muestra learn
  
  pred.learn.cl=predict(modelo.cl.learn,learn.cl,type = "response") #predicciones con los datos de entrenamiento
  pred.learn.cl.cat = as.factor(ifelse(pred.learn.cl > 0.5, 1, 0)) #con esto categorizo las probabilidades que me predice el modelo.
  pred.test.cl= predict(modelo.cl.learn,test.cl,type = "response")    #predicciones con los datos de la muestra test
  pred.test.cl.cat = as.factor(ifelse(pred.test.cl > 0.5, 1, 0))
  
  error.cl.learn[k] = mean(pred.learn.cl.cat!= learn.cl[,1])
  #calculo el error de clasificacion sobre la muestra de entrenamiento. Tendre k valores de este error (uno por cada iteracion). El simbolo != refiere al simbolo de distinto.
  error.cl.test[k] = mean(pred.test.cl.cat!= test.cl[,1])
  #error de clasificacion sobre la muestra test
}       


#Resultados
mean.error.cl.learn=mean(error.cl.learn) #como tengo 30 valores de error, uno por iteracion, calculo el promedio de los errores sobre la muestra de entrenamiento, el error de clasificacion es una proporcion que corresponde a la propocion de casos mal clasificados por el modelo. Este valor es el promedio de 30 errores de clasificacion.
sd.error.cl.learn=sd(error.cl.learn) #calculo el desvio estandar de los errores sobre la muestra de entrenamiento para tener una idea de la variabilidad
mean.error.cl.test=mean(error.cl.test) #promedio de los errores sobre la muestra test
sd.error.cl.test=sd(error.cl.test)#sd de los errores sobre la muestra test

mean.error.cl.learn
sd.error.cl.learn
mean.error.cl.test
sd.error.cl.test

#ERROR POR VALIDACION CRUZADA

#En este caso lo que hacemos es partir los datos en 5 o 10 partes iguales (de forma aleatoria), y en cada iteración, entrenamos el modelo con 4/5 o 9/10 de los datos y lo evaluamos con el 1/5 o 1/9 restante
#Esto se hace hasta que cada parte fue usada como muestra de evaluación
#Finalmente se promedian los errores sobre muestras de evaluación
#La elección del número de iteraciones (parámetro k), es usualemente (como damos a entender arriba), k=5 o k=10 pues se ha visto que estos valores de k, en general, brindan una buena estimación del error.

install.packages('caret', dependencies = TRUE)
require(caret)

#REGRESSION

#Con la función trainControl determinamos el método que queremos usar y el parámetro k (number), en este caso usaremos validación cruzada (cv) y un valor de k=10. 
#Explorar más opciones en: http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
train.control <- trainControl(method = "cv", number=10)

#A continuación la función que calcula el error por vc a partir del modelo especificado. Primero indicamos la fórmula del modelo de forma genérica: Ozone~Solar.R+Temp, luego los datos a usar. Aquí indicamos la totalidad de los datos porque la propia función se encarga de partirlos. El modelo que queremos usar, en este caso es un modelo lineal. El paquete caret tiene una amplia gama de modelos dentro de sus opciones. El útlimo argumento es trControl que es lo que nosotros definimos más arriba como train.control.
model.reg <- train(Ozone~Solar.R+Temp, data = datos.reg, method = "lm",trControl = train.control)

#Resumen de los resultados
print(model.reg)

#Lo central de los resultados es lo referente a Resampling results
#Allí se indican los errores por validación cruzada

#CLASIFICACION

train.control <- trainControl(method = "cv")
model.cl <- train(Ozone_cat~Solar.R+Temp, data = datos.cl, method = "glm",family = "binomial",trControl = train.control) #en el caso de clasificación utilizamos un glm (familia binomial).
print(model.cl)

#En este caso, en Resampling results vemos dos índices: Accuracy que refiere a la proporción de casos correctamente clasificados (1-error de clasificación) y Kappa que es otro índice para problemas de clasificación