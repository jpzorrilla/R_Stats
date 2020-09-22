data(anscombe)
X<-anscombe[,1]
Y<-anscombe[,5]
plot(X,Y, 
     xlim=c(3,18), ylim=c(3,12),pch=19)
# Ajusto modelo lineal por minimos cuadrados (lm)
lmA1<-lm(Y~X)
# Agrego regresion al grafico
abline(lmA1)

coef(lmA1) #me calcula el intercepto y x

#Otra manera de calcular el intercepto y x por separado
# Pendiente 
cov(X,Y)/var(X)
# Intercepto
mean(Y)- (cov(X,Y)/var(X)) * mean(X)

#Residuales
resA1<-residuals(lmA1)
resA1

#Histograma de los residuales
hist(resA1)

#Grafico residuales
par(mfrow=c(2,2))
plot(lmA1)

#Comparacion entre modelos 2 y 3
lmA2<-lm(anscombe[,6]~anscombe[,2])
lmA3<-lm(anscombe[,7]~anscombe[,3])
# Identicos al A1
coef(lmA2)

coef(lmA3) # existen pequenas diferencias de redondeo

# Veamos la salida de un summary del modelo lineal A2
# Solo con esta salida, no podemos determinar si el modelo es adecuado.
# Variables significativas, R2 importante, F-test significativo.
summary(lmA2)

par(mfcol=c(4,3),mar=c(5,4,0.5,0.5))
plot(lmA1)
plot(lmA2)
plot(lmA3)

# No descarto la Hipotesis nula (p>0.05)
shapiro.test(residuals(lmA1))

# No cumplo con la normalidad de los residuales (p<0.05)
shapiro.test(residuals(lmA3))

summary(lmA1)$r.squared

# simulo valores con Hetrocedasticidad de los residuales
x<-seq(-4,4, length=150)
set.seed(10)
y=2+ 1.5*x + rnorm(150, 0, x+4)

# Ajusto un modelo
lmHetero<-lm(y~x)

summary(lmHetero)

par(mfrow=c(2,2))
plot(lmHetero)

hist(residuals(lmHetero))

shapiro.test(residuals(lmHetero))

#Prediccion

# newdata debe ser un data.frame
Xnew<-data.frame(X=12)
# funcion predict
YnewP<-predict(lmA1, newdata=Xnew, interval = "prediction", level=0.95)
YnewConf<-predict(lmA1, newdata=Xnew, interval = "confidence", level=0.95)

plot(X,Y, xlim=c(3,18), ylim=c(3,12),pch=19)
abline(lmA1)
# prediccion de un nuevo punto y su IdC95%
points(Xnew, YnewP[1], pch=19, col="red")
points(Xnew, YnewP[2], pch=3, col="red")
points(Xnew, YnewP[3], pch=3, col="red")

# Prediccion de la pendiente y su IdC 95%
points(Xnew, YnewConf[1], pch=2, col="green")
points(Xnew, YnewConf[2], pch=2, col="green")
points(Xnew, YnewConf[3], pch=2, col="green")

legend("topleft", 
       legend=c("Modelo","Datos", "Nuevo dato","IdC prediccion nuevo dato", "IdC prediccion media"), 
       col=c("black","black","red", "red", "green"), 
       pch=c(NaN, 19,19,3,2), lty=c(1,NaN,NaN,NaN,NaN),
       bty="n")




















