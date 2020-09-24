#Read the data
data<-read.csv("RegresionData.csv", header=TRUE, sep=",",dec=".")

#Describe and explore your date
summary(data)
colnames(data)
head(data)
str(data) #see var types

#Visual exploration
boxplot(data)
pairs(data)

#Collinearity function (vif_calc)
vif_calc<-function(Xmat){
  VIF<-numeric()
  for(i in 1:ncol(Xmat)){
    Xmat_Y<-Xmat[,i]
    dataMAT<-data.frame(Xmat_Y, Xmat[,-i])
    R2<-summary(
      lm(Xmat_Y~.,data=dataMAT,na.action="na.exclude")
    )$r.squared
    VIF[i]<-1/(1-R2)
  }
  names(VIF)<-colnames(Xmat)
  print(VIF)
}# end function

vif_calc(Xmat=data[,2:5]) #Remember in this case the vif_calc must be <5
#As result we need to remove x3 or x4 (only 1)

vif_calc(data[,2:4]) #In this case we decided remove x4
#Now we don't have collinearity problems between vars.

#With the problem solved, we'll create a new data frame
datalm<-data[,-5]

#A complete model (~. indicates that it includes x1+x2+x3+Factor)
lm_all<-lm(y~., data=datalm)

#Explore the new model
summary(lm_all)

#Visualize the Histogram
hist(residuals(lm_all), main="")

#Viz the residuals
par(mfrow=c(2,2))
plot(lm_all)

#Model selection by F test
colnames(datalm)

#New model without x3 called lm_red1
lm_red1<-lm(y~x1+x2+Factor, data=datalm)
summary(lm_red1)

#ANOVA
anova(lm_all,lm_red1)

#We obtain that p=0.94 we rejected the H0 that means the Factor var. isn't relevant so we can drop it
lm_red2<-lm(y~x1+x2, data=datalm)
anova(lm_red1,lm_red2)
#In this case p=2.2e^-16 we can rejected H0 which the coef. asosiated to Factor var. = 0

#ANOVA with 1 Factor (categorical var.)
lm_Factor<-lm(y~Factor, data=datalm)
summary(lm_Factor)

anova(lm_Factor)

#Viz
par(mfrow=c(2,2))
plot(lm_Factor)

#Inc. Factor as var
lm_red3<-lm(y~x2+Factor, data=datalm)
anova(lm_red1,lm_red3)

summary(lm_red3)

#Recommendation: Always viz the residuals
par(mfrow=c(2,2))
plot(lm_red3)
hist(residuals(lm_red3))

#Shapiro Test
shapiro.test(residuals(lm_red3))

#New predictions
newvalues=data.frame(x1=10, x2= 8, x3=20 ,Factor= "A")
NewPred<-predict(lm_red3, newvalues, interval="prediction")
NewPred

#Viz
plot(datalm[,'x2'],datalm[,"y"], col=datalm[,"Factor"], xlab="Co-variable x2", ylab="Respuesta (Y)",pch=19)
legend("topleft", legend=c("A","B","C"), col=c(1,2,3), bty="n",pch=19)
abline(a=coef(lm_red3)[1], b=coef(lm_red3)[2])
abline(a=coef(lm_red3)[1]+coef(lm_red3)[4], b=coef(lm_red3)[2], col="green")

points(newvalues$x2, NewPred[1], pch=19, col="blue")

#Maximum Likelihood Estimation (glm)
glm_mod<- glm(y~., data=datalm, family=gaussian( link="identity"))
summary(glm_mod)