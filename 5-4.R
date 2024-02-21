library(MPV)
q4.5=p5.4
q4.5
plot(y~x, data=q4.5)
model=lm(y~x, data=q4.5)
summary(model)
anova(model)
plot(model)
#Residuals
e=model$resid
library(rgr)
cnpplt(e,pch=18,xlab="Residuals")

#yhat plot vs residuals
yhat=model$fit
plot(e~yhat,pch=19)
abline(h=0,lty=3)
library(olsrr)
ols_plot_resid_stud_fit(model, print_plot = TRUE)
#new model
attach(p5.4)
xprime=log(x)
model2=lm(y~xprime)
summary(model2)

yhat2=model2$fit
e2=model2$resid
plot(model2)
plot(e2~yhat2,pch=19)
abline(h=0,lty=3)
cnpplt(e2,pch=18,xlab="Residuals")
ols_plot_resid_stud_fit(model2, print_plot = TRUE)


#######
#Data
library(MPV)
s1=table.b2
s1
attach(s1)
lm<- lm(y ~x1+x2+x3+x4+x5)
summary(lm)

library(car)
vif(lm)
library(car)
leveragePlots(Model)

library(car)
outlierTest(Model)

library(car)
dwt(Model)

ncvTest(Model)
#studenresidual
r=rstandard(lm)
r
r>2 | r <(-2)
plot(r,pch=18,ylab="rstandard")
#R-student
rs=rstudent(lm)
rs
rs>2 | rs<(-2)
plot(rs,pch=18,ylab="R-student")



##
lm22=lm(y[-22]~x1[-22]+x2[-22]+x3[-22]+x4[-22]+x5[-22])
lm4=lm(y[-4]~ x1[-4]+x2[-4]+x3[-4]+x4[-4]+x5[-4])
lm422=lm(y [c(-4,-22)]~ x1[c(-4,-22)]+x2[c(-4,-22)]+x3[c(-4,-22)]+x4[c(-4,-22)]+x5[c(-4,-22)])
lm$coef
lm22$coef
lm4$coef
lm422$coef

(lm$coef-lm22$coef)/lm$coef
(lm$coef-lm4$coef)/lm$coef
(lm$coef-lm422$coef)/lm$coef

(summary(lm)$sigma)^2
(summary(lm22)$sigma)^2
(summary(lm4)$sigma)^2
(summary(lm422)$sigma)^2

summary(lm)$r.squared
summary(lm22)$r.squared
summary(lm4)$r.squared
summary(lm422)$r.squared

Di=cooks.distance(lm)
Di
Di>1
plot(Di,pch=19,type="o")


n=29
dfb=dfbetas(lm)
dfb


par(mfrow=c(2,3))
plot(dfb[,1],ylab="dfbIntercept",pch=19)
abline(h=2/sqrt(n),lty=3)
abline(h=-2/sqrt(n),lty=3)

plot(dfb[,2],ylab="dfbBETA1",pch=19)
abline(h=2/sqrt(n),lty=3)
abline(h=-2/sqrt(n),lty=3)

plot(dfb[,3],ylab="dfbBETA2",pch=19)
abline(h=2/sqrt(n),lty=3)
abline(h=-2/sqrt(n),lty=3)

plot(dfb[,4],ylab="dfbBETA3",pch=19)
abline(h=2/sqrt(n),lty=3)
abline(h=-2/sqrt(n),lty=3)

plot(dfb[,5],ylab="dfbBETA4",pch=19)
abline(h=2/sqrt(n),lty=3)
abline(h=-2/sqrt(n),lty=3)

##################
par(mfrow=c(1,1))
p=5
diff= dffits(lm)
x1=1:n
plot(diff~x1,pch=19)
text(x1+1,diff,cex=0.7)
abline(h=2*sqrt(p/n),lty=3)
abline(h=-2*sqrt(p/n),lty=3)

#######################

covartio=covratio(lm)
covartio
plot(covartio~x1,pch=19)
text(x1+1,covartio,cex=0.7)
abline(h=1+3*(p/n),lty=3)
abline(h=1-3*(p/n),lty=3)



######################################################
#################Dummy
library(MPV)
data(table.b11)
attach(table.b11)
Quality.lm <- lm(Quality ~ Flavor +factor(Region))
summary(Quality.lm)
anova(Quality.lm )
#############coding
newdata <- table.b11[order(Region, Flavor,Quality),]
newdata
region=rep(c(0,1,0),c(17,9,12))
attach(newdata)
dat=data.frame(y=Quality,X4=Flavor,region,Region)
lm2=lm(y~X4+Region,data=dat)
summary(lm2)
anova(lm2)

####################### model cheking

e=lm2$resid
e2=Quality.lm$resid
yhat=lm2$fit
yhat2=Quality.lm$fit


par(mfrow=c(1,2))
plot(y~X4,pch=c(1,2,3),data=dat)
legend("topright",legend=c("1","2","3"),col=c("black","red","blue"),text.col=c("black","red","blue"),pch=c(1,2,3))
plot(e~yhat,pch=c(1,2,3),data=dat)
abline(h=0,lty=3)
legend("topleft",legend=c("1","2","3"),col=c("black","red","blue"),text.col=c("black","red","blue"),pch=c(1,2,3))



plot(y~X4,pch=c(12,13,14),data=dat)
legend("topright",legend=c("1","2","3"),col=c("black","red","blue"),text.col=c("black","red","blue"),pch=c(1,2,3))
plot(e2~yhat2,pch=c(12,13,14),data=dat)
abline(h=0,lty=3)
legend("topleft",legend=c("1","2","3"),col=c("black","red","blue"),text.col=c("black","red","blue"),pch=c(12,13,14))

library(car)
qqPlot(e,col="blue",pch=19)
qqPlot(e2,col="blue",pch=19)



