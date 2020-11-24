install.packages("ggfortify")
library(KMsurv)
library(ggplot2)
library(survminer)

dat=read.table(file.choose(),header=T,sep=",")
head(dat)
dat1=dat[,-4]
View(dat1)
------------------------------------------------------------------------
library(ggfortify)
library(survival)
fit <- survfit(Surv(time, censor) ~ 1, data = dat1)
fit
autoplot(fit)

ggsurvplot(
    fit = survfit(Surv(time,censor) ~ 1, data=dat1), 
    xlab = "Months", 
    ylab = "Overall survival probability")

-----------------------------------------------------------

----------------------------------------------------------------
#EXPONENTIAL
dat2=dat1[-1,]
View(dat2)


fit_exp=survreg(Surv(time,censor)~1,dat2,dist="exponential")
summary(fit_exp)
fit_exp$coefficients

lambda=exp(-(summary(fit_exp)$table[,1]))

t<-seq(1,60)
hazard.exponential<-rep(lambda,60)
survival.exponential<-exp(-lambda*t)

par(mfrow=c(1,2))
plot(t,hazard.exponential,type="l",xlab="Time (month)",ylab="Hazard")
plot(t, survival.exponential,type="l",xlab="Time (month)",ylab="Survival")


------------------------------------------------------------------------------
#WEIBULL
fit_wei=survreg(Surv(time,censor)~1,dat2,dist="weibull")
summary(fit_wei)
fit_wei$coefficients

# The parameters
intercept<-3.597004
scale<-1.37

par(mfrow=c(1,2),mar=c(5.1,5.1,4.1,2.1)) # Make room for the hat.
# S(t), the survival function
curve(pweibull(x, scale=exp(intercept), shape=1/scale, lower.tail=FALSE), 
      from=0, to=60, col='red', lwd=2, ylab=expression(hat(S)(t)), xlab='t',bty='n',ylim=c(0,1))
# h(t), the hazard function
curve(dweibull(x, scale=exp(intercept), shape=1/scale)
      /pweibull(x, scale=exp(intercept), shape=1/scale, lower.tail=FALSE), 
      from=0, to=60, col='blue', lwd=2, ylab=expression(hat(h)(t)), xlab='t',bty='n')
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))
------------------------------------------------------------------------------
#GAMMA
fit_gam=survreg(Surv(time,censor)~1,dat1,dist="gaussian")
summary(fit_gam)
fit_gam$coefficients
------------------------------------------------------------------------------
#LOG
fit_log=survreg(Surv(time,censor)~1,dat2,dist="lognormal")
summary(fit_log)
fit_log$coefficients

curve(1-plnorm(x, 2.981, 1.74), 0, 60)


curve(logNormHaz(x,2.981, 1.74), 0, 60)


logNormHaz <- function(x, mu, s){
    dens_vals <- dlnorm(x, mu, s)
    s_vals <- (1 - plnorm(x, mu, s))
    Hazs <- (dens_vals/s_vals)
    return(Hazs)
}

-----------------------------------------------------------------------------------------

library(fitdistrplus)
library(flexsurv)

dat=read.table(file.choose(),header=T,sep=",")
colnames(data)
dat=dat[,-4]
dat1=subset(dat,group==1)
time=dat1$time
censor=dat1$censor

library(ggfortify)
library(survival)
fit <- survfit(Surv(time, censor) ~ 1, data = dat1)
summary(fit)
k=autoplot(fit)


fitg <- fitdist(p, "gamma")
summary(fitg)
plot(fitg)

fitw <- fitdist(p, "weibull")
summary(fitw)
plot(fitw)

fitln <- fitdist(p, "lnorm")
summary(fitln)
plot(fitln)
