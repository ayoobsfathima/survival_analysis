install.packages("ggfortify")
library(KMsurv)
library(ggplot2)
library(survminer)

dat=read.table(file.choose(),header=T,sep=",")
head(dat)
dat1=dat[,-4]
head(dat1)
------------------------------------------------------------------------
library(ggfortify)
library(survival)
fit <- survfit(Surv(time, censor) ~ 1, data = dat1)
plot(fit)
autoplot(fit)

ggsurvplot(
    fit = survfit(Surv(time,censor) ~ 1, data=dat1), 
    xlab = "Months", 
    ylab = "Survival Probability")



----------------------------------------------------------------
#EXPONENTIAL
dat2=dat1[-1,]
View(dat2)

fit_exp <- flexsurvreg(Surv(time, censor) ~ 1,
                   dist = "exponential", data =dat2 )
ggflexsurvplot(fit_exp)

par(mfrow=c(1,2),mar=c(5.1,5.1,4.1,2.1))
curve(1-pexp(x,0.03023), 1, 60,col='red', lwd=2, ylab=expression(hat(S)(t)), xlab='t',bty='n',ylim=c(0,1))
curve(hexp(x,0.03023), 1, 60,col='blue', lwd=2, ylab=expression(hat(h)(t)), xlab='t',bty='n')
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))


------------------------------------------------------------------------------
#WEIBULL
fit_wei <- flexsurvreg(Surv(time, censor) ~ 1,
                   dist = "weibull", data =dat2 )
ggflexsurvplot(fit_wei)


par(mfrow=c(1,2),mar=c(5.1,5.1,4.1,2.1)) 
curve(1-pweibull(x, 0.7306,36.4888), 
      from=0, to=60, col='red', lwd=2, ylab=expression(hat(S)(t)), xlab='t',bty='n',ylim=c(0,1))
curve(hweibull(x, 0.7306,36.4888), 
      from=0, to=60, col='blue', lwd=2, ylab=expression(hat(h)(t)), xlab='t',bty='n')
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))
------------------------------------------------------------------------------
#GAMMA
fit_gam <- flexsurvreg(Surv(time, censor) ~ 1,
                   dist = "gamma", data =dat1 )
ggflexsurvplot(fit_gam)
summary(fit_gam)


par(mfrow=c(1,2),mar=c(5.1,5.1,4.1,2.1))
curve(1-pgamma(x,0.69547,0.01759), 0, 60,col='red', lwd=2, ylab=expression(hat(S)(t)), xlab='t',bty='n',ylim=c(0,1))
curve(hgamma(x,0.69547,0.01759), 0, 60,col='blue', lwd=2, ylab=expression(hat(h)(t)), xlab='t',bty='n')
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))

------------------------------------------------------------------------------
#LOGNORMAL
fit_ln <- flexsurvreg(Surv(time, censor) ~ 1,
                   dist = "lognormal", data =dat2 )
ggflexsurvplot(fit_ln)


par(mfrow=c(1,2),mar=c(5.1,5.1,4.1,2.1))
curve(1-plnorm(x, 2.981, 1.74), 0, 60,col='red', lwd=2, ylab=expression(hat(S)(t)), xlab='t',bty='n',ylim=c(0,1))
curve(hlnorm(x,2.981, 1.74), 0, 60,col='blue', lwd=2, ylab=expression(hat(h)(t)), xlab='t',bty='n')
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))



-----------------------------------------------------------------------------------------

plot(fit)
lines(fit_exp, col="blue", lwd.ci=1, lty.ci=1)
lines(fit_wei, col="red", lwd.ci=1, lty.ci=1)
lines(fit_gam, col="green", lwd.ci=1, lty.ci=1)
lines(fit_ln, col="orange", lwd.ci=1, lty.ci=1)



----------------------------------------------------------------------------------------------------------
library(fitdistrplus)
library(flexsurv)
library(ggfortify)
library(survival)


fitg <- fitdist(p, "gamma")
summary(fitg)
plot(fitg)

fitw <- fitdist(p, "weibull")
summary(fitw)
plot(fitw)

fitln <- fitdist(p, "lnorm")
summary(fitln)
plot(fitln)
