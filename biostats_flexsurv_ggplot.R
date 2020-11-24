library(gridExtra)
library(KMsurv)
library(ggplot2)
library(survminer)
library(ggfortify)
library(survival)
library(flexsurv)
library(extrafont)
font_import()
loadfonts(device = "win")
library(ggplot2)
library(extrafont)

#DATA
dat=read.table(file.choose(),header=T,sep=",")
head(dat)
dat1=dat[,-4]
head(dat1)
-----------------------------------------------------------------------------------------------------
#SURVIVAL CURVE
fit <- survfit(Surv(time, censor) ~ 1, data = dat1)
plot(fit)
autoplot(fit)

ggsurvplot(
    fit = survfit(Surv(time,censor) ~ 1, data=dat1), 
    xlab = "Months", 
    ylab = "Survival Probability")
---------------------------------------------------------------------------------------------
#EXPONENTIAL
dat2=dat1[-1,]
head(dat2)

fit_exp <- flexsurvreg(Surv(time, censor) ~ 1,
                   dist = "exponential", data =dat2 )
ggflexsurvplot(fit_exp)

expo <- function(x) {1-pexp(x,0.03023)}
hexpo<- function(x) {hexp(x,0.03023)}

exo_s=ggplot(data.frame(x = c(1, 60)), aes(x = x))+
        stat_function(fun = expo, size=1.5,col="deeppink")+
theme_bw()+
ggtitle("SURVIVAL FUNCTION")+
  xlab("Time(months)") + ylab("Survival Probabilities") +
theme(plot.title = element_text(color = "black", size = 20,family="Comic Sans MS",hjust=0.5),
      text=element_text(size = 10, family="Comic Sans MS"),
              axis.text.x=element_text(colour="black", size = 10),
              axis.text.y=element_text(colour="black", size = 10))
exo_s

exo_h=ggplot(data.frame(x = c(1, 60)), aes(x = x))+
        stat_function(fun = hexpo, size=1.5,col="dodgerblue3")+
theme_bw()+
ggtitle("HAZARD FUNCTION")+
  xlab("Time(months)") + ylab("Hazard Rate") +
theme(plot.title = element_text(color = "black", size = 20,family="Comic Sans MS",hjust=0.5),
      text=element_text(size = 10, family="Comic Sans MS"),
              axis.text.x=element_text(colour="black", size = 10),
              axis.text.y=element_text(colour="black", size = 10))
exo_h

grid.arrange(exo_s,exo_h, ncol=2)

------------------------------------------------------------------------------
#WEIBULL
fit_wei <- flexsurvreg(Surv(time, censor) ~ 1,
                   dist = "weibull", data =dat2 )
ggflexsurvplot(fit_wei)

wei <- function(x) {1-pweibull(x, 0.7306,36.4888)}
hwei <- function(x) {hweibull(x, 0.7306,36.4888)}

wei_s=ggplot(data.frame(x = c(1, 60)), aes(x = x))+
        stat_function(fun = wei, size=1.5,col="deeppink")+
theme_bw()+
ggtitle("SURVIVAL FUNCTION")+
  xlab("Time(months)") + ylab("Survival Probabilities") +
theme(plot.title = element_text(color = "black", size = 20,family="Comic Sans MS",hjust=0.5),
      text=element_text(size = 10, family="Comic Sans MS"),
              axis.text.x=element_text(colour="black", size = 10),
              axis.text.y=element_text(colour="black", size = 10))
wei_s

wei_h=ggplot(data.frame(x = c(1, 60)), aes(x = x))+
        stat_function(fun = hwei, size=1.5,col="dodgerblue3")+
theme_bw()+
ggtitle("HAZARD FUNCTION")+
  xlab("Time(months)") + ylab("Hazard Rate") +
theme(plot.title = element_text(color = "black", size = 20,family="Comic Sans MS",hjust=0.5),
      text=element_text(size = 10, family="Comic Sans MS"),
              axis.text.x=element_text(colour="black", size = 10),
              axis.text.y=element_text(colour="black", size = 10))
wei_h

grid.arrange(wei_s,wei_h, ncol=2)


------------------------------------------------------------------------------
#GAMMA
fit_gam <- flexsurvreg(Surv(time, censor) ~ 1,
                   dist = "gamma", data =dat1 )
ggflexsurvplot(fit_gam)
summary(fit_gam)

gam <- function(x) {1-pgamma(x,0.69547,0.01759)}
hgam <- function(x) {hgamma(x,0.69547,0.01759)}

gam_s=ggplot(data.frame(x = c(1, 60)), aes(x = x))+
        stat_function(fun = gam, size=1.5,col="deeppink")+
theme_bw()+
ggtitle("SURVIVAL FUNCTION")+
  xlab("Time(months)") + ylab("Survival Probabilities") +
theme(plot.title = element_text(color = "black", size = 20,family="Comic Sans MS",hjust=0.5),
      text=element_text(size = 10, family="Comic Sans MS"),
              axis.text.x=element_text(colour="black", size = 10),
              axis.text.y=element_text(colour="black", size = 10))
gam_s

gam_h=ggplot(data.frame(x = c(1, 60)), aes(x = x))+
        stat_function(fun = hgam, size=1.5,col="dodgerblue3")+
theme_bw()+
ggtitle("HAZARD FUNCTION")+
  xlab("Time(months)") + ylab("Hazard Rate") +
theme(plot.title = element_text(color = "black", size = 20,family="Comic Sans MS",hjust=0.5),
      text=element_text(size = 10, family="Comic Sans MS"),
              axis.text.x=element_text(colour="black", size = 10),
              axis.text.y=element_text(colour="black", size = 10))
gam_h

grid.arrange(gam_s,gam_h, ncol=2)
------------------------------------------------------------------------------
#LOGNORMAL
fit_ln <- flexsurvreg(Surv(time, censor) ~ 1,
                   dist = "lognormal", data =dat2 )
ggflexsurvplot(fit_ln)

lno <- function(x) {1-plnorm(x, 2.981, 1.74)}
hlno <- function(x) {hlnorm(x, 2.981, 1.74)}

lno_s=ggplot(data.frame(x = c(1, 60)), aes(x = x))+
        stat_function(fun = lno, size=1.5,col="deeppink")+
theme_bw()+
ggtitle("SURVIVAL FUNCTION")+
  xlab("Time(months)") + ylab("Survival Probabilities") +
theme(plot.title = element_text(color = "black", size = 20,family="Comic Sans MS",hjust=0.5),
      text=element_text(size = 10, family="Comic Sans MS"),
              axis.text.x=element_text(colour="black", size = 10),
              axis.text.y=element_text(colour="black", size = 10))
lno_s

lno_h=ggplot(data.frame(x = c(1, 60)), aes(x = x))+
        stat_function(fun = hlno, size=1.5,col="dodgerblue3")+
theme_bw()+
ggtitle("HAZARD FUNCTION")+
  xlab("Time(months)") + ylab("Hazard Rate") +
theme(plot.title = element_text(color = "black", size = 20,family="Comic Sans MS",hjust=0.5),
      text=element_text(size = 10, family="Comic Sans MS"),
              axis.text.x=element_text(colour="black", size = 10),
              axis.text.y=element_text(colour="black", size = 10))
lno_h

grid.arrange(lno_s,lno_h, ncol=2)
-----------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
#ALL DISTRIBUTIONS


autoplot(fit,xlab="Time(months)",ylab="Survival Probabilities",surv.colour="deeppink",censor.colour="deeppink",surv.size=1.5)+
theme_bw()+
labs(title = "SURVIVAL CURVES")+
theme(plot.title = element_text(color = "black", size = 20,family="Chonkies",hjust=0.5),
      text=element_text(size = 10, family="Comic Sans MS"))+
stat_function(fun=expo, size=1,aes(colour = "Exponential")) +
stat_function(fun=wei, size=1,aes(colour = "Weibull")) +
stat_function(fun=gam, size=1,aes(colour = "Gamma")) +
stat_function(fun=lno, size=1,aes(colour = "Lognormal")) +
scale_colour_brewer(palette="Blues")+
labs(colour = "Distributions")+
    xlim(1,60)



----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------


fitg <- fitdist(p, "gamma")
summary(fitg)
plot(fitg)

fitw <- fitdist(p, "weibull")
summary(fitw)
plot(fitw)

fitln <- fitdist(p, "lnorm")
summary(fitln)
plot(fitln)