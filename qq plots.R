data=read.table(file.choose(),header=T,sep=",")
data=data[-1,-4]
data=subset(data,group==1)
data2=subset(data,censor==1)

with(data,qqPlot(time,dist="lnorm",add.line = TRUE,points.col = "dodgerblue",main="Lognormal Q-Q Plot"))
with(data,qqPlot(time,dist="exp",add.line = TRUE,points.col = "dodgerblue",estimate.params = TRUE,main="Exponential Q-Q Plot"))
with(data,qqPlot(time,dist="weibull",add.line = TRUE,points.col = "deeppink",estimate.params = TRUE,main="Weibull Q-Q Plot"))
with(data,qqPlot(time,dist="gamma",add.line = TRUE,points.col = "deeppink",estimate.params = TRUE,main="Gamma Q-Q Plot"))




with(data, 
    qqPlotCensored(time,censor, dist = "lnorm",censoring.side = "right",estimate.params = TRUE,
      prob.method = "kaplan-meier", points.col = "deeppink", add.line = TRUE,
      main = paste("Lognormal Q-Q Plot", 
          "Based on Kaplan-Meier Plotting Positions", sep = "\n")))


  with(data, 
    qqPlotCensored(time,censor, dist = "exp",censoring.side = "right",param.list=list(rate=0.06208719),
      prob.method = "kaplan-meier", points.col = "deeppink", add.line = TRUE,
      main = paste("Exponential Q-Q Plot", 
          "Based on Kaplan-Meier Plotting Positions", sep = "\n")))

with(data, 
    qqPlotCensored(time,censor, dist = "gamma",censoring.side = "right",estimate.params = TRUE,
      prob.method = "kaplan-meier", points.col = "dodgerblue", add.line = TRUE,
      main = paste("Gamma Q-Q Plot", 
          "Based on Kaplan-Meier Plotting Positions", sep = "\n")))

with(data, 
    qqPlotCensored(time,censor, dist = "weibull",censoring.side = "right",param.list=list(scale=16.50129,shape=1.063046),
      prob.method = "kaplan-meier", points.col = "dodgerblue", add.line = TRUE,
      main = paste("Weibull Q-Q Plot", 
          "Based on Kaplan-Meier Plotting Positions", sep = "\n")))
---------------------------------------------------------------------------------------------------------------------------------------
 distChooseCensored(time,censor, method = "sf", censoring.side="right",
    choices = c("gamma","lnorm"))


data1=read
time=data$time