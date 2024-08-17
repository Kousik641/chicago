library(gamair)
data(chicago)

summary(chicago)
mydata<-chicago[,-3]

apply(na.omit(mydata),2,
      function(variable) return (c(mean(variable),var(variable),median(variable))))

par(mfrow=c(2,3))
for(i in c(1:4,6)) 
{
  hist(mydata[,i],prob=T,breaks=10,xlab="",main=colnames(mydata)[i])
}

mydata<-mydata[,-5]
par(mfrow=c(2,2))
for(i in 2:ncol(mydata))
{
  plot(mydata[,i],mydata$death,xlab=colnames(mydata)[i],ylab="Mortality(deaths/day)",pch=i,main=paste("death vs",colnames(mydata)[i]))
}

par(mfrow=c(2,1))
plot(mydata$tmpd,mydata$death,xlab="temperature (in deg F)",ylab="Mortality(deaths/day)",main=paste("death/day vs tmpd"))
curve(130-0.28*x,col="steelblue",lwd=2,add=T)
qqnorm(residuals(lm(death~tmpd,data=mydata)),xlab=expression(epsilon))
qqline(residuals(lm(death~tmpd,data=mydata)),col="red")