## Initializing some stuff

a=1
b=3

p=seq(from=0,to=1,length=100)

################
## Problem 1a ##
################

meanprior=a/(a+b)
sdprior=sqrt(a*b/(((a+b)^2)*(a+b+1)))
ci95prior=c(qbeta(.025,a,b),qbeta(.975,a,b))

################
## Problem 1b ##
################

prior=dbeta(p,a,b)
as=a+19
bs=b+47-19
post2009=dbeta(p,as,bs)
nlik2009=dbeta(p,19,47-19)

plot(p,prior,type="l",ylim=c(-.2,8.5),col="green",main="2009 Distributions for 19/47")
lines(p,post2009,type="l",ylim=c(0,8.5),col="red")
lines(p,nlik2009,type="l",ylim=c(0,8.5),col="blue")
legend(.7,6,c("Prior","Post 2009","Normalized Likelihood"),col=c("green","red","blue"),lty=c(1,1,1))

mean2009=as/(as+bs)
sd2009=sqrt(as*bs/(((as+bs)^2)*(as+bs+1)))
ci952009=c(qbeta(.025,as,bs),qbeta(.975,as,bs))

################
## Problem 1c ##
################

prior2011=post2009
ass=as+20
bss=bs+47-20
post2011=dbeta(p,ass,bss)
nlik2011=dbeta(p,20,47-20)

plot(p,prior2011,type="l",ylim=c(-.2,8.5),col="green",main="2011 Distributions for 20/47")
lines(p,post2011,type="l",ylim=c(0,8.5),col="red")
lines(p,nlik2011,type="l",ylim=c(0,8.5),col="blue")
legend(.7,6,c("Prior","Post 2011","Normalized Likelihood"),col=c("green","red","blue"),lty=c(1,1,1))

mean2011=ass/(ass+bss)
sd2011=sqrt(ass*bss/(((ass+bss)^2)*(ass+bss+1)))
ci952011=c(qbeta(.025,ass,bss),qbeta(.975,ass,bss))

################
## Problem 2a ##
################


y=0:47

#predbb=dbetabinom.ab(y,47,a+(47*.25),b+(47-(47*.25))) wasnt sure at first keeping this incase
predbb=dbetabinom.ab(y,47,a,b)
predb=dbinom(y,47,meanprior)

barplot(rbind(predbb,predb),names=y,beside=TRUE,main="Predictive Prior vs Binomial Prior",legend=c("Predictive","Binomial"))

################
## Problem 2b ##
################

predbb2009=dbetabinom.ab(y,47,as,bs)
predb2009=dbinom(y,47,mean2009)

barplot(rbind(predbb2009,predb2009),names=y,beside=TRUE,main="Predictive 2009 vs Binomial 2009",legend=c("Predictive","Binomial"))






meanprior
sdprior
ci95prior
mean2009
sd2009
ci952009
mean2011
sd2011
ci952011
