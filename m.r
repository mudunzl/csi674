
library(stats)
library(VGAM)
library(MCMCpack)
library(Rgraphviz,graph)
library(actuar)

### Problem 1 (Inter Birth Times example)

#call times in seconds
x1=c( 640, 654, 1086, 1339, 1518, 1633, 1874, 2037, 2169,
      2478, 2908, 2987, 3245, 3266, 3301)

#inter call times 
dx1=diff(x1)

a10=4
b10=.002
t1=seq(length=100,from=0.1,to=1)

a11=a10+length(dx1)
b11=b10/(1+(b10*sum(dx1)))

prior1=dinvgamma(t1,shape=a10,scale=1/b10)
post1=dinvgamma(t1,shape=a11,scale=1/b11)
ciprior1 = qinvgamma(c(.025,.975),shape=a10,scale=1/b10) #actuar package qinvgamma(p,shape,rate,scale) in that order 
cipost1 = qinvgamma(c(.025,.975),shape=a11,scale=1/b11)  #so I specified scale to match with dinvgamma parameterization

#plot(t1,prior1,type="l")
#lines(t1,post1)

### Problem 2 (# events in a time period is poisson)

mean2 = 1/(b11*(a11-1))   #mean rate of posterior
lambda2 = 300/mean2      #time/rate
pofmorethan3 = ppois(3,lambda2,lower.tail=FALSE)   #P(X > 3)

### Problem 3 

t3=seq(length=50,from=.05,to=1)
prior3=array(1/50,50)
postt1=prior3*(t3^19)*((1-t3)^51)
postt1=postt1/sum(postt1)
postt2=prior3*(t3^30)*((1-t3)^40)
postt2=postt2/sum(postt2)

plot3=rbind(postt1,postt2)
barplot(plot3,beside=TRUE,names=round(t3,2),legend=c("Treatment","Placebo"),ylab="Density",xlab="Theta",main="Distribution of Relapse in Treatment vs. Placebo Groups")

### Problem 4 (Unit 4 Slide 7)

rt1i=rbinom(1000,70,19/70)
rt2i=rbinom(1000,70,30/70)
dti=rt2i-rt1i
pt1L=sum(dti>0)/length(dti)

### Problem 5

a50 = 1+19
b50 = 1+51
t5 = 0:50

pred5 = dbetabinom.ab(t5,50,a50,b50)
predb5 = dbinom(t5,50,19/70)
barplot(rbind(pred5,predb5),names=t5,beside=TRUE,legend=c("Predictive","Bin(50,19/70)"),main="Predictive vs Binomial on 50 steroid treated patients",xlab="Relapses")

### Problem 6 (HW1P4)

#   (.00044,.01575)

### Problem 7 (HW4P2 using optim() function)

gamma.fit = function(pars,p,q) {
  a7 = pars[1]
  b7 = pars[2]
  sum((q-qgamma(p,a7,b7))^2)
}
fit = optim(c(1,1),function(ab){gamma.fit(ab,c(0.1,0.5,0.9),c(5,10,20))}) 
qgamma(c(.1,.5,.9),fit$par[1],fit$par[2])

# a=3.4423754, b=0.2985795

### Problem 8 (Unit 5 slides)

x8 = c(90, 76, 90, 64, 86, 51, 72, 90, 95, 78)

#kernal estimation along with a normal curve with mean=sample mean and sd=sample sd
plot(density(x8))
lines(dnorm(0:120,mean(x8),sd(x8)))
legend(75,.015,c("Normal","Data"),col=c("red","blue"),lty=c(1,1))

#qqplot
qqnorm(x8,xlim=c(-3,3),ylim=c(50,120))
qqline(x8)

### Problem 9 (Reaction times example)

sig = 15
mu = 85
tau = 9
xbar = mean(x8)
t9=30:130
n=length(x8)
mus = ((mu/(tau^2)) + (sum(x8)/(sig^2))) / ((1/(tau^2)) + (n/(sig^2)))
taus = 1/sqrt(((1/(tau^2))+(n/(sig^2))))

prior9=dnorm(t9,85,9)
nlik9=dnorm(t9,xbar,sig/sqrt(n))
post9=dnorm(t9,mus,taus)

plot(t9,prior9,type="l",col="blue",ylim=c(-.01,.1),xlab="Weight(g) Gain in Rats",ylab="")
lines(t9,nlik9,col="red")
lines(t9,post9,col="green")
legend(100,.08,c("Prior","Normalized Likelihood","Post"),col=c("blue","red","green"),lty=c(1,1,1))

### Problem 10 (Unit 5 Slide 12 + Reaction times example)

ci1rat = qnorm(c(.025,.975),mus,sqrt(((sig^2)/1) + (taus^2)))
ci10rats = qnorm(c(.025,.975),mus,sqrt(((sig^2)/10) + (taus^2)))
ci1krats = qnorm(c(.025,.975),mus,sqrt(((sig^2)/1000) + (taus^2)))


