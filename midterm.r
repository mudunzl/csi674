
library(stats)
library(VGAM)
library(MCMCpack)
library(Rgraphviz,graph)

### Problem 1

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


prior1=dinvgamma(t1,a10,1/b10)
post1=dinvgamma(t1,a11,1/b11)

plot(t1,prior1,type="l")
lines(t1,post1)

### Problem 2

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

### Problem 4

rt1i=rbinom(1000,70,19/70)
rt2i=rbinom(1000,70,30/70)
dti=rt2i-rt1i
pt1l=sum(dti>0)/length(dti)

### Problem 5

a50 = 1+19
b50 = 1+51
t5 = 0:50

pred5 = dbetabinom.ab(t5,50,a50,b50)
predb5 = dbinom(t5,50,19/70)
barplot(rbind(pred5,predb5),names=t5,beside=TRUE,legend=c("Predictive","Bin(50,19/70)"),main="Predictive vs Binomial on 50 steroid treated patients",xlab="Relapses")

### Problem 6



### Problem 7



### Problem 8

x8 = c(90, 76, 90, 64, 86, 51, 72, 90, 95, 78)

#kernal estimation along with a normal curve with mean=sample mean and sd=sample sd
plot(density(x8))
lines(dnorm(0:120,mean(x8),sd(x8)))

#qqplot
qqnorm(x8,xlim=c(-3,3),ylim=c(50,120))
qqline(x8)

### Problem 9



