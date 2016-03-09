### Problem 1

library(MCMCpack)
library(Rgraphviz,graph)

#call times in seconds
x1=c( 640, 654, 1086, 1339, 1518, 1633, 1874, 2037, 2169,
     2478, 2908, 2987, 3245, 3266, 3301)

#inter call times 
dx1=diff(x)

a10=4
b10=.002
t1=seq(length=100,from=0.1,to=1)

a11=a0+length(dx)
b11=b0/(1+(b0*sum(dx)))


prior1=dinvgamma(t,a10,1/b10)
post1=dinvgamma(t,a11,1/b11)

plot(t,prior,type="l")
lines(t,post)

### Problem 2


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




