a=1
b=3

p=seq(from=0,to=1,length=100)

prior=dbeta(p,a,b)
as=a+19
bs=b+47-19
post2009=(gamma(as+bs)/(gamma(as)*gamma(bs)))*(p^(as-1))*((1-p)^(bs-1))

prior2011=post2009
ass=as+20
bss=bs+47-20
post2011=(gamma(ass+bss)/(gamma(ass)*gamma(bss)))*(p^(ass-1))*((1-p)^(bss-1))

plot(p,prior,type="l",ylim=c(0,8.5),col="green")
lines(p,post2009,type="l",ylim=c(0,8.5),col="red")
lines(p,post2011,type="l",ylim=c(0,8.5),col="blue")
legend(.8,6,c("Prior","Post 2009","Post 2011"),col=c("green","red","blue"),lty=c(1,1,1))


pd=seq(from=0,to=1,length=100)
y=0:47
priord=dbeta(pd,a,b)
predbb=dbetabinom.ab(y,47,1+47*.25,b+(47-(47*.25)))
barplot(predbb,names=y)
