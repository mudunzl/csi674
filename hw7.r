library(rjags)
library(R2jags)
library(coda)
library(superdiag)


set.seed(1)

### data stuff
s=c(3.74,4.61,4,4.67,4.87,5.12,4.52,5.29,5.74,5.48)
b=c(5.44,6.88,5.37,5.44,5.03,6.48,3.89,5.85,6.85,7.16)
sb=mean(s)
bb=mean(b)
n=length(s)
sss=sum((s-mean(s))^2)
ssb=sum((b-mean(b))^2)


mu0 = 6
tau0 = 1.5
a0 = 4.5
b0 = .19

a1=a0+(n/2)
a11=a1+(n/2)

b1=1/( (1/b0) + (.5*ssb) )
b11=1/( (1/b1) + (.5*sss) )


thetag_s = sb
thetag_b = bb
sigg = sd(c(s,b))
rhog = 1/sigg[1]^2

for (k in 2:10000) {
  mu1_s <- (mu0/tau0^2 + n*sb*rhog[k-1])/(1/tau0^2+n*rhog[k-1])
  mu1_b <- (mu0/tau0^2 + n*bb*rhog[k-1])/(1/tau0^2+n*rhog[k-1])
  tau1 <- 1/sqrt(1/tau0^2+n*rhog[k-1])
  thetag_s[k] <-  rnorm(1,mean=mu1_s,sd=tau1)
  thetag_b[k] <-  rnorm(1,mean=mu1_b,sd=tau1)
  b11 <- 1/(1/b0 + 0.5*sum((s-thetag_s[k])^2)+ 0.5*sum((b-thetag_b[k])^2))
  rhog[k] <- rgamma(1,shape=a11,scale=b11)
  sigg[k] <- 1/sqrt(rhog[k])
}


quantile(thetag_s,c(0.025,0.975))
quantile(thetag_b,c(0.025,0.975))
quantile(sigg,c(0.025,0.975))
quantile(thetag_b-thetag_s,c(0.025,0.975))

# quantile(thetag_s,c(0.05,0.95))
# quantile(thetag_b,c(0.05,0.95))
# quantile(sigg,c(0.05,0.95))
# quantile(thetag_b-thetag_s,c(0.05,0.95))
# quantile(rhog,c(0.05,0.95))

plot(1:10000,thetag_b-thetag_s,main="Trace Plot of Theta_b - Theta_s",xlab="Iteration",ylab="Delta")

acf=acf(thetag_b-thetag_s,type="correlation")

effectiveSize(thetag_b-thetag_s)
