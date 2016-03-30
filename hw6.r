


### data stuff
s=c(3.74,4.61,4,4.67,4.87,5.12,4.52,5.29,5.74,5.48)
b=c(5.44,6.88,5.37,5.44,5.03,6.48,3.89,5.85,6.85,7.16)
sb=mean(s)
bb=mean(b)
n=length(s)

### initializing parms
mu0_s = 0
k0_s = 0
a0_s = -.5
b0_s = Inf

mu0_b = 0
k0_b = 0
a0_b = -.5
b0_b = Inf

### updating parms
mu1_s = (k0_s*mu0_s + n*sb) / (k0_s+n)
k1_s = k0_s + n
a1_s = a0_s + n/2
b1_s = 1 / ((1/b0_s) + .5*sum((s-sb)^2) + ((k0_s*n*((sb-mu0_s)^2))/2*(k0_s+n)))

mu1_b = (k0_b*mu0_b + n*bb) / (k0_b+n)
k1_b = k0_b + n
a1_b = a0_b + n/2
b1_b = 1 / ((1/b0_b) + .5*sum((b-bb)^2) + ((k0_b*n*((bb-mu0_b)^2))/2*(k0_b+n)))


### credible intervals
ci90theta_s=c(mu1_s+qt(0.5,2*a1_s)/sqrt(k1_s*a1_s*b1_s),
              mu1_s+qt(0.95,2*a1_s)/sqrt(k1_s*a1_s*b1_s))
ci90theta_b=c(mu1_b+qt(0.5,2*a1_b)/sqrt(k1_b*a1_b*b1_b),
              mu1_b+qt(0.95,2*a1_b)/sqrt(k1_b*a1_b*b1_b))

ci90rho_s=c(qgamma(0.5,a1_s,scale=b1_s),
            qgamma(0.95,a1_s,scale=b1_s))
ci90rho_b=c(qgamma(0.5,a1_b,scale=b1_b),
            qgamma(0.95,a1_b,scale=b1_b))


### generating MC samples
mcrho_s=rgamma(10000,a1_s,scale=b1_s)
mctheta_s=rnorm(10000,sb,1/sqrt(mcrho_s*n))
mcrho_b=rgamma(10000,a1_b,scale=b1_b)
mctheta_b=rnorm(10000,bb,1/sqrt(mcrho_b*n))


### credible intervals based on MC samples
ci90mctheta_s=quantile(mctheta_s,c(.5,.95))
ci90mctheta_b=quantile(mctheta_b,c(.5,.95))
ci90mcrho_s=quantile(mcrho_s,c(.5,.95))
ci90mcrho_b=quantile(mcrho_b,c(.5,.95))


### updating parms for problem 2
mu2_s = (k0_s*mu0_s + n*sb) / (k0_s+n)
k2_s = k0_s + n
a2_s = a0_s + n/2
b2_s = 1 / ((1/b0_s) + .5*sum((s-sb)^2) + ((k0_s*n*((sb-mu0_s)^2))/2*(k0_s+n)))

mu2_b = (k0_b*mu0_b + n*bb) / (k0_b+n)
k2_b = k0_b + n
a2_b = a0_b + n/2
b2_b = 1 / ((1/b0_b) + .5*sum((b-bb)^2) + ((k0_b*n*((bb-mu0_b)^2))/2*(k0_b+n)))




