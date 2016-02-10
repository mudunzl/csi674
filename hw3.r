## csi674hw3p2

## DATA
x<-c(12, 2, 6, 2, 19, 5, 34, 4, 1, 4, 8, 7, 1, 21, 6, 11, 8, 28, 6, 4, 5, 1, 18, 9, 5, 1, 21, 1, 1, 5, 3, 14, 5, 3, 4, 5, 1, 3, 16, 2)
xcs <- cumsum(x)
xint <- diff(x)



## Exponential?
eq <- qexp(ppoints(length(x)))

qqplot(eq,xint,main="Exp Q-Q Plot of Car Arrival Intervals",xlab="Theoretical Exp Quantiles",ylab="Empirical Quantiles")
lines(eq,eq*mean(xint))

## Poisson?
freq15 <- ceiling(xcs/15)
xp15 <- tabulate(freq15)        # arrivals per 15s chunk
obs <- table(xp15)              # number of 15s chunks with 0,1,2,3,4 arrivals
xrate <- mean(xp15)             # estimated Poisson rate by 40 cars in 21 15s chunks
exp <- dpois(0:4,xrate)*21      # expected arrivals for Poisson distribution

obsexp <- rbind(obs,exp)
barplot(obsexp,main="Distribution of Arrivals per 15s", xlab="# of Cars", ylab="Empirical Count of 15s Chunks", beside=TRUE, legend=c("Observed","Expected"))

## Posterior Poisson
prior <- array(1/20,20)         # uniform .05 n=20
lambdas <- seq(from=.2, to=4, by=.2)

lik <- array(1,length(lambdas))
for (i in 1:length(xp15)) {
  lik <- lik*dpois(xp15[i],lambdas) }
  
post <- prior*lik
post <- post/sum(post)

barplot(prior,names=lambdas, ylab="Probability", xlab=expression(Lambda), main=expression(paste("Prior Distribution for Arrival Rate ",Lambda)), ylim=c(0,.25))
barplot(post, names=lambdas, ylab="Probability", xlab=expression(Lambda), main=expression(paste("Posterior Distribution for Arrival Rate ",Lambda)), ylim=c(0,.25))
