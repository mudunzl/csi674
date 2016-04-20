




diets <- read.csv("C:/Users/tim/Desktop/diets.txt", header=FALSE)

sd=0
dbar=0
n=length(diets[,1])

for (i in 1:4) {
  sd[i] = sd(diets[,i])
  dbar[i] = mean(diets[,i])
}

sig = mean(sd)
mu = mean(dbar)
tau = sd(dbar)

mu1 = (mu/tau^2 + sum(diets[,1])/sig^2) / (1/tau^2 + n/sig^2)
mu2 = (mu/tau^2 + sum(diets[,2])/sig^2) / (1/tau^2 + n/sig^2)
mu3 = (mu/tau^2 + sum(diets[,3])/sig^2) / (1/tau^2 + n/sig^2)
mu4 = (mu/tau^2 + sum(diets[,4])/sig^2) / (1/tau^2 + n/sig^2)

tau1 = (1/tau^2 + n/sig^2)^-.5
tau2 = (1/tau^2 + n/sig^2)^-.5
tau3 = (1/tau^2 + n/sig^2)^-.5
tau4 = (1/tau^2 + n/sig^2)^-.5

qnorm(c(.025,.975),mu1,tau1)
qnorm(c(.025,.975),mu2,tau2)
qnorm(c(.025,.975),mu3,tau3)
qnorm(c(.025,.975),mu4,tau4)
