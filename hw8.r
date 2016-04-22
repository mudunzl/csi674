diets <- read.csv("C:/Users/tim/Desktop/diets.txt", header=FALSE)

# initializing, couldnt get the for loop to work otherwise...
sd=0
dbar=0
mu1=0
tau1=0
delta1=NULL

n=length(diets[,1])

for (i in 1:4) {
  sd[i] = sd(diets[,i])
  dbar[i] = mean(diets[,i])
}

# these prior hyperparms were given in the problem
sig = mean(sd)
mu = mean(dbar)
tau = sd(dbar)

# N-N updating
for (i in 1:4) {
  mu1[i] = (mu/tau^2 + sum(diets[,i])/sig^2) / (1/tau^2 + n/sig^2)
  tau1[i] = (1/tau^2 + n/sig^2)^-.5
  qnorm(c(.025,.975),mu1[i],tau1[i])
}

# 95% credible intervals
qnorm(c(.025,.975),mu1[1],tau1[1])
qnorm(c(.025,.975),mu1[2],tau1[2])
qnorm(c(.025,.975),mu1[3],tau1[3])
qnorm(c(.025,.975),mu1[4],tau1[4])

# practicing loops but this is bootleg
for (i in 1:3) {
  for (j in i+1:4) {
    delta1 <- append(delta1,abs(mu1[i]-mu1[j]))
  }
}
delta1 <- delta1[!is.na(delta1)]

# always sum variance when sum/diff normals
taud = sqrt(2*(tau1[1]^2))

# 95% ci's for the delta weightgains
qnorm(c(.025,.975),delta1[1],taud)
qnorm(c(.025,.975),delta1[2],taud)
qnorm(c(.025,.975),delta1[3],taud)
qnorm(c(.025,.975),delta1[4],taud)
qnorm(c(.025,.975),delta1[5],taud)
qnorm(c(.025,.975),delta1[6],taud)
