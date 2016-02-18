
x<-c(12, 2, 6, 2, 19, 5, 34, 4, 1, 4, 8, 7, 1, 21, 6, 11, 8, 28, 6, 4, 5, 1, 18, 9, 5, 1, 21, 1, 1, 5, 3, 14, 5, 3, 4, 5, 1, 3, 16, 2)
xcs <- cumsum(x)
freq15 <- ceiling(xcs/15)
xi <- tabulate(freq15)        # arrivals per 15s chunk
obs <- table(xi)              # number of 15s chunks with 0,1,2,3,4 arrivals


lambda = seq(from=.2, to=4, length=100)
a = 1
b = 9999
as = a+sum(xi)
bs=1/((1/b)+length(xi)) ## n = length(xi)

prior = dgamma(lambda, shape = a, scale = b)
post = dgamma(lambda, shape = as, scale = bs)

plot(lambda,post,col="blue",type="l")
lines(lambda,prior,col="red")

mean = as*bs
sd = sqrt(as*bs*bs)
mode = (as-1)*bs
median = qgamma(.5,shape=as,scale=bs)

int95 = c(qgamma(.025,shape=as,scale=bs),qgamma(.975,shape=as,scale=bs))

lambdae = seq(from=.2,to=8,length=100)        # setting lambda range from .2 to 8 to include the upper .05 quantile of 7cars/15sec
ne=.75 #bins                                  # this bin size doesnt really make sense;just trying to find reasonable parameters
ase=a+(2.75*ne)
bse=1/((1/b)+ne)


priore = dgamma(lambdae,shape=ase,scale=bse)
plot(lambdae,priore)

qgamma(.1,shape=ase,scale=bse)    # lower 5% = 1.520477
qgamma(.9,shape=ase,scale=bse)    # upper 5% = 7.211008
qgamma(.5,shape=ase,scale=bse)    # median = 3.648038
ase*bse                           # mean = 4.082789
ase                               # alpha* = 3.0625
bse                               # beta* = 1.333156
