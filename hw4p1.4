
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

mean
sd
mode
median
int95
