lambdae = seq(from=.2,to=8,length=100)
ne=1 #bins
ase=a+(3.75*ne)     #messing with alpha/betas based on a mean of 3.75 assuming mean is close to median
bse=3.75/ase


priore = dgamma(lambdae,shape=ase,scale=bse)
plot(lambdae,priore)

qgamma(.1,shape=ase,scale=bse)   #checking quantiles       
qgamma(.9,shape=ase,scale=bse)
