#one dimension Metropolis Hastings
a = 2.7;b=6.3;c=2.669   #initial values
Nsim=5000
X =rep(runif(1),Nsim) #initialize the chains
for (i in 2:Nsim){
  Y=runif(1)
  rho=dbeta(Y,a,b)/dbeta(X[i-1],a,b)
  X[i]=X[i-1] + (Y - X[i-1])*(runif(1)<rho)
}
hist(X,freq = FALSE)
x_axis = seq(0,0.8,length.out = 100)
lines(x_axis,dbeta(x_axis,a,b))

library('coda')
plot(mcmc(X))
effectiveSize(mcmc(X))


cumuplot(mcmc( X ) )
list( theta = X )


heidel.diag (mcmc( X) )
print(geweke.diag (mcmc( X[Nsim/2:Nsim] ) ))


mass = 0*(1:Nsim)
theta = X[1]
for (i in 2:Nsim){
  theta = sort(c(theta,X[i]))
  mass[i] = sum( (theta[2:i] - theta[1:(i-1)])*dbeta(theta[1:(i-1)], a,b))
}

plot(mass,type="l",ylab="mass",col="sienna4",lwd=2)
par(new=T);plot(X,pch=5,axes=F,cex=.3,col="steelblue2",ylab="")
