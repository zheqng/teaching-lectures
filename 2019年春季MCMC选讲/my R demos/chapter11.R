rm(list = ls())
#Gibbs sampling for bivariate normal distribution
nsim=10^3*4
nsize = 10^3
# n=15;a=3;b=7
tt=matrix(rep(0,nsim*2),ncol = 2)
tt[1,]=c(2.5,2.5)              #initialize the chain
for (i in 2:(nsim/4)){
  tt[i,1]=rnorm(1,0.8*tt[i-1,2],1-0.8^2)
  tt[i,2]=rnorm(1,0.8*tt[i,1],1-0.8^2)
}
tt[nsize+1,]=c(2.5,-2.5)
for (i in (nsize+2):(2*nsize)){
  tt[i,1]=rnorm(1,0.8*tt[i-1,2],1-0.8^2)
  tt[i,2]=rnorm(1,0.8*tt[i,1],1-0.8^2)
}
tt[(2*nsize+1),]=c(-2.5,2.5)
for (i in (2*nsize+2):(3*nsize)){
  tt[i,1]=rnorm(1,0.8*tt[i-1,2],1-0.8^2)
  tt[i,2]=rnorm(1,0.8*tt[i,1],1-0.8^2)
}
tt[(3*nsize+1),]=c(-2.5,-2.5)
for (i in (3*nsize+2):(4*nsize)){
  tt[i,1]=rnorm(1,0.8*tt[i-1,2],1-0.8^2)
  tt[i,2]=rnorm(1,0.8*tt[i,1],1-0.8^2)
}
save(tt,file ="chain4.RData")
par(mfrow=c(1,2),mar=c(4,4,2,1))
hist(T1[nsim/2:nsim],nclass=16,col="grey",freq=FALSE, xlim=c(0,15),main="",xlab="X")
# curve(betabi(x,a,b,n),from=0, to=15,col="gold4",lwd=2,add=TRUE)
hist(T2[nsim/2:nsim],nclass=134,col="grey",freq=FALSE,xlim=c(0,.8),main="", xlab=expression(theta))
# curve(dbeta(x,shape1=a,shape2=b),from=0, to=.8,col="sienna",lwd=2,add=TRUE)
plot(T1,T2)
lines(T1,T2)


#Metropolis Hastings for bivariate normal distribution
#Section 6.3.1, Metropolis-Hasting definition, beta example 6.1

nsim=1000
X=rep(runif(1),nsim)  # initialize the chain
for (i in 2:nsim){
  Y=nvrnorm(1,Theta[i-1],0.2^2*diag(2))
  rho=dnorm(,c(0,0),diag(2))
    dbeta(Y,a,b)/dbeta(X[i-1],a,b)
  X[i]=X[i-1] + (Y-X[i-1])*(runif(1)<rho)
}
X11(h=3.5);plot(4500:4800,X[4500:4800],ty="l",lwd=2,xlab="Iterations",ylab="X")

S=readline(prompt="Type  <Return>   to continue : ")

dev.off()
ks.test(jitter(X),rbeta(5000,a,b))

par(mfrow=c(1,2),mar=c(2,2,1,1))
hist(X,nclass=150,col="grey",main="Metropolis-Hastings",fre=FALSE)
curve(dbeta(x,a,b),col="sienna",lwd=2,add=TRUE)
hist(rbeta(5000,a,b),nclass=150,col="grey",main="Direct Generation",fre=FALSE)
curve(dbeta(x,a,b),col="sienna",lwd=2,add=TRUE)
