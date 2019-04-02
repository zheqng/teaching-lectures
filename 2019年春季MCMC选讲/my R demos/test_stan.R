rm(list=ls())
library(tidyr) 
library(rstan) 
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(loo)
library(ggplot2)
library(gridExtra)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))
library(shinystan)
library(MASS)
source('stan_utility.R')
SEED <- 48927 # set random seed for reproducability
setwd('/media/zheqng/Seagate Backup Plus Drive/zheqng@nwu/文档/teaching lectures/2019年春季MCMC选讲/my R demos/')

d_bern <- list(N = 10, y = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 0))
fit_bern <- stan(file = 'bern.stan', data = d_bern, seed = SEED)
monitor(fit_bern, probs = c(0.1, 0.5, .9))

d_kilpis <- read.delim('kilpisjarvi-summer-temp.csv', sep = ';')
d_lin <-list(N = nrow(d_kilpis),
             x = d_kilpis$year,
             xpred = 2016,
             y = d_kilpis[,5])
ggplot() +
  geom_point(aes(x, y), data = data.frame(d_lin), size = 1) +
  labs(y = 'Summer temp. @Kilpisjärvi', x= "Year") +
  guides(linetype = F)



d_lin_priors <- c(list(
  pmualpha = mean(unlist(d_kilpis[,5])), # centered
  psalpha = 100, # weakly informative
  pmubeta = 0, # a priori incr. and decr. as likely
  psbeta = (.1--.1)/6), # avg temp prob does does not incr. more than a degree per 10 years
  d_lin)

fit_lin <- stan(file = 'lin.stan', data = d_lin_priors, seed = SEED)
monitor(fit_lin, probs = c(0.1, 0.5, 0.9))
check_hmc_diagnostics(fit_lin)


y = mvrnorm(10,c(0,0),diag(2))
data=list(y=y,N=10)
parameters=c("theta")
fit_bi_norm<-stan(file = 'bi_norm.stan',data = data, pars = parameters,seed = SEED)
monitor(fit_bi_norm,probs = c(0.1,0.5,0.9))


library('mvtnorm')
#metropolis algorithm
t1 <- -2.5
t2 <- 2.5
#' Number of iterations.
M <- 5000

#' Insert your own Metropolis sampling here
# Allocate memory for the sample
tt <- matrix(rep(0, 2*M), ncol = 2)
tt[1,] <- c(t1, t2)  
for(i in 2:M){
  Y = mvrnorm(1,tt[i-1,],diag(2))
  rho = dmvnorm(Y,c(0,0),diag(2)*0.8)/dmvnorm(tt[i-1,],c(0,0),diag(2)*0.8)
  tt[i,] = tt[i-1,] + (Y - tt[i-1,])*(runif(1)<rho)
}
