rho = 0.7
require(boot)
g0 = logit(0.2)
g1=1
g2=1
la1=2
la2=2
mE=0.001
robs=6
K = c(2,4,8)
n =c(1000,3000)
N=100
require(BayDR)
Delta=as.data.frame(do.call(rbind,lapply(K,function(k)
  c(k=k,Est=CausalEst(K=k,r=robs,rho=rho,gamma0=g0,gamma1=g1,gamma2=g2,lambda1=la1,lambda2=la2,mc.error=mE))
)
))
for(nobs in n)
{
  source("mainPart.R")
}
