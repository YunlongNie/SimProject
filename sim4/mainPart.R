filename = paste0('EstN',N,'n',nobs)
require(boot)
require(utils)
require(BayDR)
source("Fun.R")

pb = txtProgressBar(min=0,max=N,style=3)

#estimates=as.data.frame(do.call(rbind,lapply(robs,function(r) {
lapply(K,function(k) {
est = as.data.frame(do.call(rbind,lapply(1:N,function(i) {
  #print(r)
  Dat=GenDat2(n=nobs,K=k,r=robs,rho=rho,gamma0=g0,gamma1=g1,gamma2=g2,lambda1=la1,lambda2=la2)
  setTxtProgressBar(pb,i)
  return(simFun(Dat))
  
})
)
)
save(est,g0,g1,g2,Delta,file=paste0('data/',filename,'_',g2,k,'_.Rdata'))
#return(est)
}
)
#colMeans((estimates-Delta)^2)

#save(estimates,g0,g1,g2,Delta,file=paste0('data/',filename,'_',g2,'_.Rdata'))