filename = paste0('EstN',N,'n',nobs)
require(boot)
require(utils)
#file.copy("temp.R", paste0('out/',filename,".txt"))
source("TrueEffect.R")
Delta=causal.est(gamma0=g0,gamma1=g1,gamma2=g2)
source("DatGeneration2.R")
source("Fun.R")

#  print(i)
#print(N)
pb = txtProgressBar(min=0,max=N,style=3)

#estimates=as.data.frame(do.call(rbind,lapply(robs,function(r) {
lapply(robs,function(r) {
est = as.data.frame(do.call(rbind,lapply(1:N,function(i) {
  #print(r)
  Dat=DatGen(gamma0=g0,gamma1=g1,gamma2=g2,n=nobs,r=r)
 # print(i)
  #  print(N)
  setTxtProgressBar(pb,i)
  return(simFun(Dat))
  
})
)
)
save(est,g0,g1,g2,Delta,file=paste0('data/',filename,'_',g2,r,'_.Rdata'))
#return(est)
}
)
#colMeans((estimates-Delta)^2)

#save(estimates,g0,g1,g2,Delta,file=paste0('data/',filename,'_',g2,'_.Rdata'))