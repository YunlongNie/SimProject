filename = paste0('EstN',N,'n',nobs)
print(filename)
require(boot)
source("TrueEffect.R")
Delta=causal.est(gamma0=g0,gamma1=g1,gamma2=g2)

source("DatGeneration2.R")
source("Fun.R")
estimates = as.data.frame(do.call(rbind,lapply(1:N,function(i) {
  Dat=DatGen(gamma0=g0,gamma1=g1,gamma2=g2,n=nobs)
  print(i)
  simFun(Dat)
})
)
)

colMeans((estimates-Delta)^2)

#save(estimates,g0,g1,g2,Delta,file=paste0(filename,'.Rdata'))

#file.copy("temp.R", paste0(filename,".txt"))