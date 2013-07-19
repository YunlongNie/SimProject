#:rm(list=ls())
n=500
source("DatGeneration2.R")
source("TrueEst.R")
source("estimator.R")
require(boot)
#ls()
print(n)
N =500 
r=4;pho=0.7;
alpha2 = 0;alpha3=0 
gamma0 = -1.386294; gamma1 = 1; gamma2 = 1; lambda1 = 2;lambda2=2

source("MainPart.R")
save(cov.rate,res,mse, file=paste0("outW",n,"_",N,".Rdata"))
warnings()
#rm(list=ls())

#colMeans((res-causal)^2)
