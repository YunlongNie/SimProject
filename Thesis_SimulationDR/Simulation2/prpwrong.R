
n=500
source("Simulation2/DatGeneration2.R")
source("Simulation2/TrueEst.R")
source("estimator.R")
require(boot)
#ls()
N = 500
r=4;pho=0.7;
alpha2 = 2;alpha1=0;alpha3=2 
gamma0 = -1.386294; gamma1 = 1; gamma2 = 1; lambda1 = 0;lambda2=0

source("MainPartprp.R")
save(cov.rate,res,mse, file=paste0("prpW",n,"_",N,".Rdata"))

#rm(list=ls())
