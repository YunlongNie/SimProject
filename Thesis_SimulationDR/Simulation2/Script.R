rm(list=ls())
source("Simulation2/DatGeneration2.R")
source("Simulation2/TrueEst.R")
source("estimator.R")

#ls()
N = 100
n=1000;r=4;pho=0.7;
alpha2 = 2; 
gamma0 = -1.386294; gamma1 = 1; gamma2 = 1; lambda1 = 0;lambda2=0

source("Simulation2/mainpart.R")
save(cov.rate,res,mse, file="Simulation2/temp.Rdata")

#colMeans((res-causal)^2)