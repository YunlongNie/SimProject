rm(list=ls())
source("Data_generation.R")
source("TrueEffect.R")
source("estimator.R")

#ls()
N = 5
n=1000; r = 3; gamma = 0.3; beta0 = -1.25; lambda1 = 2; alpha0 = -0.75 ; lambda2= 0;pho=0.7

source("mainpart.R")
save(cov.rate,res,mse, file="temp.Rdata")

#colMeans((res-causal)^2)