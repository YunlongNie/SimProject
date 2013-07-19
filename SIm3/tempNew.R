

load("ParaSet.Rdata")
library(boot)
subset = subset(Paraset,obs==500)
#robs =c(6,9,12)
for(j in 1:nrow(subset))
{
robs=4
N=999
x=as.numeric(subset[j,])
nobs=x[1]
g0=x[4]
g1=x[3]
g2=x[2]
source("mainpart.R")
gc()
}
