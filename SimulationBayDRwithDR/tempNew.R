

load("ParaSet.Rdata")
library(boot)
subset = subset(Paraset,obs==100)
for(j in 1:2)
{
x=as.numeric(subset[j,])
nobs=x[1]
g0=x[4]
g1=x[3]
g2=x[2]
N=3
print(N)
source("mainpart.R")
}
