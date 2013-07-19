library(reshape2)
cov.t = list()
res.t  = list()
mse.t = list()

for (i in 1:length(input.file)){
  load(paste0("Simulation2/",input.file[i]))
  cov.t[[i]]=cov.rate
  res.t[[i]]=res
  mse.t[[i]] = sqrt(mse)
}
# load("Simulation2/outW100_500.Rdata")
# cov.t[[1]]=cov.rate
# res.t[[1]]=res
# mse.t[[1]] = sqrt(mse)
# 
# load("Simulation2/outW200_500.Rdata")
# cov.t[[2]]=cov.rate
# res.t[[2]]=res
# mse.t[[2]] = sqrt(mse)
# 
# load("Simulation2/outW500_500.Rdata")
# cov.t[[3]]=cov.rate
# res.t[[3]]=res
# mse.t[[3]] = sqrt(mse)

#######
mean.res = data.frame(n=obs.n,do.call(rbind,lapply(res.t,function(x) {apply(x[,c("reg","prp","dr")],2,mean)})))
mean.res

median.res = data.frame(n=obs.n,do.call(rbind,lapply(res.t,function(x) {apply(x[,c("reg","prp","dr")],2,median)})))
median.res

mse.res = data.frame(n=obs.n,do.call(rbind,mse.t))

melt.temp=melt(mean.res,id.vars=1)
head(melt.temp)
melt.temp=melt.temp[with(melt.temp,order(n)),]
names(melt.temp) = c("obs","estimate","mean")
melt.mean=melt.temp

melt.temp=melt(median.res,id.vars=1)
head(melt.temp)
melt.temp=melt.temp[with(melt.temp,order(n)),]
names(melt.temp) = c("obs","estimate","median")
melt.median=melt.temp

melt.temp=melt(round(mse.res,4),id.vars=1)
head(melt.temp)
melt.temp=melt.temp[with(melt.temp,order(n)),]
names(melt.temp) = c("obs","estimate","mse")
melt.mse=melt.temp


library(plyr)
res.table=join(join(melt.mean,melt.median,by=c("obs","estimate")),melt.mse,by=c("obs","estimate"))

# output
library(xtable)

print.xtable(xtable(format(res.table[,c(2,1,3:5)],digits=4),caption="res"),include.rownames=FALSE)
