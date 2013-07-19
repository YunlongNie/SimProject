#r=c(4,6,9,12)
#files = list.files(paste0("data/",r,"/"))

#estT=do.call(rbind,lapply(r,function(r){
#  files = list.files(paste0("data/",r,"/"))
#est=do.call(rbind,lapply(files,function(file)
#  
#  {
#  load(paste0("data/",r,"/",file))
#  data.frame(est,g2=g2)
#})
#)
#return(est)
#}
#))

names = list.files("ExpWrong/data/")
files=names[grep("999",names)]
estT=do.call(rbind,lapply(files,function(file)
  
{
  
  load(paste0("ExpWrong/data/",file))
  data.frame(est,g2=g2)
})
)

estT = estT[,-9] # two ph is the same 
load("ParaSet.Rdata")
subset = subset(Paraset,obs==500)
source("TrueEffect.R")
TrueEst=as.data.frame(t(apply(subset,1,function(x) c(g2=as.vector(x[2]),Delta=causal.est(gamma0=x[4],gamma1=x[3],gamma2=x[2])))))

require(plyr)

estT2=join(estT,TrueEst,by="g2")
head(estT2)
est.col = 1:9  # see head estT2 to make sure selected col number 
# dr      sold      pold      bold       snew      pnew       bnew
res=as.data.frame(do.call(rbind,dlply(estT2,.(r,g2),function(x)
  {
  
  do.call(c,c(sqrt(colMeans((x[,est.col] - x["Delta"][1,])^2)),Delta=x["Delta"][1,],unique(x["r"]),unique(x["g2"])))
}
  )
))
names(res)[1:(length(est.col)+1)] = c("D","S0","P0","B0","S1","P1","B1","S2","B2","N")

require(reshape2) 
res.melt=melt(res,id=c("r","g2"))


res.sd=as.data.frame(do.call(rbind,dlply(estT2,.(r,g2),function(x)
{
  
  s.mean = colMeans((x[,est.col] - x["Delta"][1,])^2)
  s.var= apply((x[,est.col] - x["Delta"][1,])^2,2,var)/nrow(x)
  r.mean=sqrt(s.mean)
  r.var=s.var/4/s.mean
  c(qnorm(0.975)*sqrt(r.var),Delta=0,unique(x["r"]),unique(x["g2"]))
}
)
))
names(res.sd)[1:(length(est.col)+1)] = c("D","S0","P0","B0","S1","P1","B1","S2","B2","N")

#require(reshape2) 
ressd.melt=melt(res.sd,id=c("r","g2"))
names(ressd.melt)[4] = "sd"

resMerge=merge(res.melt,ressd.melt,by=1:3)
save(resMerge,file="ExpWrong/resMerge.Rdata")
#res.melt$variable
require(ggplot2)
#head(resMerge)



