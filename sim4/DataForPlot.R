
names = list.files(paste0("data/la",la))
#files=names[grep("999",names)]
files=names
estT=do.call(rbind,lapply(files,function(file)
  
{
  
  load(paste0("data/la",la,"/",file))
  data.frame(est,n=as.numeric(substr(file,6,9)))
})
)

Delta$r =Delta$k*6
require(plyr)

estT=join(estT,Delta,by="r")

est.col = 1:8  # see head estT2 to make sure selected col number 
# dr      sold      pold      bold       snew      pnew       bnew
res=as.data.frame(do.call(rbind,dlply(estT,.(k,n),function(x)
{
  
  do.call(c,c(sqrt(colMeans((x[,est.col] - x["Est"][1,])^2))/x["Est"][1,],unique(x["k"]),unique(x["n"])))
}
)
))
names(res)[1:(length(est.col))] = c("D","S0","P","B0","S1","B1","S2","B2")

require(reshape2) 
res.melt=melt(res,id=c("k","n"))
subest = res.melt
subest$k = paste0("k=",subest$k)
subest$n = paste0("n=",subest$n)

save(subest,file=paste0("subLa",la,".Rdata"))