# r=c(4,6,9,12,15,18)
# files = list.files(paste0("data/",r,"/"))
# 
# estT=do.call(rbind,lapply(r,function(r){
#  files = list.files(paste0("data/",r,"/"))
# est=do.call(rbind,lapply(files,function(file)
#  
#  {
#  load(paste0("data/",r,"/",file))
#  data.frame(est,g2=g2)
# })
# )
# return(est)
# }
# ))

names = list.files("data/")
files=names[grep("999",names)]
estT=do.call(rbind,lapply(files,function(file)
  
{
  
  load(paste0("data/",file))
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
#res.melt$variable
require(ggplot2)
#head(resMerge)
save(resMerge,file="resMerge.Rdata")

load("Datatemp/la0.Rdata")


names(RMSEt) = c("S0","B0","P","B1","S1","S2","B2","k","n")
require(reshape2)
subest=melt(RMSEt,id=c("k","n"))

head(subset)
subest$k = paste0("k=",subest$k)
subest$n = paste0("n=",subset$n)
require(ggplot2)
ggplot.temp=ggplot(data=subest) +
  facet_wrap(n~k,scales = "free_y")+theme(legend.position = "none") +
  xlab("")+ylab("RRMSE")
plotsave=ggplot.temp +
  geom_text(aes(y=value,x=variable,label=variable,
                color=factor(variable)),alpha=1,size=3,angle=15,
            position = position_dodge(width=0.2))


figName= "SimLast1"
ggsave(plot=plotsave,filename=paste0("~/Dropbox/UBC/Thesis/",figName,".pdf"),width=10,height=6)  
capName="RRMSE of estimates $\\widehat \\Delta_{S0},\\widehat \\Delta_{P},
\\widehat \\Delta_{B0},\\widehat \\Delta_{S1},
\\widehat \\Delta_{B1},\\widehat \\Delta_{S2},
,\\widehat \\Delta_{B2}$ based on $\\rho=0.3, \\gamma_0 = \\text{logit(0.2)},\\gamma_1=\\gamma_2=1$ and $\\lambda_1=\\lambda_2=0$
Each panel is simulated under different values of number of confouder blocks, $k$, and sample size, $n$. "
source("~/Dropbox/UBC/Thesis/Simulation_Thesis/printFigure.R")
print.figure(writeto=paste0("~/Dropbox/UBC/Thesis/",figName,".tex"),filename=figName,placement="h",
             caption=capName,label=figName)


load("Datatemp/la2.Rdata")
names(RMSEt) = c("S0","B0","P","B1","S1","S2","B2","k","n")
require(reshape2)
subest=melt(RMSEt,id=c("k","n"))

head(subset)
subest$k = paste0("k=",subest$k)
subest$n = paste0("n=",subset$n)
require(ggplot2)
ggplot.temp=ggplot(data=subest) +
  facet_wrap(n~k,scales = "free_y")+theme(legend.position = "none") +
  xlab("")+ylab("RRMSE")
plotsave=ggplot.temp +
  geom_text(aes(y=value,x=variable,label=variable,
                color=factor(variable)),alpha=1,size=3,angle=15,
            position = position_dodge(width=0.2))


figName= "SimLast2"
ggsave(plot=plotsave,filename=paste0("~/Dropbox/UBC/Thesis/",figName,".pdf"),width=10,height=6)  
capName="RRMSE of estimates $\\widehat \\Delta_{S0},\\widehat \\Delta_{P},
\\widehat \\Delta_{B0},\\widehat \\Delta_{S1},
\\widehat \\Delta_{B1},\\widehat \\Delta_{S2},
,\\widehat \\Delta_{B2}$ based on $\\rho=0.3, \\gamma_0 = \\text{logit(0.2)},\\gamma_1=\\gamma_2=1$ and $\\lambda_1=\\lambda_2=2$
Each panel is simulated under different values of number of confouder blocks, $k$, and sample size, $n$. "
source("~/Dropbox/UBC/Thesis/Simulation_Thesis/printFigure.R")
print.figure(writeto=paste0("~/Dropbox/UBC/Thesis/",figName,".tex"),filename=figName,placement="h",
             caption=capName,label=figName)