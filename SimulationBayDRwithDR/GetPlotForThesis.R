# res

load("data/EstN1000n100.Rdata")
# g0 g1 g2 estimates Delta
dir = "data/exp/" # or dir ="data/"

filenames=list.files(dir) # or filenames=list.files("data/")

res=as.data.frame(do.call(rbind,lapply(c(100,200,500),function(x){
do.call(rbind,lapply(filenames[grep(paste0("n",x,"_"),filenames)],function(name){
 load(paste0(dir,name))
 names(estimates) =c("D","S","P","B")
 c(sqrt(colMeans((estimates-Delta)^2)),N=Delta,g2=g2,n=x)
}
)
)
}
)
))
require(reshape2)
res.melt=melt(res,id=c("n","g2"))
res.melt$variable
require(ggplot2)


ggplot(data=res.melt) +geom_text(aes(y=value,x=g2,label=variable,
                                     color=factor(variable)),angle=15,
                                 position = position_dodge(width=0.2))+
  facet_grid(~n,scales = "free_x")+theme(legend.position = "none") +
  scale_size_discrete(range = c(3, 5))+xlab("lambda")+ylab("RMSE")

source("~/Dropbox/UBC/Thesis/Simulation_Thesis/printFigure.R")

### one set ###
figName= "SimulationBayDR1"
capName="Root-mean-squared error of estimates $\\widehat \\Delta_{D},\\widehat \\Delta_{S},\\widehat \\Delta_{P},\\widehat \\Delta_{B}$.
$\\widehat \\Delta_{N}=0$, so N represents the true value of $\\Delta$."
ggsave(filename=paste0("~/Dropbox/UBC/Thesis/",figName,".pdf"),width=10,height=4.5)  

print.figure(writeto="~/Dropbox/UBC/Thesis/SimulationBayDRPlot1.tex",filename=figName,placement="h",
             caption=capName,label="SimDR1")

### another set ###
figName= "SimulationBayDR2"
capName="Root-mean-squared error of estimates $\\widehat \\Delta_{D},\\widehat \\Delta_{S},\\widehat \\Delta_{P},\\widehat \\Delta_{B}$.
$\\widehat \\Delta_{N}=0$, so N represents the true value of $\\Delta$. 
The exposure-generating mechanism is updated into \\eqref{expModel2}."
ggsave(filename=paste0("~/Dropbox/UBC/Thesis/",figName,".pdf"),width=10,height=4.5)  
print.figure(writeto="~/Dropbox/UBC/Thesis/SimulationBayDRPlot2.tex",filename=figName,placement="h",
             caption=capName,label="SimDR2")








