#source("GetPlot.R")
require(ggplot2)
load("resMerge.Rdata")
estimator = c("B0","B1","B2","N")
subest= subset(resMerge,variable%in%estimator)

ggplot.temp=ggplot(data=subest) +
  facet_wrap(~r,scales = "free_x")+theme(legend.position = "none") +
  xlab("lambda")+ylab("RMSE")
plotsave=ggplot.temp +
  geom_text(aes(y=value,x=g2,label=variable,
                color=factor(variable)),alpha=1,size=4,angle=15,
            position = position_dodge(width=0.2))
# + geom_linerange(aes(ymax=value-sd,ymin=value+sd,x=g2,color=factor(variable)),position=position_dodge(width=0.2),angle=90)
  
  
figName= "SimulationBayDR3_B"

capName="Root-mean-squared error of estimates $\\widehat \\Delta_{P},\\widehat \\Delta_{B0},
\\widehat \\Delta_{B1},\\widehat \\Delta_{B2} $. $\\widehat \\Delta_{N}=0$, so N represents the true value of $\\Delta$. Each panel is simulated under different number of confounders ranging from 4 to 12. "
ggsave(plot=plotsave,filename=paste0("~/Dropbox/UBC/Thesis/",figName,".pdf"),width=10,height=6)  
source("~/Dropbox/UBC/Thesis/Simulation_Thesis/printFigure.R")
print.figure(writeto="~/Dropbox/UBC/Thesis/SimulationBayDRPlot3_B.tex",filename=figName,placement="h",
             caption=capName,label="SimDR3_B")
#dev.off()