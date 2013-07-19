#source("GetPlot.R")
require(ggplot2)
load("ExpWrong/resMerge.Rdata")
estimator=c("S0","P0","B0","S1","P1","B1","S2","B2","D","N")

subest= subset(resMerge,variable%in%estimator)
ggplot.temp=ggplot(data=subest) +
  facet_wrap(~r,scales = "free_y")+theme(legend.position = "none") +
  xlab("lambda")+ylab("RMSE")
plotsave=ggplot.temp +
  geom_text(aes(y=value,x=g2,label=variable,
                color=factor(variable)),alpha=1,size=3,angle=15,
            position = position_dodge(width=0.2))
 #geom_linerange(aes(ymax=value-sd,ymin=value+sd,x=g2,color=factor(variable)),position=position_dodge(width=0.2),angle=90)

figName= "SimulationBayDRexp1"
ggsave(plot=plotsave,filename=paste0("~/Dropbox/UBC/Thesis/",figName,".pdf"),width=11,height=6)  
capName="Root-mean-squared error of estimates $\\widehat \\Delta_{S0},\\widehat \\Delta_{P0},\\widehat \\Delta_{B0},\\widehat \\Delta_{S1},\\widehat \\Delta_{P1},\\widehat \\Delta_{B1},\\widehat \\Delta_{S2},
\\widehat \\Delta_{P2},\\widehat \\Delta_{B2}$. Note that the true value of $\\Delta$ is around 0.15. 
Each panel is simulated under different number of confounders ranging from 4 to 12. "
source("~/Dropbox/UBC/Thesis/Simulation_Thesis/printFigure.R")
print.figure(writeto=paste0("~/Dropbox/UBC/Thesis/",figName,".tex"),filename=figName,placement="h",
             caption=capName,label=figName)
#dev.off()