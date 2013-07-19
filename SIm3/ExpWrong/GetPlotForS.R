#source("GetPlot.R")
require(ggplot2)
load("resMerge.Rdata")
c("D","S0","P0","B0","S1","P1","B1","S2","B2","N")
estimator = c("S0","S1","S2","N")
subest= subset(resMerge,variable%in%estimator)

ggplot.temp=ggplot(data=subest) +
  facet_wrap(~r,scales = "free_x")+theme(legend.position = "none") +
  xlab("lambda")+ylab("RMSE")
plotsave=ggplot.temp +
  geom_text(aes(y=value,x=g2,label=variable,
                color=factor(variable)),alpha=1,size=4,angle=15,
            position = position_dodge(width=0.2))
 #geom_linerange(aes(ymax=value-sd,ymin=value+sd,x=g2,color=factor(variable)),position=position_dodge(width=0.2),angle=90)
  
  
  figName= "SimulationBayDR3_S"
capName="Root-mean-squared error of estimates $\\widehat \\Delta_{S0},
\\widehat \\Delta_{S1},\\widehat \\Delta_{S2} $. $\\widehat \\Delta_{N}=0$, so N represents the true value of $\\Delta$. Each panel is simulated under different number of confounders ranging from 4 to 12. "
ggsave(plot=plotsave,filename=paste0("~/Dropbox/UBC/Thesis/",figName,".pdf"),width=10,height=6)  
source("~/Dropbox/UBC/Thesis/Simulation_Thesis/printFigure.R")
print.figure(writeto="~/Dropbox/UBC/Thesis/SimulationBayDRPlot3_S.tex",filename=figName,placement="h",
             caption=capName,label="SimDR3_S")