figName= paste0("SimLast",la)
ggsave(plot=plotsave,filename=paste0("~/Dropbox/UBC/Thesis/",figName,".pdf"),width=10,height=6)  
cap1="RRMSE of estimates $\\widehat \\Delta_{D},\\widehat \\Delta_{S0},\\widehat \\Delta_{P},
\\widehat \\Delta_{B0},\\widehat \\Delta_{S1},
\\widehat \\Delta_{B1},\\widehat \\Delta_{S2}
,\\widehat \\Delta_{B2}$ based on $\\rho=0.3, \\gamma_0 = \\text{logit(0.2)},\\gamma_1=\\gamma_2=1$ and "

cap2 = paste0("$\\lambda_1=\\lambda_2=",la,"$. ")

cap3 = "Each panel is simulated under different values of number of confounder blocks, $k$, and sample size, $n$. "

capName=paste0(cap1,cap2,cap3)

source("~/Dropbox/UBC/Thesis/Simulation_Thesis/printFigure.R")

print.figure(writeto=paste0("~/Dropbox/UBC/Thesis/",figName,".tex"),filename=figName,placement="h",
             caption=capName,label=figName)