
#### input outW input file order matters should be 100 then 200 then 500

filenames = list.files(path="Simulation2/")
input.file = filenames[grep("outW",filenames)]
input.file = filenames[grep("prpW",filenames)][-1]
model = "prpW"
obs.n=c(100,200,500)

#### source process
source("Simulation2/latexTableRes.R")

causal


#### output1 latex table mse is sqrt(mse)

alin = "r|c||rrrr|"
cap=paste0("Outcome is not correctly specified with true causal effect being ",round(causal,3))
sink(paste0("~/Dropbox/UBC/Thesis/tableEst_",model,".tex"))
print.xtable(xtable(format(res.table[,c(2,1,3:5)],digits=3),caption=cap,align=alin,label="outW.res"),
             include.rownames=FALSE,table.placement="H")
sink()

#### output2 coverage rate#####
cov.res = data.frame(n=obs.n,do.call(rbind,cov.t))
cov.res
sink(paste0("~/Dropbox/UBC/Thesis/tableCov_",model,".tex"))
print.xtable(xtable(format(cov.res,digits=3),caption="coverage"),include.rownames=FALSE,table.placement="H")
sink()