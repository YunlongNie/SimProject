load("ParaSet.Rdata")
require(xtable)
names(Paraset)[1:2] = c("n","$\\lambda$")
cap = "$\\lambda$ values are chosen according to the goodness of fit test. " 
xtable = xtable((Paraset[,1:2]),caption=cap,label="LambdaValueSimBay",digits=c(0,0,1))
sink("~/Dropbox/UBC/Thesis/SimuBayDRTable.tex")
print.xtable(xtable,table.placement="H",include.rownames=FALSE,sanitize.colnames.function = identity)
sink()