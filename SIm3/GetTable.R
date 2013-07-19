load("ParaSet.Rdata")
require(xtable)
names(Paraset)[1:2] = c("n","$\\lambda$")
nobs=500
#subset(Paraset,n==nobs)
xtable = xtable(subset(Paraset[,1:2],n==nobs),caption="$\\lambda$ values are chosen according to the goodness of fit test.",
                label="LambdaValueH1",digits=c(0,0,1))
sink("~/Dropbox/UBC/Thesis/SimH1table.tex")
print.xtable(xtable,table.placement="H",include.rownames=FALSE,sanitize.colnames.function = identity)
sink()