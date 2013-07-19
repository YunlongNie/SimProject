source("Simulation2/TrueEst.R")
r=4;pho=0.7;
alpha2 = 0;alpha3=0 
gamma0 = -1.386294; gamma1 = 1; gamma2 = 1; lambda1 = 2;lambda2=2

causal= causal.est(r=r,pho=pho,
                    gamma0 = gamma0, gamma1 = gamma1, gamma2 = gamma2, lambda1 = lambda1,lambda2=lambda2)

filenames = list.files(path="Simulation2/")
input.file = filenames[grep("outW",filenames)]


r=4;pho=0.7;
alpha2 = 2;alpha1=0;alpha3=2 
gamma0 = -1.386294; gamma1 = 1; gamma2 = 1; lambda1 = 0;lambda2=0

causal = causal.est(r=r,pho=pho,
                         gamma0 = gamma0, gamma1 = gamma1, gamma2 = gamma2, lambda1 = lambda1,lambda2=lambda2)

filenames = list.files(path="Simulation2/")
input.file = filenames[grep("prpW",filenames)]

for (i in 1:length(input.file))
{
load(paste0("Simulation2/",input.file[i]))
Z = qnorm(0.975)
con.reg.asp = data.frame(lower=res$reg-Z*res$sd.reg,upper=res$reg+Z*res$sd.reg)
con.reg.bs = data.frame(lower=res$reg-Z*res$reg.sd.bs,upper=res$reg+Z*res$reg.sd.bs)
# cov.reg.asp = coverage.rate(con.reg.asp,causal)
# cov.reg.bs = coverage.rate(con.reg.bs,causal)


con.prp.bs = data.frame(lower=res$prp-Z*res$prp.sd.bs,upper=res$prp+Z*res$prp.sd.bs)
#cov.prp.bs = coverage.rate(con.prp.bs,causal)

con.dr.asp =data.frame(lower=res$dr-Z*res$sd.dr.asp,upper=res$dr+Z*res$sd.dr.asp)
con.dr.bs = data.frame(lower=res$dr-Z*res$sd.dr.bs,upper=res$dr+Z*res$sd.dr.bs)
# cov.dr.asp = coverage.rate(con.dr.asp,causal)
# cov.dr.bs = coverage.rate(con.dr.bs,causal)


cov.rate = c(cov.reg.asp = coverage.rate(con.reg.asp,causal),
             cov.reg.bs = coverage.rate(con.reg.bs,causal),
             cov.dr.asp = coverage.rate(con.dr.asp,causal),
             cov.dr.bs = coverage.rate(con.dr.bs,causal),
             cov.prp.bs = coverage.rate(con.prp.bs,causal)
)

mse = colMeans(res[,c("dr","prp","reg")] -causal)^2
save(mse,cov.rate,res,causal,file=paste0("Simulation2/",input.file[i]))
}

