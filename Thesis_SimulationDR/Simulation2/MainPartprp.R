pb =  txtProgressBar(min = 0, max = N, style = 3)
res=do.call(rbind,lapply(1:N, function(x)
{
  dat = DatGen(n=n,r=r,pho=pho,
               alpha2 = alpha2,alpha1=alpha1, alpha3=alpha3,
               gamma0 = gamma0, gamma1 = gamma1, gamma2 = gamma2, lambda1 = lambda1,lambda2=lambda2)
  reg=est.reg(dat=dat)
  prp= est.prp(datY=dat)
  dr = est.dr(dat=dat)
  sd = sd.bs(dat=dat,B=1000)
  temp=c(reg=reg$est,sd.reg = reg$sd, prp=prp,dr=dr$est.dr,sd.dr.asp= dr$sd.dr,sd.dr.bs = sd$dr, prp.sd.bs = sd$prp,
    reg.sd.bs = sd$reg)
 print(temp)
  print(x) 
  setTxtProgressBar(pb, x)
  return(temp)
}
)
)
close(pb)
causal = causal.est(r=r,pho=pho,
                             gamma0 = gamma0, gamma1 = gamma1, gamma2 = gamma2, lambda1 = lambda1,lambda2=lambda2)

res=as.data.frame(res)
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
