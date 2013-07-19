causal.est=function(r=4,gamma = 0.3, beta0 = -1.25, alpha0 =-0.75, lambda2= 0,pho=0.7)
  {
C = do.call(expand.grid, rep(list(c(0,1)),r))
gc = rowSums(C)
Z <- rnorm(100000, 0, 1)

pr.C= sapply(gc, function(x) {
mean(pnorm((pho^(-1) - 1)^(-0.5) * Z, 0, 1)^(r-x)*(1-pnorm((pho^(-1) - 1)^(-0.5) * Z, 0, 1))^x)
})

mu1 = sum(sapply(1:2^r, function(x)
  {
  temp=beta0 + gamma + C[x,1] + C[x,2] + lambda2*C[x,1]*C[x,2]*(1-C[x,3])
  exp(temp)/(1+exp(temp))*pr.C[x]
})) 
mu0 = sum(sapply(1:2^r, function(x)
  {
    temp=beta0  + C[x,1] + C[x,2] + lambda2*C[x,1]*C[x,2]*(1-C[x,3])
    exp(temp)/(1+exp(temp))*pr.C[x]
  }))

cau.est= mu1 - mu0
#return(c(cau.est,mu1,mu0))
return(cau.est)
}