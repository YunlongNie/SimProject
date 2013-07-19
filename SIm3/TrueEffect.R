causal.est=function(
                    r = 4,pho=0.7,
                    gamma0 = -1.25, gamma1 = 1, 
                    gamma2
                    )
  {
C = do.call(expand.grid, rep(list(c(0,1)),r))
gc = rowSums(C)
Z <- rnorm(1000000, 0, 1)

Mean34 <- mean((1-pnorm((pho^(-1)-1)^(-0.5)*Z,0,1))^2)
pr.C= sapply(gc, function(x) {
mean(pnorm((pho^(-1) - 1)^(-0.5) * Z, 0, 1)^(r-x)*(1-pnorm((pho^(-1) - 1)^(-0.5) * Z, 0, 1))^x)
})

mu1 = sum(sapply(1:(2^r), function(x)
  {
  temp=gamma0 + gamma1 + gamma2*(C[x,1]*C[x,2]-Mean34)
  exp(temp)/(1+exp(temp))*pr.C[x]
})) 
mu0 = sum(sapply(1:2^r, function(x)
  {
    temp=gamma0 
    exp(temp)/(1+exp(temp))*pr.C[x]
  }))

cau.est= mu1 - mu0
#return(c(cau.est,mu1,mu0))
return(cau.est)
}



#gamma0+ gamma1*X[x]  + gamma3*X[x]*(C[x,1]*C[x,2]-Mean34)
