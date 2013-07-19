causal.est=function(r=4,pho=0.7,
                    gamma0 = -1.386294, gamma1 = 1, gamma2 = 1, lambda1 = 0,lambda2=0)
{
  C = do.call(expand.grid, rep(list(c(0,1)),r))
  gc = rowSums(C)
  Z <- rnorm(100000, 0, 1)
  Mean34 <- mean((1-pnorm((pho^(-1)-1)^(-0.5)*Z,0,1))^2)
  Mean123 = mean((1-pnorm((pho^(-1)-1)^(-0.5)*Z,0,1))^2*pnorm((pho^(-1)-1)^(-0.5)*Z,0,1))
  pr.C= sapply(gc, function(x) {
    mean(pnorm((pho^(-1) - 1)^(-0.5) * Z, 0, 1)^(r-x)*(1-pnorm((pho^(-1) - 1)^(-0.5) * Z, 0, 1))^x)
  })
  
  mu1 = sum(sapply(1:2^r, function(x)
  {
    temp=gamma0 + gamma1 + gamma2*(C[x,1]-0.5) + lambda1*(C[x,1]*C[x,2]-Mean34) + lambda2*(C[x,2]*C[x,3]*(1-C[x,4])-Mean123)
    exp(temp)/(1+exp(temp))*pr.C[x]
  })) 
  mu0 = sum(sapply(1:2^r, function(x)
  {
    temp=gamma0 +  gamma2*(C[x,1]-0.5) + lambda2*(C[x,2]*C[x,3]*(1-C[x,4])-Mean123)
    exp(temp)/(1+exp(temp))*pr.C[x]
  }))
  
  cau.est= mu1 - mu0
  #return(c(cau.est,mu1,mu0))
  return(cau.est)
}
# outcome.pr = sapply(1:n, function(x)
# {
#   pr = gamma0+ gamma1*X[x] + gamma2*(C[x,1]-0.5) + lambda1*X[x]*(C[x,1]*C[x,2]-Mean34) + lambda2*(C[x,2]*C[x,3]*(1-C[x,4])-Mean123)
#   exp(pr)/(1+exp(pr))
# })