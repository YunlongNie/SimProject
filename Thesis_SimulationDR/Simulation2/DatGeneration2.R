

DatGen = function(n=1000, r = 4,pho=0.7,
                  alpha0 , alpha1 , alpha2 = 0, alpha3=0, 
                  gamma0 = -1.386294, gamma1 = 1, gamma2 = 1, lambda1 = 0,lambda2=0)
{ 
  if(missing(alpha0)) alpha0 = (logit(0.5) + logit(0.05))/2
  if(missing(alpha1)) alpha1 = (logit(0.5) - logit(0.05))/4
  require(LearnBayes)
  Sigma <- diag(rep(1, r))
  Sigma[Sigma == 0] <- pho
  temp <- rmnorm(n, mean = rep(0, r), varcov = Sigma)
  temp[temp > 0] <- 1
  temp[temp != 1] <- 0
  C=temp
  
  Z <- rnorm(10000,0,1)
  Mean34 <- mean((1-pnorm((pho^(-1)-1)^(-0.5)*Z,0,1))^2)
  
  Mean123 = mean((1-pnorm((pho^(-1)-1)^(-0.5)*Z,0,1))^2*pnorm((pho^(-1)-1)^(-0.5)*Z,0,1))
  prp = apply(C, 1, function(x)
  {
    
    pr = alpha0 + alpha1*(x[1]+ x[2] +x[3]-1.5) + alpha2*(x[3]*x[4] - Mean34)+
     alpha3*(x[1]*x[2]*(1-x[3]) - Mean123)
   
    exp(pr)/(1+exp(pr))
    
  }
  )
  
  X = rbinom(n,size=1,prob=prp)
  
  outcome.pr = sapply(1:n, function(x)
  {
    pr = gamma0+ gamma1*X[x] + gamma2*(C[x,1]-0.5) + lambda1*X[x]*(C[x,1]*C[x,2]-Mean34) + lambda2*(C[x,2]*C[x,3]*(1-C[x,4])-Mean123)
    exp(pr)/(1+exp(pr))
  })
  
  Y = rbinom(n,size=1,prob=outcome.pr)
  dat=as.data.frame(cbind(Y,X,C))
  names(dat) = c("Y","X",paste("C",1:r,sep=""))
  return(dat)
}

