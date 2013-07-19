## Data Generation

# return a list of C, X, Y 

DatGen = function(n=1000, r = 4, gamma = 0.3, beta0 = -1.25, lambda1 = 0.6, alpha0 =-0.75, lambda2=0,pho=0.7,prp.para)
{ 
  if(missing(prp.para)) prp.para=2
  require(LearnBayes)
  Sigma <- diag(rep(1, r))
  Sigma[Sigma == 0] <- pho
  temp <- rmnorm(n, mean = rep(0, r), varcov = Sigma)
  temp[temp > 0] <- 1
  temp[temp != 1] <- 0
  C=temp
  prp = apply(C, 1, function(x)
    {
    
    pr = alpha0 + prp.para*x[1]* x[2] + lambda1*x[1]*x[2]*(1-x[3])
    #pr = alpha0 + x[1] +  x[2] + lambda1*x[1]*x[2]*(1-x[3])
    exp(pr)/(1+exp(pr))
    
  }
        )

  X = rbinom(n,size=1,prob=prp)
  
  outcome.pr = sapply(1:n, function(x)
    {
    pr = beta0 + gamma*X[x] + C[x,1] + C[x,2] + lambda2*C[x,1]*C[x,2]*(1-C[x,3])
    exp(pr)/(1+exp(pr))
  })

  Y = rbinom(n,size=1,prob=outcome.pr)
  dat=as.data.frame(cbind(Y,X,C))
  names(dat) = c("Y","X",paste("C",1:r,sep=""))
  return(dat)
}
