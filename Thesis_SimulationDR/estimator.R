## estimators. 
 

# given Y, X,C
# causal.est()
# 
# dat = DatGen(n=10000)

est.reg=function(dat)
{
glm.fit = glm(Y~.,data=dat, family="binomial")

new.data=cbind(X=1,dat[,-c(1:2)])
pr1 = predict(glm.fit,newdata=new.data)

est.mu1= mean(exp(pr1)/ (1+exp(pr1)))

new.data=cbind(X=0,dat[,-c(1:2)])
pr0 = predict(glm.fit,newdata=new.data)
est.mu0 = mean(exp(pr0)/ (1+exp(pr0)))

est.reg = est.mu1 - est.mu0


######

s = summary(glm.fit)
estCov = s$cov.scaled

b0 = mean(exp(pr1)/ (1+exp(pr1))^2) -  mean(exp(pr0)/ (1+exp(pr0))^2)
bx = mean(exp(pr1)/ (1+exp(pr1))^2)

der = c(b0,bx,sapply(grep("C",names(dat)), function(x){
c = dat[,x]
mean(exp(pr1)/ (1+exp(pr1))^2*c ) -  mean(exp(pr0)/ (1+exp(pr0))^2*c )
}
))
     
list(est=est.reg, sd=sqrt(sum(der%*%estCov*der)))
  
}

est.prp=function(datY)
{
  dat = datY[,-1]
  glm.fit = glm(X~.,data=dat, family="binomial")
  
  prp = fitted(glm.fit)
  Y = datY$Y
  X= datY$X
  est1 = mean(X*Y/prp)
  
  est0 = mean((1-X)*Y/(1-prp))
  
  est.prp=est1-est0
  est.prp
#   #c(est.prp,est1=est1,est0=est0)
#   
#   ###########
#   s = summary(glm.fit)
#   estCov = s$cov.scaled
#   
#   
#   temp.dat = cbind(int=1,dat[,-grep("X",names(dat))])  
#   der = apply(temp.dat,2, function(x) {  
#   mean((X*Y/prp^2 + (1-X)*Y/(1-prp)^2)*x)
#   }  
#   )
#   sqrt(est1+ est0 - est.prp^2 - 
#     sum(der%*%estCov*der))
  
  ###########
}


est.dr = function(dat)
{
  glm.fit = glm(Y~.,data=dat, family="binomial")
  new.data=cbind(X=1,dat[,-c(1:2)])
  pr1 = predict(glm.fit,newdata=new.data)
  pr1 = exp(pr1)/(1+exp(pr1))
  new.data=cbind(X=0,dat[,-c(1:2)])
  pr0 = predict(glm.fit,newdata=new.data)
  pr0 = exp(pr0)/(1+exp(pr0))
  glm.fit = glm(X~.,data=dat[,-1], family="binomial")
  prp = fitted(glm.fit)
  Y = dat$Y
  X= dat$X
  
  est1 = mean( (Y*X - (X-prp)*pr1)/prp )
  est0 = mean( (Y*(1-X) + (X-prp)*pr0)/(1-prp)) 
  est.dr = est1 - est0  
  ## sandwich se
  sd.dr = sqrt(sum(((Y*X - (X-prp)*pr1)/prp - (Y*(1-X) + (X-prp)*pr0)/(1-prp)-est.dr)^2)/nrow(dat)^2)
  
  list(est.dr=est.dr,sd.dr=sd.dr)
  #c(est.dr,est1=est1,est0=est0)
}
  

sd.bs <- function(dat,B=1000)
{
  est.boos = do.call(rbind,lapply(1:B,function(x)
    {
    b.sample = sample(1:nrow(dat),size=nrow(dat),replace=TRUE)
    dr= est.dr(dat[b.sample,])$est.dr
    prp = est.prp(dat[b.sample,])
    reg = est.reg(dat[b.sample,])$est
    c(dr,prp,reg)
  }))
  
  est.sd = apply(est.boos,2,sd)
  list(dr=est.sd[1],prp=est.sd[2],reg=est.sd[3])
}


coverage.rate=function(con.matrix,causal) {
  mean(apply(con.matrix,1,function(x)
  {x[1] < causal& causal<x[2]}
  ))
}

