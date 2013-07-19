# est.dr = function(dat,sd=FALSE)
# {
#   glm.fit = glm(Y~.,data=dat, family="binomial")
#   new.data=cbind(X=1,dat[,-c(1:2)])
#   pr1 = predict(glm.fit,newdata=new.data)
#   pr1 = exp(pr1)/(1+exp(pr1))
#   new.data=cbind(X=0,dat[,-c(1:2)])
#   pr0 = predict(glm.fit,newdata=new.data)
#   pr0 = exp(pr0)/(1+exp(pr0))
#   glm.fit = glm(X~.,data=dat[,-1], family="binomial")
#   prp = fitted(glm.fit)
#   Y = dat$Y
#   X= dat$X
#   
#   est1 = mean( (Y*X - (X-prp)*pr1)/prp )
#   est0 = mean( (Y*(1-X) + (X-prp)*pr0)/(1-prp)) 
#   est.dr = est1 - est0  
#   ## sandwich se
#   if (sd) {sd.dr = sqrt(sum(((Y*X - (X-prp)*pr1)/prp - (Y*(1-X) + (X-prp)*pr0)/(1-prp)-est.dr)^2)/nrow(dat)^2)} else {sd.dr=NULL}
#   
#   list(est.dr=est.dr,sd.dr=sd.dr)
#   #c(est.dr,est1=est1,est0=est0)
# }

est.dr = function(dat,sd=FALSE)
{ 
  require(speedglm)
  #glm.fit = glm(Y~.,data=dat, family="binomial")
  forglm=as.formula(
    paste("Y ~ X +",paste("C",1:(ncol(dat)-2),collapse="+",sep=""))
  )
  glm.fit2=speedglm(forglm,data=dat,family=binomial())
  
  new.data=cbind(int=1,X=1,dat[,-c(1:2)])
  pr1 <- inv.logit(as.matrix(new.data)%*%as.matrix(coef(glm.fit2)))
  new.data=cbind(int=1,X=0,dat[,-c(1:2)])
  pr0 <- inv.logit(as.matrix(new.data)%*%as.matrix(coef(glm.fit2)))
  
  forglm=as.formula(
    paste("X ~",paste("C",1:(ncol(dat)-2),collapse="+",sep=""))
  )
  glm.fit = glm(forglm,data=dat[,-1], family=binomial())
  
  new.data=cbind(int=1,dat[,-c(1:2)])
  prp  <- inv.logit(as.matrix(new.data)%*%as.matrix(coef(glm.fit)))
   
  Y = dat$Y
  X= dat$X
  
  est1 = mean( (Y*X - (X-prp)*pr1)/prp )
  est0 = mean( (Y*(1-X) + (X-prp)*pr0)/(1-prp)) 
  est.dr = est1 - est0  
  ## sandwich se
  if (sd) {sd.dr = sqrt(sum(((Y*X - (X-prp)*pr1)/prp - (Y*(1-X) + (X-prp)*pr0)/(1-prp)-est.dr)^2)/nrow(dat)^2)} else {sd.dr=NULL}
  
  list(est.dr=est.dr,sd.dr=sd.dr)
  #c(est.dr,est1=est1,est0=est0)
}


sd.bs <- function(dat,B=1000)
{
  pb = txtProgressBar(min=0,max=B,style=3)
  est.boos = do.call(rbind,lapply(1:B,function(x)
  {
    b.sample = sample(1:nrow(dat),size=nrow(dat),replace=TRUE)
    dr= est.dr(dat[b.sample,])$est.dr
    X = dat[b.sample,"X"]
    Y = dat[b.sample,"Y"]
    C = dat[b.sample,paste0("C",1:(ncol(dat)-2))]
    setTxtProgressBar(pb,x)
    est = as.vector(bay.est.old(Y,X,C)$est)
    return(c(dr,sold=est[1],P=est[2],Bold=est[3]))
  }))
  
  est.sd = as.vector(apply(est.boos,2,sd))
  list(dr.sd=est.sd[1],sold.sd=est.sd[2],P.sd=est.sd[3],Sold.sd=est.sd[4])
}

simFun <- function(Dat)
{
  #source("Fun.R")
  dr = est.dr(Dat)$est.dr
  require(BayDR)
  bay0 = as.vector(try(bay.est.old(Dat=Dat)$est))
  bay1 = as.vector(try(bay.est.new(Dat=Dat,addBin=0)$est))
  bay2 = as.vector(try(bay.est.new(Dat=Dat)$est))
  
  
  if (!(is.numeric(bay0)&is.numeric(bay1)&is.numeric(bay2))) bay0=bay1=bay2=rep(NA,3)
  res  = c(dr = dr,s0=bay0[1],p0=bay0[2],b0=bay0[3],s1 =bay1[1],
           p1 = bay1[2], b1 = bay1[3],s2 =bay2[1],
           p1 = bay2[2], b2 = bay2[3],
           r=ncol(Dat)-2)
  gc()
  return(res)
}
