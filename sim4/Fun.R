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
  
  new.data=cbind(int=1,X=1,dat[,grep("C",names(dat))])
  pr1 <- inv.logit(as.matrix(new.data)%*%as.matrix(coef(glm.fit2)))
  new.data=cbind(int=1,X=0,dat[,grep("C",names(dat))])
  pr0 <- inv.logit(as.matrix(new.data)%*%as.matrix(coef(glm.fit2)))
  
  forglm=as.formula(
    paste("X ~",paste("C",1:(ncol(dat)-2),collapse="+",sep=""))
  )
  glm.fit = glm(forglm,data=dat[,-grep("Y",names(dat))], family=binomial())
  
  new.data=cbind(int=1,dat[,grep("C",names(dat))])
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


sat2 <- function(Y,X,C,k_q,mean,BinMean,con,BinCon,addBin,kappa,beta,Mc.error,liketype,Dat)
{
  if (missing(Y)|missing(X)|missing(C)) {Y=Dat$Y;X=Dat$X;C=Dat[,paste0("C",1:(ncol(Dat)-2))]}
  if (missing(Dat)&(missing(Y)|missing(X)|missing(C))) stop("Data entry wrong Y or X or C or Dat is missing") 
  if (missing(k_q)) k_q=1
  if (missing(mean)) mean=c(0.01,0.99)
  if (missing(con)) con=c(1,20)
  if (missing(BinMean)) BinMean=20
  if (missing(BinCon)) BinCon=20
  if (missing(beta)) beta=30^2*diag(rep(1,ncol(C)+2))
  if (missing(Mc.error)) Mc.error=0.001
  if (missing(kappa)) kappa=0.5
  if (missing(addBin)) addBin=c(0,10)
  if (missing(liketype)) liketype="bernoulli"
  
  temp.p = para.est(Y,X,C,beta=beta,k_q=k_q,Mc.error=Mc.error)
  estimate.p = temp.p$P
  likelihood.p=temp.p$likelihood
  
  temp.s0 <- sat.est.old(Y,X,C)
  likelihood0 = temp.s0$likelihood.old
  estimate.s0 = temp.s0$est.old
  
  ratio.likelihood0<- exp(likelihood0 - likelihood.p)
  w0 <- kappa/(kappa+(1-kappa)*ratio.likelihood0)
  estimate.b0 <- w0*estimate.p+(1-w0)*estimate.s0 # bayesian estimate for hirarchical version
  
  temp.s1 <- sat.est.new(Y,X,C,k_q,mean,BinMean,con,BinCon,addBin=addBin[1],liketype=liketype)
  likelihood1 = temp.s1$likelihood.new
  estimate.s1 = temp.s1$est.new
  
  ratio.likelihood1<- exp(likelihood1 - likelihood.p)
  w1 <- kappa/(kappa+(1-kappa)*ratio.likelihood1)
  estimate.b1 <- w1*estimate.p+(1-w1)*estimate.s1 # bayesian estimate for hirarchical version
  
  temp.s2 <- sat.est.new(Y,X,C,k_q,mean,BinMean,con,BinCon,addBin=addBin[2],liketype=liketype)
  likelihood2 = temp.s2$likelihood.new
  estimate.s2 = temp.s2$est.new
  
  ratio.likelihood2<- exp(likelihood2 - likelihood.p)
  w2 <- kappa/(kappa+(1-kappa)*ratio.likelihood2)
  estimate.b2 <- w2*estimate.p+(1-w2)*estimate.s2 #
  
  
  return(
    list(est=c(
               P=estimate.p,  
               S0=estimate.s0, 
               B0=estimate.b0,
               S1=estimate.s1,
               B1=estimate.b1,
               S2=estimate.s2,
               B2=estimate.b2
               ),
         likelihood=c(l.p=likelihood.p,l.S0=likelihood0,l.S1=likelihood1,S2=likelihood2)    
    )
  )  
}


simFun <- function(Dat)
{
  #source("Fun.R")
  dr = est.dr(Dat)$est.dr
  require(BayDR)
  est = as.vector(try(sat2(Dat=Dat)$est))
#   bay1 = as.vector(try(bay.est.new(Dat=Dat,addBin=0)$est))
#   bay2 = as.vector(try(bay.est.new(Dat=Dat)$est))
  
  
  if (!is.numeric(est)) est=rep(NA,length(est))
  res  = c(dr = dr,s0=est[2],p=est[1],b0=est[3],s1 =est[4],
             b1 = est[5],s2 =est[6],
           b2=est[7],
           r=ncol(Dat)-2)
  gc()
  return(res)
}
