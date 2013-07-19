source("DatGeneration2.R")

nobs=100
require(boot)
g0=logit(0.2)
g0=-1.2
g1=0.7
g2=3
source("TrueEffect.R")
Delta=causal.est(gamma0=g0,gamma1=g1,gamma2=g2)
Delta

Pvalue=sapply(1:5000,function(i) 
{
Dat=DatGen(gamma0=g0,gamma1=g1,gamma2=g2,n=nobs,pho=0.3)
glm0 = glm(Y~. + I(X*C1*C2),data=Dat,family=binomial)
sy = summary(glm0)
sy$coefficients[7,4]
#glm1=glm(Y~.,data=Dat,family=binomial)
#pchisq(glm1$dev,glm1$df.resid,lower.tail=FALSE)
#avo=anova(glm0,glm1,test='Chisq')
#avo[5][2,]
}
)
mean(Pvalue<0.05,na.rm=TRUE)

para200=data.frame(obs=200,g2=c(0.8,2.1,1.4),g1=0.7,g0=-1.2)
para500 = data.frame(obs=500,g2=c(0.5,0.9,1.3),g1=0.7,g0=-1.2)
para100 = data.frame(obs=100,g2=c(1.4,2.8),g1=0.7,g0=-1.2)
Paraset=rbind(para100,para200,para500)
save(Paraset,file="ParaSet.Rdata")

glm2 = glm(Y~X+I(X*C1*C2),data=Dat,family=binomial)

summary(glm2)

anova(glm1,test="Chisq")


X = rbinom(n=1000,size=1,prob=0.5)
C1 = rbinom(n=1000,size=1,prob=0.3)
C2 = rbinom(n=1000,size=1,prob=0.2)
C3 = rbinom(n=1000,size=1,prob=0.1)
C4 = rbinom(n=1000,size=1,prob=0.7)

gamma0=-1.2
gamma1=0.7
gamma2=0
Mean34=0.2
outcome.pr = sapply(1:1000, function(x)
{
  pr = gamma0+ gamma1*X[x]  + gamma2*X[x]*(C1[x]*C2[x]-Mean34)
  #lambda2*(C[x,2]*C[x,3]*(1-C[x,4])-Mean123)
  exp(pr)/(1+exp(pr))
})

Y = rbinom(n=1000,size=1,prob=outcome.pr)
glm2=glm(Y~X+C1+C2+C3+C4,family=binomial)
glmn = glm(Y~X+I(C1*C2),family=binomial)
pchisq(glm2$dev,glm2$df.resid,lower.tail=FALSE)
