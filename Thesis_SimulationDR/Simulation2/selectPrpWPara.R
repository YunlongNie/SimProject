pho=0.7
Z <- rnorm(10000,0,1)
Mean34 <- mean((1-pnorm((pho^(-1)-1)^(-0.5)*Z,0,1))^2)
Mean123 = mean((1-pnorm((pho^(-1)-1)^(-0.5)*Z,0,1))^2*pnorm((pho^(-1)-1)^(-0.5)*Z,0,1))

require(boot)
x = logit(0.9);y=logit(0.05)

(alpha3 = (Mean34*(x-y) + y)/(Mean34-Mean123))
(alpha2 = x-y-alpha3)


alpha2=2
alpha3=2
x = alpha2*(1-Mean34)+alpha3*(1-Mean123)
inv.logit(x)

y = -alpha2*Mean34-alpha3*Mean123
inv.logit(y)


alpha0 = (logit(0.5) + logit(0.05))/2
alpha1 = (logit(0.5) - logit(0.05))/4
r=4
alpha2=1
alpha3=3
C = do.call(expand.grid, rep(list(c(0,1)),r))

range=apply(C,1,function(x){
  
  inv.logit(alpha0 + alpha1*(sum(x[1:3])-1.5) + alpha2*(x[3]*x[4]-Mean34) +
    alpha3*(x[1]*x[2]*(1-x[3])-Mean123))
  
})

range(range)