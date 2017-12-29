################## Monte carlo 
###########
rejection = function(n){
  r = NULL
  for(i in 1:n){
    t = -1
    while(t < 0){
      x = rexp(1,1)
      y = runif(1,0,exp(-abs(x)))
      if(x > 1 )
        t = -y
      else t = (exp(-x^2/2)/sqrt(2*3.14)) - y
    }
    r[i] = x
  }
  r
}  
rejection(500)
################
rejection = function(n, alpha,lambda){
  r = NULL
  for(i in 1:n){
    t = -1
    while(t < 0){
      x = rexp(1,1)
      y = runif(1,0,exp(-x))
      if(x > 1)
        t = -y
      else 
        t = x^(alpha-lambda) * exp(-x) - y
    }
    r[i] = x
  }
  r
}  
rejection(200,2.3,2)
ks.test(rejection(200,2.3,2),pgamma(q=2.3, rate=2,shape=2))
############
rejection = function(n,alpha,lambda){
  r = NULL
  a = NULL
  for(i in 1:n){
    t = -1
    while(t < 0){
      x = rexp(1,1)
      y = runif(1,0,exp(-x))
    
      if(x > 1)
        t = -y
        #a = dagamma(y,alpha,lambda)
      else 
        t = exp(-x)*x^(alpha-lambda) - y
      }
    r[i]
    }
    r
}  
rejection(100000,2.3,2)
############
inv.exp   <- function(n, lambda){
  u <- runif(n)
  x <- -log(1-u)/lambda
  x
}
inv.gamma.int <- function(n,k,lambda){
  x  <- matrix(inv.exp(n=n*k, lambda=lambda),ncol=k)
  apply(x,1,sum)
}
#rejection sampling
#y <- inv.gamma.int(200000,20,2)
fx<-dgamma(0.5,shape=2.5,rate=3)
hx<-dgamma(0.5,shape=2,rate=2)
m<-fx/hx
m

V=NULL
while(length(V)<100){
  U<-runif(1)
  X<-inv.gamma.int(100000,20,2)
  Y<-U*m*dgamma(X,shape=2,rate=2)
  fx<-dgamma(X,shape=2.5,rate=3)
  if (Y<fx){
    V=append(V,X)
  }
}  
V
mean(V)
######to test#####
require(distr) 
G0 <- Gammad(scale = 2.3, shape = 2) ## generates a Gamma distribution 
#G <- 1/G0 ## the corresponding inverse Gamma 
d(G0)(2) ### density of G0 at 2 
p(G0)(4) ## cdf of G0 at 4 ... 
#########

