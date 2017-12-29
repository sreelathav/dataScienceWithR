#curve(dgamma(x, 3, 1/4), 0, 30, ylim=c(0,.2), lwd=2, col="blue")
#curve(2*dexp(x,1/12), 0, 30, add=T, col="red", lwd=2)
#abline(v=0, col="green2");  abline(h=0, col="green2")
#####
rz1<-function () 
{
  repeat {
    x <- runif(1, 0, 1)
    y <- runif(1, 0, 3/2)
    fx <- 6 * x * (1 - x)
    if (y < fx) 
      return(x)
  }
}

rz2<-function (n) 
{
  zvector <- vector("numeric", n)
  for (i in 1:n) {
    zvector[i] <- rz1()
  }
  zvector
}

rz3<-function (n) 
{
  xvec <- runif(n, 0, 1)
  yvec <- runif(n, 0, 3/2)
  fvec <- 6 * xvec * (1 - xvec)
  xvec[yvec < fvec]
}

rz4<-function (n) 
{
  x <- rz3(n)
  len <- length(x)
  aprob <- len/n
  aprob
  shortby <- n - len
  n2 <- round(shortby/aprob)
  n2
  x2 <- rz3(n2)
  x3 <- c(x, x2)
  x3
}

hist(rz4(10000),30)

##################
sample.x = runif(100000,0,1)
accept = c()
for(i in 1:length(sample.x)){
  U = runif(1, 0, 1)
  if(dunif(sample.x[i], 0, 1)*3*U <= dbeta(sample.x[i], 6, 3)) {
    
    accept[i] = 'Yes'
  }
  else if(dunif(sample.x[i],0,1)*3*U > dbeta(sample.x[i], 6, 3)) {
    accept[i] = 'No'
  }
}
T = data.frame(sample.x, accept = factor(accept, levels= c('Yes','No')))

#We can plot the results along with the true distribution with the following code.

hist(T[,1][T$accept=='Yes'], breaks = seq(0,1,0.01), freq = FALSE, main = 'Histogram of X', xlab = 'X')
lines(x, dbeta(x,6,3))
######

#exp distribution
lambda <- 1
u <- runif(1)
x   <-  -log(1-u)/lambda
x
###
n <- 20
lambda <- 1
u <- runif(n)
x <- -log(1-u)/lambda
x
####exp random variates
inv.exp   <- function(n, lambda){
  u <- runif(n)
  x <- -log(1-u)/lambda
  x
}
inv.exp(20,1)
###comparision of histogram of sampled values to a plot of density
x <- inv.exp(n=1000, lambda=0.5)
hist(x, freq=FALSE)
t <- 0:150/10
lines(t,dexp(t,rate=0.5), lwd=2)
#kolmogorov-smirnov test to test null hypothesi that sample is from Ex(0.5)
ks.test(x,pexp,rate=0.5)
#gamma distribution
k <- 4
lambda <- 1
x <- inv.exp(n=k, lambda=lambda)
y   <-  sum(x)
y
# define matrix with n*k random exponential numbers and add up each row
n <- 20
k <- 4
lambda <- 1
x <- matrix(inv.exp(n=n*k, lambda=lambda),ncol=k)
x
y  <- apply(x,1,sum)
y
##wrapping in a function
inv.gamma.int <- function(n,k,lambda){
  x  <- matrix(inv.exp(n=n*k, lambda=lambda),ncol=k)
  apply(x,1,sum)
}
y <- inv.gamma.int(20,4,1)
y
##################excercise 1########
Pois1<-function(s0,lam0){
  spread=3*sqrt(lam0)
  t=round(seq(max(0,lam0-spread),lam0+spread,1))
  prob=ppois(t,lam0)
  X=rep(0,s0)
  for (i in 1:s0){
    u=runif(1)
    X[i]=max(t[1],0)+sum(prob<u)-1
  }
  return(X)
}
and
Pois2<-function(s0,lam0){
  X=rep(0,s0)
  for (i in 1:s0){
    sum=0;k=1
    sum=sum+rexp(1,lam0)
    while (sum<1){ sum=sum+rexp(1,lam0);k=k+1}
    X[i]=k
  }
  return(X)
}
###############################################
inv.pois.int <- function(n,k,lambda){
  
  Po = NULL
  for (i in 1:k){
      x  <- inv.exp(n, lambda=lambda)
  
  
      y <- length(which(cumsum(x)<=1))
  }
  
  
  #Po  <- matrix(inv.exp(n=n*k, lambda=lambda),ncol=k)
}
k=100
n=10000
ib1 <- inv.pois.int(10000,100,2)
y
lambda=2
po  <- matrix(inv.pois.int(n=n*k, lambda=lambda),ncol=k)
po 
ib1 <- inv.pois.int(10000,100,2)
ib1

summary(ib1)
mean(ib1)

ib2 <- inv.pois.int(10000,100,5)
mean(ib2)
ib3 <- inv.pois.int(100000,100,10)
mean(ib3)
####

rejection = function(n){
  r = NULL
  for(i in 1:n){
    t = -1
    while(t < 0){
      x = rexp(1,1)
      y = runif(1,0,exp(-x))
      if(x > 1)
        t = -y
      else t = x^2 * exp(-x) - y
    }
    r[i]
  }
  r
}  
rejection(2000)
#############################Ex 2
rejection = function(n){
  r = NULL
  for(i in 1:n){
    t = -1
    while(t < 0){
      x = rexp(1,1)
      y = runif(1,0,exp(-x))
      if(x > 1 & x <1)
        t = -y
      else t =exp(-x^2/2)/sqrt(2*3.14) - y
    }
    r[i]
  }
  r
}  
rejection(2000)
