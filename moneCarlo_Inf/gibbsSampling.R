

#################         MC Lab        20-11-'17
gibbs2 = function(iters,y,mu=0,tau=1){
  alpha0 = 0.00001
  beta0 = 0.00001
  mu0 = 0.0
  tau0 = sqrt(0.00001)
  x <-array(0,c(iters+1,2))
  x[1,1] = mu
  x[1,2] = tau
  n = length(y)
  ybar = mean(y)
  for(t in 2:(iters+1)){
    x[t,1] = rnorm(1,(n*ybar*x[t-1,2] + mu0*tau0)/
                     (n*x[t-1,2]+tau0), sqrt(1/(n*x[t-1,2]+tau0)))
    sn = sum((y-x[t,1])^2)
    x[t,2] = rgamma(1,alpha0+n/2,rate=beta0+sn/2)
  }
  par(mfrow=c(1,2))
  plot(1:length(x[,1]),x[,1],type='l',lty=1,xlab='t',ylab='mu',main = "Uninformative")
  plot(1:length(x[,2]),1/x[,2],type='l',lty=1,xlab='t',ylab='sigma^2',main = "Uninformative")
  x
}
y = rnorm(10,1,sqrt(2))           #simulating data with size 10 and N(1,2)
#Gibbs sampler with mu=5, tau=2
n=100
mu=5
tau=2
output = gibbs2(n,y,mu,tau)
#####Excercise 2
n=10000
mu=5
tau=2
output = gibbs2(n,y,mu,tau)
# mean 
mean(output[,1]) #1.82
var(output[,1])  #0.367
# variance
mean(output[,2]) #0.362
var(output[,2]) #0.03

# posterior probability mu >1.5  sigma >2.5
PmuGT1.5 = sum(output[,1]>1.5)/n
PmuGT1.5 #=0.719
PsigmaGT2.5 = sum(1/output[,2]>2.5)/n
PsigmaGT2.5 #=0.647
#plots
par(mfrow=c(1,2))
hist(output[,1], prob="TRUE", xlab="mean", main = "Uninformative")
lines(density(output[,1]),col="red",lwd=2)
hist(1/output[,2], prob="TRUE", xlab="variance",main = "Uninformative")
lines(density(1/output[,2]),col="green",lwd=2)
                         
                         #####Excercise 3
gibbs1 = function(iters,y,mu=0,tau=1){
  alpha0 = 0.01
  beta0 = 0.01
  mu0 = 0.0
  tau0 = 0.01
  x <-array(0,c(iters+1,2))
  x[1,1] = mu
  x[1,2] = tau
  n = length(y)
  ybar = mean(y)
  for(t in 2:(iters+1)){
    x[t,1] = rnorm(1,(n*ybar*x[t-1,2] + mu0*tau0)/
                     (n*x[t-1,2]+tau0), sqrt(1/(n*x[t-1,2]+tau0)))
    sn = sum((y-x[t,1])^2)
    x[t,2] = rgamma(1,alpha0+n/2,rate=beta0+sn/2)
  }
  par(mfrow=c(1,2))
  plot(1:length(x[,1]),x[,1],type='l',lty=1,xlab='t',ylab='mu',main = "Informative")
  plot(1:length(x[,2]),1/x[,2],type='l',lty=1,xlab='t',ylab='sigma^2',main = "Informative")
  x
}
#test the gibbs1 function
#y = rnorm(15,0,1)
#output = gibbs1(1000,y,5,0.5)
########
y = rnorm(10,1,sqrt(2))
n=100
mu=5
tau=2
output = gibbs1(n,y,mu,tau)

# mean variance
set.seed(456)
n=10000
mu=5
tau=2
output = gibbs1(n,y,mu,tau)

mean(output[,1]) #1.046
var(output[,1])  #0.196
#mean variance
mean(output[,2]) #0.651
var(output[,2])  #0.092
# posterior probability mu >1.5  sigma >2.5
PmuGT1.5 = sum(output[,1]>1.5)/n
PmuGT1.5   #0.1343
PsigmaGT2.5 = sum(1/output[,2]>2.5)/n
PsigmaGT2.5 #0.2081

#plots
hist(output[,1], prob= TRUE, xlab="Mean", main = "Informative")
lines(density(output[,1]),col="blue",lwd=2)
hist(1/output[,2], prob= TRUE,xlab="Variance",main = "Informative")
lines(density(1/output[,2]),col="purple",lwd=2)

###excercise 5
data(lynx)
library(forecast)
tsdisplay(lynx)
auto.arima(lynx) #Arima(2,0,2) model
##
gibbsAR1 = function(iters,y,a=1,c=10,tau=0.01){
  alpha0 = 0.01
  beta0 = 0.01
  taua = 0.001
  tauc = 0.001
  p <-array(0,c(iters+1,3))
  p[1,1] = c
  p[1,2] = a
  p[1,3] = tau
  n = length(y)
  ybar = mean(y)
  for(t in 2:(iters+1)){
     p[t,1] = rnorm(1,(n*ybar*p[t-1,3]+c*tauc-n*ybar*p[t-1,2]*p[t-1,3] )/
                     (n*p[t-1,3]+tauc), sqrt(1/(n*p[t-1,3]+tauc)))
     p[t,2] = rnorm(1,(n*(ybar^2)*p[t-1,3]+a*taua -n*ybar*p[t-1,1]*p[t-1,3])/
                     (n*(ybar^2)*p[t-1,3]+taua), sqrt(1/(n*p[t-1,3]+taua)))
     s <-array(0,n)
     s[1] = y[1] - p[t,1]
     for(i in 2:n){
      s =  (y[i]-p[t,1]-p[t,2]*y[i-1])
     }
     sn= sum(s*s)
     p[t,3] = rgamma(1,alpha0+n/2,rate=beta0+sn/2)
  }
  par(mfrow=c(1,2))
  plot(1:length(p[,1]),p[,1],type='l',lty=1,xlab='t',ylab='mu', main = "TracePlots Gibbs sampling")
  plot(1:length(p[,3]),1/p[,3],type='l',lty=1,xlab='t',ylab='sigma^2',main = "of Lynx Data")
  p
}
y = lynx          # lynx data
#Gibbs sampler with parameters
set.seed(456)
n=10000
a=1
c=20
tau=0.001
output = gibbsAR1(n,y,a,c,tau)

#####Excercise 6
#posterior mean and variances of the parameters.
# mean 
mean(output[,1]) #-8.35
var(output[,1])  #128565
#
mean(output[,2]) #1.22
var(output[,2])  #951.8
# variance
mean(output[,3]) #5.86e-05
var(output[,3]) #1.88e-06

#Density plots 
c=output[,1]
a=output[,2]
tau=output[,3]
par(mfrow=c(1,3))
hist(c, prob =TRUE)
lines(density(c),col="blue",lwd=2)  
hist(a, prob =TRUE)
lines(density(a),col="red",lwd=2) 
hist(1/tau, prob= TRUE , xlab="variance")
lines(density(1/tau),col="green", lwd=2) 


