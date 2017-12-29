###############  Monte carlo sampling
                      ### ###poisso distribution
pois<-function(n,lambda){
  y = rep(0,n)
  for (i in 1:n){
    sum = 0;
    k = 1
    sum = sum + rexp(1,lambda)
    while (sum<1){ 
      sum = sum + rexp(1,lambda)
      k=k+1
    }
    y[i]=k
  }
  return(y)
}
plot(pois(10000,2))
s2 <- sample(pois(100000,2),100)
s5 <- sample(pois(100000,5),100)
s10 <- sample(pois(100000,10),100)
s15 <- sample(pois(100000,15),100)

mean(s2); var(s2)
mean(s5); var(s5)
mean(s10); var(s10)
mean(s15); var(s15)
               ##############Rejection sampling
rejection = function(n){
  r = NULL
  for(i in 1:n){
    finished = FALSE
    while(!finished){
      x = rexp(1,1)
      f = (sqrt(2*pi)*exp(-x^2+abs(x)/2))
      y = runif(1,0,exp(-abs(x)))
      finished =  (f<y )
    }   
    r[i] = x
  }
  r
}  
rejection(500)
                     ###### #Acceptance function
calcM <-function(alpha,lambda) {
  k <- floor(alpha)
  dalpha <- alpha-k
  (lambda^alpha)*(dalpha^dalpha)*exp(-dalpha)/(lambda-1)^k
}
gammafn <- function(x,alpha,lambda){
  (lambda^alpha)*x^(alpha-1)*exp(-lambda*x)
}
acceptFn<-function (n,alpha,lambda){ 
  if (alpha <= 1 || lambda <= 1) stop('alpha <= 1 or lambda <= 1')
  M <- calcM(alpha,lambda)
  xvec <- runif(n, 0, 1)
  yvec <- gammafn(xvec, alpha,lambda)
  fvec <- M*gammafn(xvec,floor(alpha), (lambda-1))
  #fvec <- runif(1,0,exp(-x/lambda))
  xvec[yvec < fvec]
}
gammaRejSampling<- function(nsamp,alpha,lambda){
     n = 1000000
     x <- acceptFn(n,alpha,lambda)
     rprob <- length(x)/n
     rprob
     desired_n= floor(nsamp/rprob+1)
     desired_n

     x <- acceptFn(desired_n,alpha,lambda)
     #length(x)
     sample(x,nsamp)
}
                   ####    
s1 <- gammaRejSampling(1000,2.3,2)
s1
pxGT4 <- length(s1[s1>4])/1000
pxGT4      # P(X>=4) = 0
                   ####### 
n = 10000000
x <- acceptFn(n,2.3,2)
rprob <- length(x)/n
rprob 
desired_n= floor(1000/rprob)
desired_n 

                    ################# Importance function
importanceFn<-function (n,alpha,lambda){ 
  if (alpha <= 1 || lambda <= 1) stop('alpha <= 1 or lambda <= 1')
  
  xvec <- runif(n, 0, 1)
  yvec <- gammafn(xvec, floor(alpha),lambda)
  gvec <- gammafn(xvec,floor(alpha), (lambda-1))
  wtvec <- yvec/gvec
  c(mean(wtvec*gvec), var(wtvec*gvec))
}
            
normalgamma<-  function (n,alpha,lambda){ 
  if (alpha <= 1 || lambda <= 1) stop('alpha <= 1 or lambda <= 1')
  xvec <- runif(n, 0, 1)
  yvec <- gammafn(xvec, alpha,lambda)
  c(mean(yvec), var(yvec))
}
normalgamma(100,2.3,2)
#[1] 0.61131441 0.04521426
importanceFn(100,2.3,2)
#[1] 0.58098673 0.03015328

