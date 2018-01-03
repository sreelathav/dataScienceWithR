# ARIMA order guessing game! User selects number of timepoints, computer samples p,d,q then plots time series, ACF, and PACF and asks for guesses for order of ARIMA
# Guessing order of integration is interactive with any non-stationarity guess causing differencing of the series followed by new plots and guesses
require(TSA)
require(forecast)
stationary_AR2=function(phi) # function to check for stationarity conditions of an AR process based on phi
{
  # check 3 conditions on p72 of Cryer and Chan)
  if (((phi[1]+phi[2])<1) & (phi[2]-phi[1]<1) & (abs(phi[2])<1))
    return (TRUE) else return (FALSE)
}
samp_coefs=function(l)
{
  vec=rep(0,l)
  for (i in 1:l) vec[i]=runif(1,0.2,1)*sample(c(-1,1),1) # magnitude uniformly b/w 0.2 and 1 and random sign
  return(vec)
}
plot_and_guess_d=function(y)
{
  enterdelta=function(ntries)
  {
    d.local=NaN
    if (ntries>0)
      print("please enter n or y only")
    delta=readline("Difference the time series?: N/y ")
    if (delta=="") delta="n" # default guess is no
    if (delta=="y" | delta=="Y")
      d.local=1
    if (delta=="n" | delta=="N") 
      d.local=0
    return (d.local)
  }
  # plot the time series, ACF, PACF
  par(mfrow=c(1,3));plot(y,t='l');acf(y);pacf(y)
  ntries=0
  dd=NaN
  while (is.nan(dd))
  {
    dd=enterdelta(ntries)
    ntries=ntries+1
  }
  if (dd==1)
  {
    print("taking d differences...")
    y=base::diff(y,lag=1)
  } 
  return(list(d=dd,y=y))
}


play_arima_game=function()
{
  print("Welcome to the ARIMA order guessing game!")
  print("For simplicity any ARMA models that have both AR and MA components will be order 1 in both")

  n=sample(10:1e3,1) # length of time series
  cat("Simulating a time series of length", n, "\n")

  # sample p,d,q randomly; p from [0,1,2]; q from [0,1,2]; d from [0,3]
  p=sample(0:2,1)
  q=sample(0:2,1)
  d=sample(0:2,1) 
  # if p and q both > 0 make them both 1
  if ((p>0) & (q>0)) 
  {
    p=1;q=1
  }
  sim.order=c(p,d,q)
  # now simulate a time series of order (p,d,q)
  phi=NULL; if (p>0) phi=samp_coefs(p) # sample values b/w -1 and 1
  if (p==2) 
  {
    # resample values for phi b/w -1 and 1 until staionarity is ok
    while (!stationary_AR2(phi))
      phi=samp_coefs(p) # sample values b/w -1 and 1
  }
  theta=NULL; if (q>0) theta=samp_coefs(q) # sample values b/w -1 and 1
  y=arima.sim(model=list(ar=phi,ma=theta,order=c(p,d,q)),n=n)

  # first allow the forecast package to make an automated guess
  auto.res=auto.arima(y,allowdrift=FALSE,seasonal=F)
  if (length(auto.res$model$Delta)==0) auto.res$model$Delta=0
  auto.order=c(auto.res$arma[1], auto.res$arma[length(auto.res$arma)-1], auto.res$arma[2])
  auto.correct=ifelse(all(auto.order==sim.order),"(correctly)", "(incorrectly)")

  d.tmp=1 # start with 1 to force a first guess
  d.guess=0 # running total of differences to make final guess
  while (d.tmp>0)
  {
    res=plot_and_guess_d(y)
    y=res$y
    d.tmp=res$d
    d.guess=d.guess+d.tmp
  }
  # now that we have an ARMA, guess p and q
  p.guess=as.integer(readline("enter guess for p: "))
  q.guess=as.integer(readline("enter guess for q: "))

  if ((p.guess==p) & (d.guess==d) & (q.guess==q))
  {
    cat("well done! It in indeed order", sim.order, "\n")
  } else cat("Wrong! You guessed", c(p.guess,d.guess,q.guess),  "Truth is ", c(p,d,q), "\n")
  cat("coefficients used to simulate the data were:")
  if (p>0) cat(" phi=", phi)
  if (q>0) cat(" theta=", theta)
  cat("\n")

  # compare with how an automated guess based on BIC does
  cat("R function auto.arima guessed", auto.order, auto.correct, "\n")
}
