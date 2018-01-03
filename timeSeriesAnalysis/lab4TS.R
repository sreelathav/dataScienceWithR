                         #      Lab 4 TSA
lab4=scan("lab4.dat")
library(forecast)
tsdisplay(diff(lab4))  #1000 items
require(TSA)
adf.test(lab4)
#Dickey-Fuller = -3.0443, Lag order = 9, p-value = 0.1362
#alternative hypothesis: stationary
adf.test(diff(lab4))
#Dickey-Fuller = -9.3355, Lag order = 9, p-value = 0.01
#alternative hypothesis: stationary
ima1.lab4=arima(lab4,order=c(0,1,1))
res.lab4=residuals(ima1.lab4)
McLeod.Li.test(y=res.lab4) # all below critical level
#The ARIMA residuals fail McLeod-Li so there is evidence of ARCH type behaviour in the
#model. We can also see this by running:
tsdisplay(res.lab4^2)
#Significant ACF and PACF terms of the squared residuals are clearly visible. We can also use
#this plot to choose p and q for the GARCH(p,q).

require(fGarch)
#
fit=garchFit(~arma(0,1)+garch(1,0),diff(lab4),include.mean=F)
plot(fit@residuals,t='l')
plot(fit@residuals/fit@sigma.t,type='h',ylab='standard residuals')
plot(fit@sigma.t^2,type='l',ylab='conditional variances')
acf((fit@residuals/fit@sigma.t)^2)
Box.test((fit@residuals/fit@sigma.t)^2, lag=10, t='Ljung')

######               Lab4 code
library(forecast)
require(TSA)
require(fGarch)
data(google)
google=google-mean(google)
####                        Excercise 1(a)
tsdisplay(google)
auto.arima(google) #2,0,2
adf.test(google)   #series stationary
#From the ACF and PACF of the daily returns, we see that the data are essentially uncorrelated
#over time.
#(b) Test the data for conditional heterskedasticity and report the result.
#McLeod.Li.test(y=google)
#Clearly significant at any max lag K.
#                          Excercise 1(b)
            # test for conditional heteroskedasticity
fit.google = arima(google,order=c(2,0,2))
res.google=residuals(fit.google)
McLeod.Li.test(y=res.google) # all below critical level
tsdisplay(res.google^2)
                ##garch fit    Excercise 1(c)
fitgarch=garchFit(~arma(1,2)+garch(2,1),google,include.mean=F)
summary(fitgarch)
#The ACF and PACF plots of google show the ARMA order to be (0, 0). Examining
#the ACF and PACF of the squared series (equivalent to the squared residuals from an
#                                        ARMA(0,0) fit):
  tsdisplay(googleˆ2)
#Both are exponentially decaying, showing an ARMA model with max(p, q) and p both
#non-zero. Therefore we should try a GARCH(1,1) model and then check for significance of
#parameters.
fit=garchFit(~arma(0,0)+garch(1,1),google,include.mean=F)
summary(fit) # all terms significant so we need p > 0, q > 0
fit=garchFit(~arma(0,0)+garch(2,1),google,include.mean=F)
summary(fit) # q=2 term not significant
fit=garchFit(~arma(0,0)+garch(1,2),google,include.mean=F)
summary(fit) # p=2 term not significant
#Therefore revert to
fit=garchFit(~arma(0,0)+garch(1,1),google,include.mean=F)
#Note that the Li-Mak ARCH test at end is not failed, thus there appears to be no additional
#ARCH type behaviour not captured by the model.
                    ###        Excercise 1(d)
plot(fitgarch@residuals/fitgarch@sigma.t,type='h',ylab='standard residuals')
plot(fitgarch@sigma.t^2,type='l',ylab='conditional variances')
##(d) Plot the conditional variances and the standardised residuals.
#plot(fit@sigma.tˆ2,type=’l’,ylab=’conditional variances’)
#plot(fit@residuals/fit@sigma.t,type=’h’,ylab=’standard residuals’)
#(e) Create a plot and run a Ljung-Box test for whether the squared standardised residuals are
#autocorrelated. 2
#res2=(fit@residuals/fit@sigma.t)ˆ2

                       ###        Excercise 1(e)
acf((fitgarch@residuals/fitgarch@sigma.t)^2)
Box.test((fitgarch@residuals/fitgarch@sigma.t)^2, lag=10, t='Ljung')
#X-squared = 5.2498, df = 10, p-value = 0.8739
#acf(res2)
#Box.test(res2,t=’L’,lag=10)
#The test is not failed showing that there is not evidence of ARCH in the residuals of our
#fitted model.