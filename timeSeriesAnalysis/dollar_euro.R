y=read.table("weekly_exchange.txt",skip=1)[,2] # read in the data; only need second column
#getwd()
require(TSA)
par(ask=T) # pause for user to hit enter after each plot

plot(y,t='l') # plot the time series

# looks non-stationary; let's check the ACF
acf(y)

# take first differences and try again
z=diff(y)
plot(z,t='l') # looks ok; check ACF again
acf(z) # ok; check PACF
pacf(z) # ok; so we needed to take differences just once. Therefore d=1

# based on ACF and PACF, either could be null after lag 1 or exponentially decaying
# so we have either an ARMA(0,1) a.k.a. MA(1), ARMA(1,0) a.k.a AR(1), or ARMA(1,1) model for z. 
# i.e. ARIMA(0,1,1), ARIMA(1,1,0), or ARIMA(1,1,1) for y

# next estimate the largest model and check the coefficients
fit11=arima(y,order=c(1,1,1))
print(fit11) # summary of the fit
# calculate p-values based on a z-test of the estimated coefficients
pvals=2*(1-pnorm(fit11$coef,mean=c(0,0),sd=sqrt(diag(fit11$var.coef))))
# reject hypothesis that MA term is zero but fail to reject hypothesis that AR terms is zero
# so we default back to the ARIMA(0,1,1) model for y
fit01=arima(y,order=c(0,1,1))
# theta=0.42 as per slides
# now check the residuals
qqnorm(residuals(fit01));qqline(residuals(fit01))
shapiro.test(residuals(fit01)) # reject null hypothesis that the residuals follow a normal dist

acf(residuals(fit01))
pacf(residuals(fit01))
