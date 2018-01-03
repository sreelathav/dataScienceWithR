library(forecast)
require(TSA)
y=AirPassengers
tsdisplay(y) #trend ,seasonal
adf.test(y)   #dicker-Fuller test , reject null, no unit roots 
TC=ma(y,12)
#For forecasting purposes we’d like to fit a linear model to this trend. 
#You can do this as follows.
linear_tc=lm(TC~time(y))
#We can get an even clearer picture of the seasonal period 
#by plotting the de-trended time series.
tsdisplay(y-TC)
#
#In R we can do this quickly for all 12 seasonal parts (months) 
#by creating a matrix of AirPassengers/T C and then taking the row means. 
#Finally, to get all 12 years we simply repeat our
#estimated seasonal components 12 times.
#
pseudo_s = y/TC
matrix_s = matrix(pseudo_s,nrow=12)
s = rowMeans(matrix_s,na.rm=TRUE)
S = rep(s,length(y)/12)
S = S/mean(S)
#Now that we have both the estimated trend and estimated seasonal component, 
#we can estimate the random component:
R=y/(TC*S)
#################using decompose function or stl
?decompose()
require(graphics)
sc <- decompose(y, type="multiplicative")
sc$figure
plot(sc)
##############forecast

ftime=seq(time(y)[length(y)]+deltat(y),length=24,by=deltat(y)) # future times
d_fy=(linear_tc$coef[1]+linear_tc$coef[2]*ftime)*rep(S[1:12],2)
#
fit=Arima(R,order=c(2,0,1))
fy=forecast(fit,h=30)
msef=d_fy*fy$mean[-c(1:6)]
#95% CI
y.low95=d_fy*fy$lower[-c(1:6),2]
y.high95=d_fy*fy$upper[-c(1:6),2]
#plot
plot(y,xlim=c(time(y)[1],ftime[length(ftime)]),ylim=c(0,700))
lines(ftime, msef, col=4)
polygon(c(ftime, rev(ftime)), c(y.high95, rev(y.low95)),
        col=rgb(0,0,1,alpha=0.1),border=NA)
