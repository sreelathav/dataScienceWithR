library(forecast)
require(TSA)

data(beersales)
tsdisplay(beersales)
adf.test(beersales)

bs = beersales         #[61:length(beersales)]
##additive model, beersales = trend_component+seasonal+Random
#apply a moving average of order 12 to the whole time series. The last 6 months will not have a value in this smooth.
#Examine the last 12 trend timepoints for which you do have values. These will be points (n-17) to (n-6). 
#Fit a linear model / regress trend on time for those 12 timepoints. Use this for forecasting. 
trendc <- ma(bs,12)
par(mfrow=c(1,2))
plot(bs,type="l")
plot(trendc,type="l")
tsdisplay(beersales-trendc)
tail(trendc,18)



#### 2(b)
pseudo_s = bs-trendc
matrix_s = matrix(pseudo_s,nrow=12)
s = rowMeans(matrix_s,na.rm=TRUE)
S = rep(s,length(bs)/12)
S = S - mean(S)
plot(S,type="l")
##  Random component
R = bs-trendc-S
plot(R,type="l")
#####
?decompose()
require(graphics)
sc <- decompose(bs, type="additive")
sc$figure
plot(sc)
####


##############  forecast 
#                     Excercise 3
auto.arima(bs)
#####################(a)
NonNAindex <- which(!is.na(trendc))
n <- length(NonNAindex)
mt <-NonNAindex[n-11]:NonNAindex[n]  #to get 12 trend points from the recent years
linearTrend <-lm(tail(TC[!is.na(TC)],12)~tail(time(y)[which(!is.na(TC))],12)) #linear regression 
linearTrend
#

ftime=seq(time(bs)[length(bs)]+deltat(bs),length=24,by=deltat(bs)) # future times
d_fy=(linearTrend$coef[1]+linearTrend$coef[2]*ftime)+rep(S[1:12],2)
d_fy
# (b)Fit an ARIMA to the random component and check that it is a good fit.
auto.arima(R)
par(mfrow=c(1,1))
fit = Arima(R,order=c(2,0,2))
Rpt = forecast(fit,h=24)
Rpt
plot(Rpt)
#   
tcSRf   =  d_fy + Rpt$mean
tcSRf 
#95% CI
bs.low95  = d_fy + Rpt$lower[,2]
bs.high95 = d_fy + Rpt$upper[,2]
bs.low95
#plot
par(mfrow=c(1,1))
plot(bs,xlim=c(time(bs)[1],ftime[length(ftime)]),ylim=c(0,80))
lines(ftime, tcSRf , col=4)
polygon(c(ftime, rev(ftime)), c(bs.high95, rev(bs.low95)),
        col=rgb(0,0,1,alpha=0.1),border=NA)



