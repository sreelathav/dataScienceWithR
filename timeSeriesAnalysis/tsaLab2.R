library(forecast)
xAR=arima.sim(list(ar=0.5), 200)
xAR=arima.sim(list(ar=c(0.5,0.25), order=c(2,0,0)), 200)
tsdisplay(xAR)
xMA=arima.sim(list(ma=0.8), 200)
xMA=arima.sim(list(ma=c(0.6,0.7), order=c(0,0,2)), 200)
tsdisplay(xMA)
xARMA=arima.sim(list(ar=0.5, ma=0.25,order=c(1,0,1)), 200)
tsdisplay(xARMA)
xARIMA=arima.sim(list(ar=0.5, ma=0.25,order=c(1,1,1)), 200)
tsdisplay(xARIMA)

ARMAacf(ar=0.5, ma=-0.2679492, lag.max=2)
ARMAacf(ar=0.5, ma=-0.2679492, lag.max=2, pacf=TRUE)
#

#                             TSA Lab2


# load, explore data
library(forecast)
require(lmtest)
lab2data = read.csv("lab2.csv", header=TRUE)
names(lab2data)

#model7 = arima(lab2$Series7, order=c(p,d,0))
series7 <- lab2data[,1]
series8 <- lab2data[,2]
series9 <- lab2data[,3]



par(mfrow=c(1,2))
acf(series7)
pacf(series7)
# differencing series 7
df1s7 <- diff(series7)
tsdisplay(df1s7)
acf(df1s7)
pacf(df1s7)
#
# differencing series 7
df2s7 <- diff(df1s7)
acf(df2s7)
pacf(df2s7)
model7 = arima(series7, order=c(0,0,2))
tsdisplay(series7)
coeftest(model7)
model7=arima(series7, order=c(2,0,0),include.mean=FALSE)
coeftest(model7)
plot(fitted(model7),series7)
qqnorm(residuals(model7))
qqline(residuals(model7))
shapiro.test(residuals(model7))
Box.test(residuals(model7),type='Ljung-Box',lag=2+10,fitdf=2)



# load, explore data
lab2data = read.csv("lab2.csv", header=TRUE)
names(lab2data)

#model7 = arima(lab2$Series7, order=c(p,d,0))
series7 <- lab2data[,1]
series8 <- lab2data[,2]
series9 <- lab2data[,3]
######################     series 8
####################  Excercise 1 (a)
par(mfrow=c(1,2))
acf(series8)
pacf(series8)
#diff 1 series 8
df1s8 <- diff(series8)
acf(df1s8)
pacf(df1s8)
#diff 2 series 8
df2s8 <- diff(df1s8)
acf(df2s8)
pacf(df2s8)
# diff1 acf and diff 2 acf giving the similar structure -> MA(1) with d=1
#diff 3 series 8          
#df3s8 <- diff(df2s8)
#acf(df3s8)
#pacf(df3s8)
#              overfitting
#####   time series display of differenced series 8
tsdisplay(series8)
tsdisplay(df1s8) #non stationary - derived from series8
########       model testing series 8
model8arima011=arima(series8,order=c(0,1,1),include.mean=FALSE)
coeftest(model8arima011) #ma=-0.64
ARMAacf(ma=-0.64, lag.max=2, pacf=TRUE)
#acf=-0.4540295 pacf=-0.2596724
#
plot(residuals(model8arima011))
plot(fitted(model8arima011),series8)
qqnorm(residuals(model8arima011))
qqline(residuals(model8arima011))
shapiro.test(residuals(model8arima011))
#
model8arima012=arima(series8,order=c(0,1,2),include.mean=FALSE)
coeftest(model8arima012) #ma1 =-0.586, ma2 = -0.086
#
model8arima111=arima(series8,order=c(1,1,1),include.mean=FALSE)
coeftest(model8arima111) #ar1=0.143, ma1=-0.725
ARMAacf(ar=0.143,ma=-0.725, lag.max=2, pacf=TRUE)
#acf=-0.3957150 pacf=-0.2527569
#
plot(residuals(model8arima111))
plot(fitted(model8arima111),series8)
qqnorm(residuals(model8arima111))
qqline(residuals(model8arima111))
shapiro.test(residuals(model8arima111))



                 ##########  Excercise 1 (b)
model8=arima(series8, order=c(1,1,1),include.mean=FALSE)
coeftest(model8)
#
par(mfrow=c(1,3))
plot(fitted(model8),series8)
plot(residuals(model8))
qqnorm(residuals(model8))
qqline(residuals(model8))
#
shapiro.test(residuals(model8)) #p-value=0.375
Box.test(residuals(model8),type='Ljung-Box',lag=2+10,fitdf=2)
#X-squared = 3.4743, df = 10, p-value = 0.968
#
par(mfrow(c(1,2)))
acf(series7)
pacf(series7)
#pacf showed 2 significant lines, possible AR(2)
model7=arima(series7, order=c(1,0,1), include.mean=FALSE)
coeftest(model7)
#
par(mfrow=c(1,3))
plot(fitted(model7),series7)
plot(residuals(model7))
qqnorm(residuals(model7))
qqline(residuals(model7))
shapiro.test(residuals(model7)) #p-value = 0.504
Box.test(residuals(model7),type='Ljung-Box',lag=2+10,fitdf=2) 
#X-squared = 5.4427, df = 10, p-value = 0.8597
par(mfrow=c(1,1))
#
#
######################     series 9
####################  Excercise 1 (c)
par(mfrow=c(1,2))
acf(series9)
pacf(series9)
tsdisplay(series9) #not clear
#diff 1 series 9
df1s9 <- diff(series9)
tsdisplay(df1s9) #acf has 3 

#diff 2 series 9        
df2s9 <- diff(df1s9)
tsdisplay(df2s9)    #acf has 2
#diff 3 series 9        
df3s9 <- diff(df2s9)
tsdisplay(df3s9)     #acf has 3   not clear
#
#trial and error
#

model9=arima(series9, order=c(1,0,1),include.mean=FALSE)
coeftest(model9)
shapiro.test(residuals(model9)) #p-value = 0.9035 normal resid
Box.test(residuals(model9),type='Ljung-Box',lag=2+10,fitdf=2)
#X-squared = 5.4138, df = 10, p-value = 0.8619
#acf 
auto.arima(series9)
####################  Excercise 1 (d)
#
model9arma11=arima(series9, order=c(1,0,1),include.mean=FALSE)
coeftest(model9arma11)

#
model9arma12=arima(series9, order=c(1,0,2),include.mean=FALSE)
coeftest(model9arma12)
#
model9arma21=arima(series9, order=c(2,0,1),include.mean=FALSE)
coeftest(model9arma21)
#
####################  Excercise 2
r9 <- residuals(model9arma11)
qqnorm(r9)
qqline(r9)
par(mfrow=c(1,2))
acf(r9,main="ARMA11")

shapiro.test(r9)#p-value =0.9
Box.test(r9,type='Ljung-Box',lag=2+10,fitdf=2)
#X-squared = 5.4138, df = 10, p-value = 0.8619
#
model9ar2=arima(series9, order=c(2,0,0),include.mean=FALSE)
coeftest(model9ar2)
acf(residuals(model9ar2),main="AR2")
pacf(residuals(model9ar2),main="AR2")
shapiro.test(residuals(model9ar2))
Box.test(residuals(model9ar2),type='Ljung-Box',lag=2+10,fitdf=2)
#X-squared = 74.682, df = 10, p-value = 5.486e-12

################### Excercise 3
model9ar10=arima(series9, order=c(10,0,0),include.mean=FALSE)
coeftest(model9ar10)
qqnorm(residuals(model9ar10))
qqline(residuals(model9ar10))
acf(residuals(model9ar10)) #not all of them zero
shapiro.test(residuals(model9ar10)) #p-value 0.907
Box.test(residuals(model9ar10),type='Ljung-Box',lag=2+10,fitdf=2)
#X-squared = 15.947, df = 10, p-value = 0.1011


################################################
# Model Identification
# • Do the simulated series always look like they are a series simulated from the
# requested model?
# No, especially if φ ' 1 or some of φ i ' 0, etc. Also, the smaller the time series the harder
# it is to be sure of (p, d, q)
# • What model do you identify for Series 7?
# exponential decay of ACF, first two lags non-null for PACF so AR(2) ≡ ARIMA(2,0,0).
# Note the series may need to be differenced before you can identify the correct ARMA
# model. Check your notes to help identify how to interpret the order of an ARMA model from
# the acf and pacf.
# 4
# ARMAacf
# Try looking at the results of ARMAacf(). This function returns theoretical autocorrelations
# and partial autocorrelations for ARMA models given AR and / or MA parameters.
# • Run the command:
#   ARMAacf(ar=0.5, ma=-0.2679492, lag.max=2)
# and compare to the results you got for Exercise 3(3) of Assignment 3. Does
# this make sense?
# Yes, these values of φ and θ give us the same values of ρ 1 and ρ 2 that yielded the moment
# estimators φ̂ and θ̂ in the assignment.
# • Also try
# ARMAacf(ar=0.5, ma=-0.2679492, lag.max=2, pacf=TRUE)
# to get the partial autocorrelations.
# 5
# Parameter Estimation
# There are multiple options in R for fitting ARIMA models; Arima() from the forecast pack-
#   age, arima() from the stats package, and arima() from the TSA package. The last two have
# the same name and are almost identical but the TSA version has some additional functionality.
# The first argument to arima() is the time series data and the second is the order (p, d, q).
# • Fit an ARMA model to Series 7 as follows, where p is the order you identified
# and d the number of differences you performed.
# model7 = arima(lab2$Series7, order=c(p,d,0))
# To perform a significance test on the parameter estimated use coeftest() from the lmtest
# package.
# 2require(lmtest)
# coeftest(model7)
# Parameters that return non-significant p-values should be considered for removal from the
# model. Here the mean comes out to not significant. You can fit a model with a mean set to
# zero using
# model7=arima(lab2$Series7, order=c(p,d,q),include.mean=FALSE)
# 6
# Diagnostics
# Recall the 5 steps to perform model diagnostics by examining the residuals of the fitted ARIMA
# model:
#   1. Plot the residuals versus time.
# 2. qq-plot of the residuals.
# 3. Plot residuals versus the fitted values.
# 4. ACF plot of residuals and Ljung-Box test.
# 5. PACF plot of residuals.
# You can extract the residuals from a fitted model obtained from arima() using the residuals()
# function.
# • Use qqnorm(residuals(model7)) to examine normality of the residuals.
# • Use qqline(residuals(model7)) to add a (0,1) line.
# • Use shapiro.test(residuals(model7)) perform a formal hypothesis test for nor-
#   mality.
# • Use Box.test(residuals(model7),type=”Ljung-Box”,lag=p+q+10,fitdf=p+q) to
# perform the χ 2 K−p−q test on the autocorrelations of the residuals. The lag argument is K
# in the notes. What should you set it to? What is the test statistic? What are we testing
# here?
# The lag should be set to capture al the non-null ACFs and PACFs, so at least p + q + 1.
# The statistic is the weighted sum of the squared residuals:
#   Q = n(n + 2)
# K
# X
# k=1
# 
# 
# (ε̂) 2
# ρ̂ k
# n − k
# .
# The test is whether the sum of the squared autocorrelations of the residuals are what
# we’d expect from a white noise.
# 3Exercises
# Exercise 1.
# (a) For Series 8, identify the number of differences (if any) and order of the ARMA model for
# the stationary series based on the results of tsdisplay(). Give reasons for your answers.
# • Series8: Linear decrease of ACF so take differences. For ∆(Series 8) we see first lag
# only is non-null for ACF and exponential decay of PACF so MA(1). Therefore Series
# 8 is ARIMA(0,1,1).
# (b) Report the AR and / or MA parameter estimates for Series 7 and 8, with no drift by using
# include.mean=FALSE. Are they statistically significant?
# • Series7: φ = (0.309440, 0.204663). Both significant right down to level 1 × 10 −5 .
# • Series8: θ = −0.6452. Significant at any level.
# (c) Look at Series 9. What type of model should you choose and why? Again, focus on models
# without drift (i.e. mean zero).
# Exponential decay of ACF and PACF, so a mixed ARMA(p > 0, q > 0) model. We can’t
# tell p or q from the plots so try fitting simplest model and build it up one parameter at a
# time until something comes out as not significant.
# (d) Now try ARMA(1,1), ARMA(1,2) and ARMA(2,1) models fit to Series 9. Do the parame-
#   ters come out to be statistically significant? Why?
# ARMA(1,1) is best as it is simplest that is consistent with ACF and PACF plots and has
# all significant parameters. The others are overfitting; there’s more parameters than are
# required so they are not statistically significantly different from zero.
# Exercise 2. Examine the residuals of an ARMA(1,1) model fit to Series 9 and the ACFs of
# the residuals of an AR(2) fit to Series 9. Describe what you find.
# fitAR=arima(lab2$Series9, order=c(2,0,0),include.mean=FALSE)
# resAR=residuals(fitAR)
# fitARMA=arima(lab2$Series9, order=c(1,0,1),include.mean=FALSE)
# resARMA=residuals(fitARMA)
# e.g. where fit is the output of residuals and res is the output of residuals(), the 5 steps are:
#   1. Plot the residuals versus time: plot(res,t=’p’)
# 2. qq-plot of the residuals: qqnorm(res);qqline(res);shapiro.test(res)
# 3. Plot residuals versus the fitted values: plot(fitted(fit), res)
# 4. ACF plot of residuals: acf(res) and Ljung-Box test: Box.test(res,type=”Ljung-
#                                                                   Box”,lag=10) where lags is chosen to capture the potentially important lags.
# 5. PACF plot of residuals: pacf(res)
# 4• AR(2) model: Step 1 no pattern, Step 2 normally distributed, Step 3 no pattern, Step
# 4 plot looks wrong and p-value is 5.637 × 10 −12 at lag=10, Step 5 plot looks
# wrong. So we reject this model.
# • ARMA(1,1) model: Step 1 no pattern, Step 2 normally distributed, Step 3 no pattern,
# Step 4 plot looks ok and p-value is 0.8989 at lag=10, Step 5 plot looks ok. So we don’t
# reject this model.
# Exercise 3. Now fit an AR(10) model to Series 9. Are the parameters significant? Examine
# the residuals. Why might this model fit the Series well?
# fitAR10=arima(lab2$Series9, order=c(10,0,0),include.mean=FALSE)
# resAR10=residuals(fitAR)
# • Parameters all significant except φ 10 according to coeftest(fitAR10).
# • Step 1: Residuals show no pattern against time.
# • Step 2: Residuals appear normally distributed.
# • Step 3: Residuals show no pattern against fitted values.
# • Step 4: ACF plot looks ok and Ljung-Box test give p-value of 0.2548 at lag=10.
# • Step 5: PACF plot looks a little concerning as multiple values are close to significant.
# The reason this model fits the time series quite well is because of invertibility. Any ARMA(p,q)
# model with roots of the MA polynomial less than 1 in absolute value may be written as AR(∞)
# model. Thus for large p it is well approximated by an AR(p) model.
