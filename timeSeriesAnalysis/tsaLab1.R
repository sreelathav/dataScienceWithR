#                             TSA Lab 1


# load, explore data
lab1data = read.csv("lab1.csv", header=TRUE)
names(lab1data)
par(mfrow=c(2,1))
acf(lab1data[,1])
pacf(lab1data[,1])

##########
#Ex1:                   series  1
par(mfrow=c(1,1))
plot(lab1data[,1], type="l", main = "Series 1")
head(lab1data[,1])
summary(lab1data[,1])

par(mfrow=c(1,2)) 
plot(lab1data[,2], type="l", main = "Series 1")
acf(lab1data[,1], main = "ACF of Series 1")
pacf(lab1data[,1], main = "PACF of Series 1")

#Ex2
                 #Series 2 Acf and Pacf
par(mfrow=c(1,2)) 
acf(lab1data[,2], main = "ACF of Series 2")
pacf(lab1data[,2], main = "PACF of Series 2")
                 ##Series 3 Acf and Pacf
par(mfrow=c(1,2)) 
acf(diff(lab1data[,3]), main = "ACF of Series 3")
pacf(diff(lab1data[,3]), main = "PACF of Series 3")
#

#Ex3
par(mfrow=c(1,3))
plot(lab1data[,4], type="l", main = "Series 4")
plot(lab1data[,5], type="l", main = "Series 5")
plot(lab1data[,6], type="l", main = "Series 6")

#Ex4
series4diff = diff(lab1data[,4], lag=1)
series5diff = diff(lab1data[,5], lag=1)
series6diff = diff(lab1data[,6], lag=1)
#plots
plot(series4diff, type="l", main = "diff Series 4")
plot(series5diff, type="l", main = "diff Series 5")
plot(series6diff, type="l", main = "diff Series 6")


#Ex 5
timet = 1:nrow(lab1data)
timet
regSeries4 = lm(lab1data[,4]  ~ timet)
summary(regSeries4)

par(mfrow=c(1,2))
plot(lab1data[,4], type="l", main = "Series 4")
plot(fitted(regSeries4), type ="l", main = "Fitted Value S4")

par(mfrow=c(1,2))
acf(lab1data[,4], main = "ACF Series 4")
acf(resid(regSeries4), main = "ACF of residuals S4")

par(mfrow=c(2,2))
plot(regSeries4)

par(mfrow=c(1,1))
#

