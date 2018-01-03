set.seed(1234)
numSims <- 100000
y <- NULL
y[2]=0
epsilon<-  rnorm(numSims,0,1)
#MA
phi1=0
phi2 =0
theta1 = 0.6
theta2 = 0
for (t in 3:numSims) {
  y[t] <- phi1*y[t-1] +epsilon[t] + theta1*epsilon[t-1] + theta2*epsilon[t-2]
}
acf(y[3:numSims])[1]
pacf(y[3:numSims])[1]

#AR

numSims <- 100000
y <- NULL
y[1]=0
y[2]=0
epsilon<-  rnorm(numSims,0,1)
phi1=-0.6
phi2 =0
theta1 = 0
theta2 = 0
for (t in 3:numSims) {
  y[t] <- phi1*y[t-1] +phi2*y[t-2] + epsilon[t] + theta1*epsilon[t-1] + theta2*epsilon[t-2]
}
acf(y[3:numSims])
pacf(y[3:numSims])

#simulation
y<-arima.sim(list(c(3,1,1)),n=5000000)
acf(y)
pacf(y)
######
y = arima.sim(n = 1000000, list(ma = -0.6))
acf(y,main = "ACF (0,0,1)")[1]
pacf(y,main = "PACF (0,0,1)")[1]
y = arima.sim(n = 1000000, list(ar = -0.6))
acf(y,main = "ACF (1,0,0)")[1]
pacf(y,main = "PACF (1,0,0)")[1]

