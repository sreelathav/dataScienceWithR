# Load the kernlab package
library(kernlab)
# Load the vignette for the package
vignette("kernlab")
library(mclust)
#load data
data(diabetes)
str(diabetes)
set.seed(1024)
index <- sample(1:dim(diabetes)[1])
datatrain <- diabetes[index[1:floor(dim(diabetes)[1]/2)], ]
datavalid <- diabetes[index[((floor(dim(diabetes)[1]/2)) + 1):ceiling(dim(diabetes)[1]*3/4)], ]
datatest <- diabetes[index[((ceiling(dim(diabetes)[1]*3/4)) + 1):dim(diabetes)[1]], ]
pairs(diabetes) 
## train a support vector machine
#Gaussian Radial Basis kernel function. Hyperparameter : sigma = 0.05
# rbf
rbf <- rbfdot(sigma = 0.05)
filter <- ksvm(class~.,data=datatrain,kernel="rbfdot",
               kpar=list(sigma=0.05),C=5,cross=3)

filter


## predict class type on the validation set
rbfclass <- predict(filter,datavalid)

## Check results
tab.r <- table(rbfclass,datavalid$class)
tab.r
rbfAccu <-  sum(diag(tab.r))/sum(tab.r)
rbfAccu

# polydot
filter <- ksvm(class~.,data=datatrain,kernel="polydot",
               kpar=list(degree=2,offset=1),C=5,cross=3)

filter
## predict class type on the validation set
polyclass <- predict(filter,datavalid)

## Check results
tab.p <- table(polyclass,datavalid$class)
tab.p
polyAccu <-  sum(diag(tab.p))/sum(tab.p)
polyAccu
#tanhdot
filter <- ksvm(class~.,data=datatrain,kernel = "tanhdot",
                kpar=list(offset=1),C=10,cross=3)

filter
## predict class type on the validation set
tanhclass <- predict(filter,datavalid)

## Check results
tab.h <- table(tanhclass,datavalid$class)
tab.h
tanhAccu <-  sum(diag(tab.h))/sum(tab.h)
tanhAccu
#laplacedot
filter <- ksvm(class~.,data=datatrain,kernel = "laplacedot",
               kpar=list(sigma=0.05) ,C=10,cross=3)

filter
## predict class type on the validation set
laplaceclass <- predict(filter,datavalid)

## Check results
tab.l <- table(laplaceclass,datavalid$class)
tab.l
lapAccu <-  sum(diag(tab.l))/sum(tab.l)
lapAccu
## predict class type on the test set
filter <- ksvm(class~.,data=datatrain,kernel="polydot",
               kpar=list(degree=2,offset=1),C=5,cross=3)

filter
polyclass <- predict(filter,datatest)
#plot(polyclass, data = datatest)
## Check results
tab.ptest <- table(polyclass,datatest$class)
tab.ptest
polytestAccu <-  sum(diag(tab.ptest))/sum(tab.ptest)
polytestAccu
