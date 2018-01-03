# Load libraries needed.
library(rpart)
library(nnet)
library(mvtnorm)
library(adabag)

# Set seed of random number generator
set.seed(1024)

# Simulate training data with two different groups
#X are the data, l are the labels
# The data are stored as a data frame called dat
X <- rmvnorm(100,c(0,0),diag(2))
X[1:50,] <- X[1:50,]+1
X[51:100,] <- X[51:100,]-1
l <- rep(1:2,c(50,50))
l <- as.factor(l)
dat <- data.frame(X,l)
colnames(dat) <- c("x1","x2","l")
N<-nrow(dat)

# Plot the data 
# Axes are fixed so plots look similar across iterations
plot(X,col=as.numeric(l),pch=as.numeric(l),ylim=c(-4.2,4),xlim=c(-4.2,4))

# Generate the test data in a similar manner
Xnew <- rmvnorm(100,c(0,0),diag(2))
Xnew[1:50,] <- Xnew[1:50,]+1
Xnew[51:100,] <- Xnew[51:100,]-1
lnew <- rep(c(1,2),c(50,50))
datnew <- data.frame(Xnew,lnew)
colnames(datnew) <- c("x1","x2","l")
Nnew<-nrow(datnew)

# Use the boosting() function in the adabag package
fit <- boosting(l~.,data=dat)

# Test on both datasets
# Look at the performance in training and test data
pred <- predict(fit,newdata=rbind(dat,datnew))
table(l,pred$class[1:N])
table(lnew,pred$class[(N+1):(N+Nnew)])

# Plot the results. 
# We will generate a heatmap to demonstrate how the classifier would classify new observations
# In the plot of the classifier. 
# The redder the region, the more likely it is a triangle.
# The yellower the region, the more likely it is a circle. 

L<-400;
xseq<-seq(-4,4,length=L)
yseq<-seq(-4,4,length=L)
datgrid<-expand.grid(xseq,yseq)
datgrid<-cbind(datgrid,NA)
colnames(datgrid)<-c("x1","x2","l")
datgrid<-as.data.frame(datgrid)

predgrid<-predict(fit,newdata=rbind(dat,datgrid),type="class")$class[-(1:N)]
predgrid<-predict(fit,newdata=rbind(dat,datgrid))$prob[-(1:N),1]
predgrid<-as.numeric(predgrid)
image(xseq,yseq,matrix(predgrid,L,L))

points(X,pch=as.numeric(l))	