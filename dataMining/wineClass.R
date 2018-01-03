# Load the adabag library
library(adabag)

# Load the wine data
library(gclus)
data(wine)
wine$Class<-as.factor(wine$Class)

# Implement Bootstrap Aggregating
fit.b <- bagging(Class~.,data=wine)
pred.b <- predict(fit.b,newdata=wine,type="class")$class

# Fit a classification tree (same data)
fit.r <- rpart(Class~.,data=wine)
pred.r <- predict(fit.r,newdata=wine,type="class")

# Compare performance
table(wine$Class,pred.b)
table(wine$Class,pred.r)

# Plot examples of 9 trees
library(partykit)
par(mfrow=c(3,3))
for (j in 1:9)
{
	plot(fit.b$trees[[j]],main=paste("Example ",j,sep=""))
	text(fit.b$trees[[j]],use.n=TRUE,xpd=TRUE,col="red")
}
par(mfrow=c(1,1))

# Load the randomForest package
library(randomForest)

# Implement the random forest algorithm
fit.rf <- randomForest(Class~.,data=wine)

# Examine the results
fit.rf

# Let's look at variable importance
varImpPlot(fit.rf,type=1)
############################
# Using same data for fitting and testing

# Set seed of random number generator
set.seed(1000)

# Load the wine data
# The gclus package is needed just for the data
library(gclus) 
data(wine)

# Ensure that Class is categorical
is.factor(wine$Class)
wine$Class <- as.factor(wine$Class)

# Load the rpart and partykit libraries
library(rpart)
library(partykit)

# Use a classification tree to classify the data
fit.r <- rpart(Class~.,data=wine)
plot(as.party(fit.r))

# Classify the observations
pred <- predict(fit.r,type="class")

# Look at table (rows=truth, cols=prediction)
tab <- table(wine$Class,pred)
tab

# Work out the accuracy
sum(diag(tab))/sum(tab)
#############
# Using training and test data

# Set seed of random number generator
set.seed(1000)

# Load the wine data
# The gclus package is needed just for the data
library(gclus) 
data(wine)

# Ensure that Class is categorical
is.factor(wine$Class)
wine$Class <- as.factor(wine$Class)

# Load the rpart and partykit libraries
library(rpart)
library(partykit)

# Split the data
# 75% of values are used as training data
# The remaining 25% are used as test data

N <- nrow(wine)
indtrain <- sample(1:N,size=0.75*N)
indtrain <- sort(indtrain)
indtest <- setdiff(1:N,indtrain)

# Fit a classifier to only the training data
fit.r <- rpart(Class~.,data=wine,subset=indtrain)
plot(as.party(fit.r))

# Classify for ALL of the observations
pred <- predict(fit.r,type="class",newdata=wine)

# Look at the results for the test data only
pred[indtest]

# Look at table for the test data only (rows=truth, cols=prediction)
tab <- table(wine$Class[indtest],pred[indtest])
tab

# Work out the accuracy
sum(diag(tab))/sum(tab)

# Look at the results for the training data only
tab <- table(wine$Class[indtrain],pred[indtrain])
tab

# Work out the accuracy
sum(diag(tab))/sum(tab)

# Let's repeat this process to see if we were just unlucky!

# Set up res to store results

res<-matrix(NA,100,2)

# Start simulation to look at this 
iterlim <- 100
for (iter in 1:iterlim)
{
  
  # Split the data
  # 75% of values are used as training data
  # The remaining 25% are used as test data
  N <- nrow(wine)
  indtrain <- sample(1:N,size=0.75*N)
  indtrain <- sort(indtrain)
  indtest <- setdiff(1:N,indtrain)
  
  # Fit a classifier to the training data only
  fit.r <- rpart(Class~.,data=wine,subset=indtrain)
  
  # Classify for ALL of the observations
  pred <- predict(fit.r,type="class",newdata=wine)
  
  # Look at table for the test data only (rows=truth, cols=prediction)
  tab <- table(wine$Class[indtest],pred[indtest])
  
  # Work out the accuracy
  res[iter,1]<-sum(diag(tab))/sum(tab)
  
  # Look at the results for the training data only
  tab <- table(wine$Class[indtrain],pred[indtrain])
  
  # Work out the accuracy
  res[iter,2] <- sum(diag(tab))/sum(tab)
  
} # iter


# Check out the error rate summary statistics.
colnames(res)<-c("test","train")
apply(res,2,summary)
#######################
# Using bootstrapping to build training and test data

# Set seed of random number generator
set.seed(1000)

# Load the wine data
# The gclus package is needed just for the data
library(gclus) 
data(wine)

# Ensure that Class is categorical
is.factor(wine$Class)
wine$Class <- as.factor(wine$Class)

# Load the rpart and partykit libraries
library(rpart)
library(partykit)

# Bootstrap the data
# Use the bootstrap sample as training data
# Test the model on the remainder of the data
N <- nrow(wine)
indtrain <- sample(1:N,replace=TRUE)
indtrain <- sort(indtrain)
indtest <- setdiff(1:N,indtrain)

# Fit a classifier to only the training data
fit.r <- rpart(Class~.,data=wine,subset=indtrain)
plot(as.party(fit.r))

# Classify for ALL of the observations
pred <- predict(fit.r,type="class",newdata=wine)

# Look at the results for the test data only
pred[indtest]

# Look at table for the test data only (rows=truth, cols=prediction)
tab <- table(wine$Class[indtest],pred[indtest])
tab

# Work out the accuracy
sum(diag(tab))/sum(tab)

# Look at the results for the training data only
tab <- table(wine$Class[indtrain],pred[indtrain])
tab

# Work out the accuracy
sum(diag(tab))/sum(tab)

# Let's repeat this process to see if we were just unlucky!

# Set up res to store results

res<-matrix(NA,100,2)

# Start simulation to look at this 
iterlim <- 100
for (iter in 1:iterlim)
{
  
  # Bootstrap the data
  # Use the bootstrap sample as training data
  # Test the model on the remainder of the data
  N <- nrow(wine)
  indtrain <- sample(1:N,replace=TRUE)
  indtrain <- sort(indtrain)
  indtest <- setdiff(1:N,indtrain)
  
  # Fit a classifier to the training data only
  fit.r <- rpart(Class~.,data=wine,subset=indtrain)
  
  # Classify for ALL of the observations
  pred <- predict(fit.r,type="class",newdata=wine)
  
  # Look at table for the test data only (rows=truth, cols=prediction)
  tab <- table(wine$Class[indtest],pred[indtest])
  
  # Work out the accuracy
  res[iter,1]<-sum(diag(tab))/sum(tab)
  
  # Look at the results for the training data only
  tab <- table(wine$Class[indtrain],pred[indtrain])
  
  # Work out the accuracy
  res[iter,2] <- sum(diag(tab))/sum(tab)
  
} # iter


# Check out the error rate summary statistics.
colnames(res)<-c("test","train")
apply(res,2,summary)
############################
# Comparing classifiers using training, validation and test

# Set seed of random number generator
set.seed(1000)

# Load the wine data
# The gclus package is needed just for the data
library(gclus) 
data(wine)

# Ensure that Class is categorical
is.factor(wine$Class)
wine$Class <- as.factor(wine$Class)

# Load the rpart and partykit libraries
library(rpart)
library(partykit)

# Sample 50% of the data as training data
# Sample 25% of the data as validation 
# Let the remaining 25% data be test data

N <- nrow(wine)
indtrain <- sample(1:N,size=0.50*N,replace=FALSE)
indtrain <- sort(indtrain)
indvalid <- sample(setdiff(1:N,indtrain),size=0.25*N)
indvalid <- sort(indvalid)
indtest <- setdiff(1:N,union(indtrain,indvalid))

# Fit a classifier to only the training data
fit.r <- rpart(Class~.,data=wine,subset=indtrain)
plot(as.party(fit.r))

# Fit a logistic regression to the training data only too
# First load the nnet package
library(nnet)
fit.l <- multinom(Class~., data=wine,subset=indtrain)

# Classify for ALL of the observations
pred.r <- predict(fit.r,type="class",newdata=wine)
pred.l <- predict(fit.l,type="class",newdata=wine)

# Look at table for the validation data only (rows=truth, cols=prediction)
tab.r <- table(wine$Class[indvalid],pred.r[indvalid])
tab.r
tab.l <- table(wine$Class[indvalid],pred.l[indvalid])
tab.l

# Work out the accuracy
acc.r <- sum(diag(tab.r))/sum(tab.r)
acc.l <- sum(diag(tab.l))/sum(tab.l)

acc.r
acc.l

# Look at the method that did best on the validation data 
# when applied to the test data
if (acc.r>acc.l)
{
  tab <- table(wine$Class[indtest],pred.r[indtest])
}else
{
  tab <- table(wine$Class[indtest],pred.l[indtest])
}

tab

sum(diag(tab))/sum(tab)

# Let's repeat this process to see if we were just unlucky!

# Set up res to store results

res<-matrix(NA,100,4)

# Start simulation to look at this 
iterlim <- 100
for (iter in 1:iterlim)
{
  # Sample 50% of the data as training data
  # Sample 25% of the data as validation 
  # Let the remaining 25% data be test data
  
  N <- nrow(wine)
  indtrain <- sample(1:N,size=0.50*N,replace=FALSE)
  indtrain <- sort(indtrain)
  indvalid <- sample(setdiff(1:N,indtrain),size=0.25*N)
  indvalid <- sort(indvalid)
  indtest <- setdiff(1:N,union(indtrain,indvalid))
  
  # Fit a classifier to only the training data
  fit.r <- rpart(Class~.,data=wine,subset=indtrain)
  
  # Fit a logistic regression to the training data only too
  fit.l <- multinom(Class~., data=wine,subset=indtrain)
  
  # Classify for ALL of the observations
  pred.r <- predict(fit.r,type="class",newdata=wine)
  pred.l <- predict(fit.l,type="class",newdata=wine)
  
  # Look at table for the validation data only (rows=truth, cols=prediction)
  tab.r <- table(wine$Class[indvalid],pred.r[indvalid])
  tab.l <- table(wine$Class[indvalid],pred.l[indvalid])
  
  # Work out the accuracy
  acc.r <- sum(diag(tab.r))/sum(tab.r)
  acc.l <- sum(diag(tab.l))/sum(tab.l)
  
  # Store the results
  res[iter,1] <- acc.r
  res[iter,2] <- acc.l
  
  # Look at the method that did best on the validation data 
  # when applied to the test data
  if (acc.r>acc.l)
  {
    tab <- table(wine$Class[indtest],pred.r[indtest])
    acc <- sum(diag(tab))/sum(tab)
    res[iter,3] <- 1
    res[iter,4] <- acc
  }else
  {
    tab <- table(wine$Class[indtest],pred.l[indtest])
    acc <- sum(diag(tab))/sum(tab)
    res[iter,3] <- 2
    res[iter,4] <- acc
  }
  
} # iter


# Check out the error rate summary statistics.
colnames(res)<-c("valid.r","valid.l","chosen","test")
apply(res,2,summary)
table(res[,3])
###############################
# Using k-fold cross validation

# Set seed of random number generator
set.seed(1000)

# Load the wine data
# The gclus package is needed just for the data
library(gclus) 
data(wine)
N <- nrow(wine)

# Ensure that Class is categorical
is.factor(wine$Class)
wine$Class <- as.factor(wine$Class)

# Load the rpart and partykit libraries
library(rpart)
library(partykit)

# Fit a classifier to all of the training data
fit.r <- rpart(Class~.,data=wine)
plot(as.party(fit.r))

# Let's do some k-fold cross validation

# First, let's assign the observations to folds.
K <- N

folds <- rep(1:K,ceiling(N/K))
folds <- sample(folds) 
folds <- folds[1:N]

# Set up res to store results

res<-matrix(NA,K,1)

# We will need to drop each fold in turn.
iterlim <- K
for (iter in 1:iterlim)
{
  indtrain <- (1:N)[!(folds==iter)]
  indtest <- setdiff(1:N,indtrain)
  
  # Fit a classifier to only the training data
  fit.r <- rpart(Class~.,data=wine,subset=indtrain)
  
  # Classify for ALL of the observations
  pred.r <- predict(fit.r,type="class",newdata=wine)
  
  # Look at table for the validation data only (rows=truth, cols=prediction)
  tab.r <- table(wine$Class[indtest],pred.r[indtest])
  
  # Let's see how well we did on the fold that we dropped
  # res[iter,1] <- sum(diag(tab.r))/sum(tab.r)
  res[iter,1]<-sum(pred.r[indtest]==wine$Class[indtest])/length(indtest)
} # iter


# Check out the error rate summary statistics.
colnames(res)<-c("test")
apply(res,2,summary)
#######################
