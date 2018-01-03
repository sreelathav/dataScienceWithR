# Case study: Forensic Glass Data

# Set random number seed
set.seed(1)

# Load the forensic glass data
library(MASS)
data(fgl)
?fgl 

# Load many required packages
library(rpart)
library(adabag)
library(randomForest)
library(partykit)

# Split data into training and test
N<-nrow(fgl)
indtrain<-sample(1:N,size=0.75*N)
indtrain<-sort(indtrain)
indtest<-setdiff(1:N,indtrain)

# Look at performance of classification trees
fit.r <- rpart(type~.,data=fgl,subset=indtrain)
plot(as.party(fit.r))
pred.r <- predict(fit.r,newdata=fgl,type="class")

# Test data
table(fgl$type[indtest],pred.r[indtest])
sum(fgl$type[indtest]==pred.r[indtest])/length(indtest)

# Training data
table(fgl$type[indtrain],pred.r[indtrain])
sum(fgl$type[indtrain]==pred.r[indtrain])/length(indtrain)

# Look at bagging classification trees
fit.b <- bagging(type~.,data=fgl[indtrain,])
pred.b <- predict(fit.b,newdata=fgl,type="class")$class

# Test data
table(fgl$type[indtest],pred.b[indtest])
sum(fgl$type[indtest]==pred.b[indtest])/length(indtest)

# Training data

table(fgl$type[indtrain],pred.b[indtrain])
sum(fgl$type[indtrain]==pred.b[indtrain])/length(indtrain)

# Look at random forest classifier
fit.rf <- randomForest(type~.,data=fgl,subset=indtrain)
pred.rf <- predict(fit.rf,newdata=fgl,type="class")

# Test data
table(fgl$type[indtest],pred.rf[indtest])
sum(fgl$type[indtest]==pred.rf[indtest])/length(indtest)

# Training data
table(fgl$type[indtrain],pred.rf[indtrain])
sum(fgl$type[indtrain]==pred.rf[indtrain])/length(indtrain)
##################report
# Classification Accuracy for Forensic Glass Data
# The forensic glass data contains chemical and physical measurements for 214
# glass samples. The measurements made are refractive index (RI), sodium (Na),
# manganese (Mg), Aluminium (Al), Silicon (Si), Potassium (K), Calcium (Ca),
# Barium (Ba) and Iron (Fe). The glass samples were of the different types, as
# shown in Table 1.
# An initial exploratory analysis (Table 1) shows that the Affective symptom
# is most frequent (75%) and Hallucination is least frequent (8%).
# Table 1: Types of glass in the data
# Type
# Window Float (WinF)
# Window Non Float (WinNF)
# Vehicle (Veh)
# Container (Con)
# Tableware (Tabl)
# Headlight (Head)
# Count
# 70
# 76
# 17
# 13
# 9
# 29
# We wish to assess the possibility of predicting the correct glass type on
# the basis of the chemical and physical measurements. For this purpose, we
# propose investigating the use of four different classification methods
# including: classification trees, multinomial logistic regression, bagging
# and random forests. The default implementation of these methods was used,
# where the following R packages (Table 2) were used.
# Table 2: Classification methods and R package used.
# Type
# Classification Trees
# Multinomial Logistic Regression
# Bagging
# Random Forests
# Package
# rpart
# nnet
# adabag
# randomForest
# In order to get an accurate estimate of the classification performance on
# these data the following approach was used:
#   1) Fit all four classifiers to a random training dataset consisting of
# 70% of the observations.
# 2) Assess the fit of all four classifiers on a random validation dataset
# consisting of 15% of the observations (none of which were in the
#                                        training dataset). Select the method with the highest classification
# accuracy on the validation data
# 3) Assess the fit of the most accurate classifier (as determined in Step
#                                                    2) on a random test dataset consisting of the remaining 15% of the
# observations.
# The classifier chosen using this approach was a random forest classifier.
# The classifier achieved a 72.7% accuracy on the test dataset. Table 3 shows
# the confusion matrix of the classification on the test dataset.
# Table 3: Confusion matrix for the classifier chosen.
# WinF
# WinNF
# Veh
# Con
# Tabl
# Head
# WinF WinNF Veh Con Tabl Head
# 13
# 1
# 1
# 0
# 0
# 0
# 1
# 7
# 1
# 0
# 1
# 0
# 1
# 0
# 0
# 0
# 0
# 0
# 0
# 2
# 0
# 2
# 0
# 0
# 1
# 0
# 0
# 0
# 1
# 0
# 0
# 0
# 0
# 0
# 0
# 1
# The classifier is very accurate at predicting the correct type for the
# Window Float and Window Non-Float glass types; these were the most prevalent
# types in the dataset.
# It is worth noting that the classification accuracy of the random forest
# classifier on the training and validation datasets were 100% and 81.25%respectively. These measurements are overly optimistic because the method of
# fitting and selecting the classifier utilizes the labeling of the data set
# being assessed. Thus, the performance assessment is biased upwards. In
# contrast, the assessment of classification performance on the test dataset
# is an independent assessment of performance and is thus unbiased.# Code used
# # Set seed
# set.seed(1)
# Let's use the forensic glass data
library(MASS)
data(fgl)
fgl
# Divide the data into training, validation, test
N<-nrow(fgl)
indtrain<-sort(sample(1:N,size=N*0.7))
indtest<-setdiff(1:N,indtrain)
indvalid<-sort(sample(indtest,size=N*0.15))
indtest<-sort(setdiff(indtest,indvalid))
# Classification trees
library(rpart)
fit.rpart<-rpart(type~.,data=fgl,subset=indtrain)
pred.rpart<-predict(fit.rpart,newdata=fgl,type="class")
table(fgl$type[indvalid],pred.rpart[indvalid])
acc.rpart<-sum(fgl$type[indvalid]==pred.rpart[indvalid])
# Multinomial logistic
library(nnet)
fit.mult<-multinom(type~.,data=fgl,subset=indtrain)
pred.mult<-predict(fit.mult,newdata=fgl,type="class")
table(fgl$type[indvalid],pred.mult[indvalid])
acc.mult<-sum(fgl$type[indvalid]==pred.mult[indvalid])
# Bagging
library(adabag)
fit.adabag<-bagging(type~.,data=fgl[indtrain,])
pred.adabag<-
  predict(fit.adabag,newdata=fgl,type="class")$class
table(fgl$type[indvalid],pred.adabag[indvalid])
acc.adabag<-sum(fgl$type[indvalid]==pred.adabag[indvalid])
# Random forest
library(randomForest)
fit.rf<-randomForest(type~.,data=fgl,subset=indtrain)
pred.rf<-predict(fit.rf,newdata=fgl,type="class")
table(fgl$type[indvalid],pred.rf[indvalid])
acc.rf<-sum(fgl$type[indvalid]==pred.rf[indvalid])
# Select best methods
acc<-c(acc.rpart,acc.mult,acc.adabag,acc.rf)
acc/length(indvalid)
# Assess performance on test data
table(fgl$type[indtest],pred.rf[indtest])
sum(fgl$type[indtest]==pred.rf[indtest])/length(indtest)
##################
#############
# Load necessary libraries
library(adabag)
library(MASS)

# Load data
data(fgl)

# Split data into training and test
N <- nrow(fgl)
indtrain <- sample(1:N,size=N*0.75)
indtrain <- sort(indtrain)
indtest <- setdiff(1:N,indtrain)

# Fit boosting
# When doing this, look at the effect of options:
# coeflearn, boos, etc.
fit <- boosting(type~.,data=fgl[indtrain,],boos=FALSE,coeflearn="Freund")

# Predict
pred<- predict(fit,newdata=fgl)

# Look at performance (train)
table(fgl$type[indtrain],pred$class[indtrain])
sum(fgl$type[indtrain]==pred$class[indtrain])/length(indtrain)

# Look at performance (test)
table(fgl$type[indtest],pred$class[indtest])
sum(fgl$type[indtest]==pred$class[indtest])/length(indtest)
#################