# Load the data
library(caret)
data(GermanCredit)

# Inspect the data
head(GermanCredit)

# Define response variable as binary
table(GermanCredit$Class)
Good <- (GermanCredit$Class=="Good")*1

# Fit the model using glm
fit0 <- glm(Good~Duration+Amount+Age+NumberExistingCredits,data=GermanCredit
,family=binomial())
summary(fit0)

# Fit a model with the marginally insignificant terms dropped.
fit1<-glm(Good~Duration+Age,data=GermanCredit,family=binomial())
summary(fit1)

# Compare the models
anova(fit1,fit0)

# Call the chosen model fit
fit<-fit1

# Let's look at the predictions. 
# This gives P(Good=1) values

pred <- predict(fit,type="response")
pred

# We can use to predict (using a cutoff of 0.5)
predGood <- (pred >= 0.5)*1
table(Good,predGood)

# We can also look at our residual plots
res <- residuals(fit,type="pearson")
plot(pred,res)

# Let's look at goodness of fit
library(binomTools)
# Hosmer-Lemeshow Test
HLtest(Rsq(fit))
# Pearson Test
X2GOFtest(Rsq(fit))

# Predictive Performance
library(ROCR)predobj<-prediction(pred,Good)perf <- performance(predobj,"tpr","fpr")

#Plot the ROC curve.plot(perf)
abline(a=0,b=1,lty=3,col="gray")

AUC <- performance(predobj,"auc")
AUC@y.values[[1]]

# Using ROC for cut off choice
tpr <- perf@y.values[[1]]
fpr <- perf@x.values[[1]]
tau <- perf@alpha.values[[1]]

plot(tau,tpr+(1-fpr),type="l")
abline(v=tau[tpr+(1-fpr)==max(tpr+(1-fpr))],lty=3,col="gray")

threshold <- tau[tpr+(1-fpr)==max(tpr+(1-fpr))]
threshold

# We can use to predict (using a cutoff of 0.5)
predGood <- (pred >= threshold)*1
table(Good,predGood)