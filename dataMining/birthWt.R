library(MASS)
data(birthwt)

birthwt$race<-as.factor(birthwt$race)
birthwt$smoke<-as.factor(birthwt$smoke)
birthwt$ht<-as.factor(birthwt$ht)
birthwt$ui<-as.factor(birthwt$ui)

fit<-glm(low~age+lwt+race+smoke+ptl+ht+ui+ftv,data=birthwt,family="binomial")

summary(fit)

# Extract the coefficients of the model
beta<-coef(fit)

# Compute the odds
exp(beta)

# Compute a 95% confidence interval for beta & odds
# Extract the coefficients & standard errors.
summ<-summary(fit)

# Compute confidence limits for beta
betaLB<-summ$coef[,1]-qt(0.975,summ$df.residual)*summ$coef[,2]
betaUB<-summ$coef[,1]+qt(0.975,summ$df.residual)*summ$coef[,2]

# Store coefficients & confidence limits in matrix
BETA<-cbind(betaLB,beta,betaUB)

# Compute odds & confidence limits for odds
exp(BETA)

# Using the rms library
library(rms)

fit1<-lrm(low~age+lwt+race+smoke+ptl+ht+ui+ftv,data=birthwt,x=TRUE,y=TRUE)

residuals(fit1,type="pearson")

# Performance
library(xtable)
pred<-predict(fit,type="response")
tau<-1.0
l<-(pred>=tau)*1
l<-factor(l,levels=c(0,1))
tab<-table(birthwt$low,l)

xtable(tab)

tauv<-seq(0,1,by=0.001)

res<-matrix(NA,length(tauv),8)
for (j in 1:length(tauv))
{
tau<-tauv[j]	
l<-(pred>=tau)*1
l<-factor(l,levels=c(0,1))
tab<-table(birthwt$low,l)
Sens<-tab[2,2]/(tab[2,2]+tab[2,1])
Spec<-tab[1,1]/(tab[1,1]+tab[1,2])
PPV<-tab[2,2]/(tab[2,2]+tab[1,2])
NPV<-tab[1,1]/(tab[1,1]+tab[2,1])
Acc<-(tab[2,2]+tab[1,1])/sum(tab)
FDR<-1-PPV
FPR<-1-Spec

res[j,]<-c(tau,Sens,Spec,PPV,NPV,Acc,FDR,FPR)
}
res<-data.frame(res)
colnames(res)<-c("tau","Sensitivity","Specificity","PPV","NPV","Accuracy","FDR","FPR")

xtable(res)

plot(res$FPR,res$Sens,xlab="False Positive Rate (FPR)",ylab="True Positive Rate (TPR)",type="s")
abline(a=0,b=1,lty=3)
points(c(0,0,1),c(0,1,1),lty=2,col="red",type="l")

library(ROCR)
predobj<-prediction(pred,birthwt$low)
perf <- performance(predobj,"tpr","fpr")
plot(perf)

plot(tauv,res$Sens+res$Spec,xlab=expression(tau),ylab="Sensitivity+Specificity",pch=3)

j<-which.max(res$Sens+res$Spec)
tauv[j]

plot(res$FPR,res$Sens,xlab="False Positive Rate (FPR)",ylab="True Positive Rate (TPR)",type="s")
abline(a=0,b=1,lty=3)
points(res$FPR[j],res$Sens[j],pch=3,col="red",cex=3)