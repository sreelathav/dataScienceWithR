#             
#Author: Sree Latha Vallabhaneni                            date 2-05-2017

#Load prices data into R
prices<- read.csv('~/data/Prices.csv')
head(prices)
names(prices) #to be subsetted on the Area variable 
table(prices$Area) #13 observations in each area

#subsetting on the Area
prices.MP = subset(prices, Area=="MP")
prices.PA = subset(prices, Area=="PA")
prices.MP
prices.PA

#Function to check whether the 2 communities have properties with significantly different characters
diffProperty<- function(x,subset1,subset2){
  #number of observations in each subset
  n1=nrow(subset1)
  n2=nrow(subset2)
  #p dimensional multivariate normal distribution
  p=ncol(x)
  #covariance matrix of each subset
  s1 <- cov(subset1)
  s2 <- cov(subset2)
  #The estimate of the common covarince matrix
  comCovMat = ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)
  comCovMat
  # mahalanobis distance between 2 groups with common cov matrix comCovMat
  D <- mahalanobis(x, colMeans(x), comCovMat)
  D2= D%*%D 
  # Hotellings, multivariate t-test
  T2 <- n1*n2*D2/(n1+n2)
  T2
  #F distribution
  Fdist = (n1+n2-p-1)*T2/(n1+n2-2)*p
  Fdist
  #standard F to compare
  refF = qf(.95,df1=p, df2=n1+n2-p-1)
  #The null hypothesis that the means of the variables in the first population is equal
  #the means of the variables in the second population can be rejeted if Fdist greater than
  #standard F
  ifelse( Fdist > refF , "sigificantly different", "not signifcantly different")
  
}

diffProperty(prices[,2:4],prices.MP[,2:4],prices.PA[,2:4])
#"sigificantly different"

##############                           ###########
#Load votes data into R
load('/data/2016_First6Votes_PresentAbsent.RData')
head(votes)
names(votes)

#heirarchical clustering
set.seed(456)
cl.average = hclust(dist(votes,method="manhattan"), method="average")
plot(cl.average)
cl.ward = hclust(dist(votes,method='canberra'), method="ward.D2")
plot(cl.ward)
hcl = cutree(cl.ward, k = 4)
#plot(hcl)
table(hcl)
#optimal clust
hsil <- silhouette(hcl,dist(votes))
plot(hsil)
# elbow.k <- function(mydata){
#   dist.obj <- dist(mydata)
#   hclust.obj <- hclust(dist.obj)
#   css.obj <- css.hclust(dist.obj,hclust.obj)
#   elbow.obj <- elbow.batch(css.obj)
#   k <- elbow.obj$k
#   return(k)
# }
# elbow.k(votes)

#
library(klaR)
K <- 4
kcl <- kmodes(votes, modes=K,iter.max=50000)
table(kcl$cluster)
attributes(kcl)
kcl$withindiff
plot(silhouette(kcl$cluster,dist(votes)))

K <- 5
kcl <- kmodes(votes, modes=K,iter.max=50000)
table(kcl$cluster)
kcl$withindiff
plot(silhouette(kcl$cluster,dist(votes)))

K <- 6
kcl <- kmodes(votes, modes=K,iter.max=50000)
table(kcl$cluster)
kcl$withindiff
plot(silhouette(kcl$cluster,dist(votes)))
## and visualize
#plot(votes, col= kcl$cluster)
#points(kcl$centers, col=1:K, pch=8)

#mclust
library(mclust)
mcl <- Mclust(votes,g=4:8)
mcl$BIC
plot(mcl, what="BIC", legendArgs = list(x="topleft", cex=0.5,
                                        horiz=T), ylim=c(-7400, -4000))
plot(mcl, what="classification")
plot(mcl, what="uncertainty")
plot(mcl$uncertainty, type="h")

#compare the different cluster analysis
adjustedRandIndex(kcl$cluster, mcl$classification)
adjustedRandIndex(hcl, mcl$classification)
adjustedRandIndex(kcl$cluster, hcl)

#poLCA

library(poLCA)
form <- cbind(ED1, ED2, Credit, Confidence1, Confidence2, Trade)~1
polca2 <- poLCA(form,votes,nclass=2,maxiter=60000)
polca3 <- poLCA(form,votes,nclass=3,maxiter=60000)
polca4 <- poLCA(form,votes,nclass=4,maxiter=60000,graph=TRUE)
polca5 <- poLCA(form,votes,nclass=5,maxiter=60000,graph=TRUE)
polca6 <- poLCA(form,votes,nclass=6,maxiter=60000)
polca7 <- poLCA(form,votes,nclass=7,maxiter=60000)
polca8 <- poLCA(form,votes,nclass=8,maxiter=60000)

attributes(polca4)
summary(polca4)
table(polca4$predclass)
polca4$P

# polca polca_res dataframe with fit-values for different models
polca_res <- data.frame(nclust=2,
                      log_likelihood=polca2$llik,
                      resid.df = polca2$resid.df,
                      BIC=polca2$bic,
                      AIC=polca2$aic,
                      likelihood_ratio=polca2$Gsq,
                      Goodness_of_fit=polca2$Chisq)
                      #Entropy = R2_entropy(polca2$P,polca2$posterior)

polca_res[2,1]<-3
polca_res[3,1]<-4
polca_res[4,1]<-5
polca_res[5,1]<-6
polca_res[6,1]<-7
polca_res[7,1]<-8
polca_res$nclust = as.integer(polca_res$nclust)

polca_res[2,2]<-polca3$llik
polca_res[3,2]<-polca4$llik
polca_res[4,2]<-polca5$llik
polca_res[5,2]<-polca6$llik
polca_res[6,2]<-polca7$llik
polca_res[7,2]<-polca8$llik

polca_res[2,3]<-polca3$resid.df
polca_res[3,3]<-polca4$resid.df
polca_res[4,3]<-polca5$resid.df
polca_res[5,3]<-polca6$resid.df
polca_res[6,3]<-polca7$resid.df
polca_res[7,3]<-polca8$resid.df

polca_res[2,4]<-polca3$bic
polca_res[3,4]<-polca4$bic
polca_res[4,4]<-polca5$bic
polca_res[5,4]<-polca6$bic
polca_res[6,4]<-polca7$bic
polca_res[7,4]<-polca8$bic

polca_res[2,5]<-polca3$aic
polca_res[3,5]<-polca4$aic
polca_res[4,5]<-polca5$aic
polca_res[5,5]<-polca6$aic
polca_res[6,5]<-polca7$aic
polca_res[7,5]<-polca8$aic

polca_res[2,6]<-polca3$Gsq
polca_res[3,6]<-polca4$Gsq
polca_res[4,6]<-polca5$Gsq
polca_res[5,6]<-polca6$Gsq
polca_res[6,6]<-polca7$Gsq
polca_res[7,6]<-polca8$Gsq
#
polca_res[2,7]<-polca3$Chisq
polca_res[3,7]<-polca4$Chisq
polca_res[4,7]<-polca5$Chisq
polca_res[5,7]<-polca6$Chisq
polca_res[6,7]<-polca7$Chisq
polca_res[7,7]<-polca8$Chisq
#
entropy<-function (p) sum(-p*log(p))
R2_entropy <- function(P, posterior){
  error_prior<-entropy(P) # nclust model 
  error_post<-mean(apply(posterior,1, entropy),na.rm = TRUE)
  round(((error_prior-error_post) / error_prior),3)
}

# polca_res[2,8]<-R2_entropy(polca3$P,polca3$posterior)
# polca_res[3,8]<-R2_entropy(polca4$P,polca4$posterior)
# polca_res[4,8]<-R2_entropy(polca5$P,polca5$posterior)
# polca_res[5,8]<-R2_entropy(polca6$P,polca6$posterior)
# polca_res[6,8]<-R2_entropy(polca7$P,polca7$posterior)
# polca_res[7,8]<-R2_entropy(polca8$P,polca8$posterior)
# #
par(mfrow=c(1,3))
plot(polca_res$nclust,polca_res$BIC, xlab= "No. of Classes", ylab= "BIC") #MIN AT NCLUST=4
plot(polca_res$nclust,polca_res$AIC, xlab = "No. of Classes", ylab= "AIC") #MIN AT NCLUST=4
plot(polca_res$nclust,polca_res$likelihood_ratio, xlab= "No. of Classes", ylab= "Likelyhood Ratio")
par(mfrow=c(1,1))
#install.packages("ztable")
ztable::ztable(polca_res)
##comare the different cluster analysis
adjustedRandIndex(kcl$cluster, hcl)
adjustedRandIndex(polca4$predclass, hcl)
adjustedRandIndex(kcl$cluster, polca4$predclass)

#Q2(d)
mebers.party<- load("~/data/PartyMembership.Rdata")
names(members.party)
head(members.party)
table(members.party$Party)

###merge votes and party mmbership
rn <- rownames(votes)
Party<-members.party[match(rn, members.party$TD), 2]
votes_party <- cbind(votes, Party)

#merge votes, party and polca class
votes_party_class<- cbind(votes, Party,Class=polca5$predclass)

head(votes_party_class)
table(votes_party_class$Party,votes_party_class$Class)
#
vpc4<- cbind(votes, Party,Class=polca4$predclass)
table(vpc4$Party,vpc4$Class)
#specific votes analysis
table(vpc4$ED1,vpc4$Party)
table(vpc4$ED2,vpc4$Party)
table(vpc4$Credit,vpc4$Party)
table(vpc4$Confidence1,vpc4$Party)
table(vpc4$Confidence2,vpc4$Party)
table(vpc4$Trade,vpc4$Party)
polca4$predcell
