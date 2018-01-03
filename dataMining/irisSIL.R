data(iris)
X <- iris[,-5]
pairs(X)
k <- 3

fitkm<-kmeans(X,centers=K)
colvec<-rainbow(K,alpha=0.2)
pairs(X,col=colvec[fitkm$cluster],pch=19)
library(cluster)
fitpam<-pam(x,k=3)
pairs(X,col=colvec[fitkm$cluster],pch=19)
d <- dist(X, method="euclidean")^2
sil <- silhouette(fitkm$cluster,d)
plot(sil)
silvals<-sil[,3]
pchvec<-rep(19,nrow(X))
pchvec[silvals<0]<-3
silvals[silvals<0]<-0
pairs(X,col=rgb(silvals,0,1-silvals,alpha=0.2),pch=pchvec)

#Run both algorithms
#K-means
K<-3
fitkm<-kmeans(X,centers=K,nstart=10)
# K-medoids
d<-dist(X,method="manhattan")
fitkmed<-pam(d,k=K)
# Tabulate the results
tab<-table(fitkm$cluster,fitkmed$clustering)
tab
silvals<-sil[,3]
#Choose the plot symbol for the positive/negative silhouette observations
pchvec<-rep(19,nrow(X))
pchvec[silvals<0]<-3
# Replace negative silhouettes with zero (just for plot coloring)
silvals[silvals<0]<-0
#Construct the pairs plot
pairs(X,col=rgb(silvals,0,1-silvals,alpha=0.2),pch=pchvec)

#Compute the Rand and adjusted Rand indices
library(e1071)
classAgreement(tab)

