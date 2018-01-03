#setwd("~/DataMiningR/codeR")
votes<-load('dailvotes.Rdata')
load('dailvotes.Rdata')
votesTran <- 1*(votes=="y")
head(votesTran)
votesdf <- as.data.frame(t(votesTran))
head(votesdf)
pairs(votesdf)
k <- 2
#
fitkm<-kmeans(votesdf,centers=k)
colvec<-rainbow(k,alpha=0.2)
pairs(votesdf,col=colvec[fitkm$cluster],pch=19)
summary(fitkm)
fitpam<-pam(votesdf,k=2)
summary(fitpam)
pairs(votesdf,col=colvec[fitpam$clustering],pch=19)
 ## and for Windows' 'Unicode'
 #str(xx <- iconv(votes[,0], "latin1", "UTF-16LE", toRaw = TRUE))
 #iconv(votes[,0], "UTF-8","WINDOWS-1252")
 #votesR<-read.table("assign2.Rdata")
 #votesR <- load("assign2.Rdata")
d <- dist(votesdf, method="euclidean")^2
sil <- silhouette(fitkm$cluster,d)
plot(sil)


silvals<-sil[,3]
#Choose the plot symbol for the positive/negative silhouette observations
pchvec<-rep(19,nrow(votesdf))
pchvec[silvals<0]<-3
# Replace negative silhouettes with zero (just for plot coloring)
silvals[silvals<0]<-0
#Construct the pairs plot
pairs(votesdf,col=rgb(silvals,0,1-silvals,alpha=0.2),pch=pchvec)
#Run both algorithms
#K-means
K<-2
fitkm<-kmeans(votesdf,centers=K,nstart=10)
# K-medoids
d<-dist(votesdf,method="manhattan")
fitkmed<-pam(d,k=K)
# Tabulate the results
tab<-table(fitkm$cluster,fitkmed$clustering)
tab
library(e1071)
classAgreement(tab)
matchClasses(tab, "exact")

