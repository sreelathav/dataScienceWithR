data("faithful")
WGSS <- rep(0,10)
n <- nrow(faithful)
WGSS[1] <- (n-1) * sum(apply(faithful, 2, var))
for(k in 2:10)
   {
    
      WGSS[k] <- sum(kmeans(faithful, centers = k)$withinss)
       }
plot(1:10, WGSS, type="b", xlab="k", ylab="Within group sum of squares")
K <- 2
cl <- kmeans(faithful, center=K)
table(cl$cluster)
plot(faithful, col= cl$cluster)
points(cl$centers, col=1:K, pch=8)
#cut dendogram
hcl <- cutree(hclust(dist(faithful)), 2)
pcl <- kmeans(faithful, centers=2)
tab <- table(hcl, pcl$cluster)
tab

#class agreement
library(e1071)
library(help=e1071)
classAgreement(tab)
#oliveoil knn
#read olive oil data
oliveoil<- read.csv('~/data/OliveOilData.csv')
head(oliveoil)
names(oliveoil)
#
acids = oliveoil[,3:10]
names(acids)
#determine number of clusters
WGSS <- rep(0,10)
n <- nrow(oliveoil)
WGSS[1] <- (n-1) * sum(apply(acids, 2, var))
for(k in 2:10)
{
  
  WGSS[k] <- sum(kmeans(acids, centers = k)$withinss)
}
plot(1:10, WGSS, type="b", xlab="k", ylab="Within group sum of squares")
K <- 3
cl <- kmeans(acids, center=K)
table(cl$cluster)
plot(acids, col= cl$cluster)
points(cl$centers, col=1:K, pch=8)
#
hcl <- cutree(hclust(dist(acids)), 3)
pcl <- kmeans(acids, centers=3)
tab <- table(hcl, pcl$cluster)
tab
classAgreement(tab)
#microgene
microGene <- read.csv('~/data/microarraydata.csv')
head(microGene,2)
dim(microGene)
microGene[2,1:100]
WGSS <- rep(0,10)
n <- nrow(microGene )
WGSS[1] <- (n-1) * sum(apply(microGene , 2, var))
for(k in 2:10)
{
  
  WGSS[k] <- sum(kmeans(microGene , centers = k)$withinss)
}
plot(1:10, WGSS, type="b", xlab="k", ylab="Within group sum of squares")
K <- 3
cl <- kmeans(microGene , center=K)
table(cl$cluster)
plot(microGene , col= cl$cluster)
points(cl$centers, col=1:K, pch=8)
#
hcl <- cutree(hclust(dist(microGene )), 3)
pcl <- kmeans(microGene , centers=3)
tab <- table(hcl, pcl$cluster)
tab
classAgreement(tab)
