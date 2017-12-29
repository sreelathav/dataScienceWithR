             #Author: Sree Latha Vallabhaneni
                        

#Load pottery data 
#getwd()
#setwd("~/Documents/multiVarAnalysis")
pottery <- read.csv("~/Data/PotteryData.csv")
head(pottery)
names(pottery)
chem9 <- pottery[,1:9]
chem9
        #Q 1 a  (iv) checking for standardisation
colMeans(chem9) #large difference in the means
round(apply(chem9,2,sd),3) #large difference in standard deviations
#advisable to standardise data
#sdev <- apply(chem9,2,sd)
#stdPottery <- sweep(chem9,2,sdev,"/")
#using scale() function to standardize

#QUESTION 1 
             # (a) (i) covariance matrix of the 9 chemicals
#?cov()
cov_mat_pottery <- cov(scale(chem9))
round(cov_mat_pottery,2)
#checking for singularity of covarince matrix
library(matrixcalc)
is.singular.matrix(cov_mat_pottery)
# False, so  not singular
#
           #1a (ii) Eigen values and eigen vectors of covariance matrix
Eig <- eigen(cov_mat_pottery)
Eig
# checking if the eigen values and vectors are indeed eigen values and vectors of cov_mat_pottery
#for this cov_mat_pottery*eigenVectors = eigenValues*eigenVectors

#checking all vectors, values whether they are eigen
for (i in 1:7)
{
   U = round(Eig$values[i] * Eig$vectors[,i],3) 
   V = round(t( Eig$vectors[,i])%*%cov_mat_pottery,3)
   if (all(U==V) )    
      print ( c(i,"   vector and value are eigen vector and value"))
}
#[1] "1"                                             
#[2] "   vector and value are eigen vector and value"
#[1] "2"
#[2] "   vector and value are eigen vector and value"
#they are equal, so all are  eigen values and eigen vectors.

round(Eig$values[1:2],4) #first 2 eigen values
#[1] 4.203908 2.523285

round(Eig$vectors[,1:2],4) #first 2 eigen vectors
#          [,1]        [,2]
# [1,]  0.34829631  0.32780562
# [2,] -0.32709620  0.39525949
# [3,] -0.43456923 -0.18964741
# [4,] -0.06428531  0.50119655
# [5,] -0.21717586  0.45551237
# [6,] -0.45633257 -0.01837517
# [7,]  0.34019754  0.30078403
# [8,] -0.45522720  0.08753781
# [9,] -0.01854192  0.37839957

#assign first 2 eigen vectors
firstVector = Eig$vectors[,1]
firstVector
secondVector = Eig$vectors[,2]
secondVector
                           #Q1 (a) iii
#2 vectors are orthogonal if  transpose(A)*B = 0 = transpose(B)*A
# testing if first 2 eigen vectors are orthoganal
round(t(firstVector)%*%secondVector,3)
#[1,] 0
round(t(secondVector)%*%firstVector,3)
#[1,] 0
# so first 2 eigen vectors are orthogonal

# vectors A and B are othonormal if they are orthoganal and transpose(A)*A = I= transpose(B)*B
round(t(firstVector)%*%firstVector,3)
#[1,] 1
round(t(secondVector)%*%secondVector,3)
#[1,] 1
#so the first 2 vectors are orthonormal

#1(b)
#Let E[X1]= ex1 E[X2] = ex2, Var[X1]=varx1, var[X2]= varx2, cov[X1,X2]= covx1x2
#Given
ex1 <- 10
ex2 <- 5
varx1 <- 11
varx2 <- 4
covx1x2 <- 2


                                #Q1 b (i) 
#Let U = 2X1 -X2, V = X1 + 2X2
#Let expU = E(2X1 - X2), Let varU = Var(2X1 -X2)
expU <- 2*ex1 - ex2
expU 
#E(2X1-X2) = 15
varU <- varx1*(2^2 ) + varx2*(-1)^2 + 2*2*(-1)*covx1x2
varU
#var(2X1-X2)= 40

                        #1b 
#Let expV = E(X1+2X2), varV = var(X1+2X2)
expV = ex1+2*ex2
expV    #20
varV = varx1 + varx2*(2^2) + 2*2*1*covx1x2
varV
#var(X1+2X2) = 35

#covUV = cov(U,V) = cov(2X1-X2, X1+2X2)
#     = cov(2X1,X1)+ cov(2X1,2X2)+cov(-X2,X1)+cov(-X2,2X2)
covUV     = 2*varx1 + 4*covx1x2 - 1*covx1x2 -2*varx2
covUV  #20

# corUV = cor(U,V)= covUV/sqrt(varU*varV), 
corUV = covUV/sqrt(varU*varV)
corUV  #0.5345

                                #Question 2

library(mclust)
library(e1071)
library (cluster)
library (vegan)
#library(help=e1071)
                  
#read and explore data
pottery <- read.csv("~/Data/PotteryData.csv")
head(pottery) 
#kiln is a categorical. separate kiln variable and scale rest of the data
chem9 <- scale(pottery[,1:9])
chem9
table(pottery$kiln) #5 classes
#pairs plot with division on varable "klin"
pairs(chem9, main="Diabetes pairs plot", pch=22, 
      bg=c("red", "blue","green","yellow","purple")[unclass(pottery$kiln)])
#crowding in the clusters with few observations in a couple of clusters

           #Looking at k-means way of clustering Q2 (a)
#set seed
set.seed(654)
#prepare to get data for Sil plot to decide on number of clusters
WGSS <- rep(0,10)
n <- nrow(chem9)
#within in group sum of sqares for single cluster type
WGSS[1] <- (n-1) * sum(apply(chem9, 2, var))
#for 2 clusters and more
for(k in 2:10)
{
  WGSS[k] <- sum(kmeans(chem9, centers = k)$withinss)
}

plot(1:10, WGSS, type="b", xlab="k", ylab="Within group sum of squares")
#clear elbow at k=3, but seems we can also able to see elbow at 4 or 5 
#need further investigation 
#try k means with k = 3,4,5
K <- 5
kcl <- kmeans(chem9, center=K)
table(kcl$cluster)
#sil plot
dis = vegdist(chem9, method="manhattan")
sil = silhouette (kcl$cluster,dis) #  use  cluster vector
plot(sil)
summary(sil)
#cluster plot
plot(chem9, col= kcl$cluster,main="Cluster plot for k=5", xlab="",ylab="")
points(kcl$centers, col=1:K, pch=8)
 
#compare with original classes
tab <- table(actual = pottery$kiln, k_means = kcl$cluster)
tab
adjustedRandIndex(pottery[,10],kcl$cluster)
#

#plot for k = 3, 4, 5
#heirarchical clustering
hcl <- hclust(dist(chem9, method="manhattan"),  method="average")
plot(hcl)
#cut dendogram at 3 and 5
hcl <- cutree(hclust(dist(chem9)),3)
plot(hcl, main = "heirarchical cluster plot cutTree=3")
tab <- table(actual=pottery[,10],hierarchical=hcl)
tab
adjustedRandIndex(pottery[,10],hcl)
sil = silhouette (hcl,dis) #  use  cluster vector
plot(sil)
summary(sil)

#compare k-means and heirarchical
kcl <- kmeans(chem9, centers=3)
hcl <- cutree(hclust(dist(chem9, method="manhattan"),method="average"),3)
tab <- table(k_means= kcl$cluster, heirarchical=hcl)
tab
adjustedRandIndex(hcl,kcl$cluster)

##class agreement
classAgreement(tab)
classAgreement(table(pottery[,10],kcl$cluster))
classAgreement(table(pottery[,10],hcl))


#rand index from mclust
mcl <- Mclust(chem9)
attributes(mcl)
plot(mcl,what = "classification")
plot(mcl$uncertainty, type = "h")

#comaring with the original data
table(pottery[,10],mcl$classification)
adjustedRandIndex(pottery[,10],mcl$classification)
#
adjustedRandIndex(pottery[,10],hcl)
adjustedRandIndex(pottery[,10],kcl$cluster)
#comparing heirarchical and kmeans
adjustedRandIndex(hcl,kcl$cluster)

                           #Question3
#load Pima data 
pima <- read.csv("Pima.csv")
head(pima)
is.factor(pima[,8])
pairs(pima[,1:7], main="Pima pairs plot", pch=22, 
      bg=c("red", "blue")[unclass(pima$type)])
#From pairs plot it looks quadratic discrimant analysis works better

#qda implementation
library(MASS)
?qda()
qdaOut <- qda(type ~ ., data=pima)
attributes(qdaOut)
#qdaOut$prior
#qdaOut$means
#qdaOut$xlevels
#
#Quadratic discriminant function
#δk(x) =  logπk −(1/2)log|Σk|−(1/2)transpose(x−μk)inverse(Σk)(x−μk)
#function to calculate qdf
qdf <- function(x, prior, mu, covar)
{
  
  x <- matrix(as.numeric(x), ncol=1)
  log(prior) - 0.5*log(det(covar)) -(0.5*t(x-mu)%*%solve(covar)%*%(x-mu)) 
}
#requirements for qdf 
#
N <- nrow(pima)
N
G <- length(levels(pima$type))
G
#subset on class
diabetes.yes <- subset(pima, type == "Yes")
diabetes.no <- subset(pima, type == "No")

#cov  in each class
cov_yes <- cov(diabetes.yes[,1:7])
cov_no <- cov(diabetes.no[,1:7])
#over all covariance
cov_all<-((cov_yes*(nrow(diabetes.yes)-1)) + (cov_no*(nrow(diabetes.no)-1)))/(N - G)
#cov_all
#solve(cov_all)
#calculate qdf for mth patient(new observation here)

new_row <- data.frame(npreg=7,glu=187,bp=50,skin=33,bmi=33.9,ped=0.826,age=30,type="No")
new_row #new observation
#  npreg glu bp skin  bmi   ped age type
#1     7 187 50   33 33.9 0.826  30   No

pima1 <- rbind(pima,new_row) #adding new obs to data
tail(pima1)
id <- 528
dfs <- rep(0, G)
for(g in 1:G)
{
  dfs[g] <- qdf(pima1[id,1:7], qdaOut$prior[g], qdaOut$means[g,], cov_all)
}
dfs
#[1] -20.94997 -18.27589
levels(pima$type)[dfs == max(dfs)]
#[1] "Yes"
#predicting for the new observation using QDA 3(a)
predict(qdaOut,new_row)$class
#[1] "Yes"
#

#Question 3(b)
# creating test data with first test observation
testPima<- data.frame(npreg=2,glu=88,bp=58,skin=26,bmi=28.4,ped=0.766,age=22,type="No")

#function to add new observations to pima
addObsPima <- function(data,npreg1,glu1,bp1,skin1,bmi1,ped1,age1,type1){
  new_row <- data.frame(
    npreg=npreg1,glu=glu1,bp=bp1,skin=skin1,bmi=bmi1,ped=ped1,age=age1,type=type1)  
  rbind(data,new_row)
}

#adding rest of test observations to test data
testPima <- addObsPima(testPima,9,170,74,31,44.0,0.403,43,"Yes")
testPima <- addObsPima(testPima,10,101,76,48,32.9,0.171,63,"No")
testPima <- addObsPima(testPima,5,121,72,23,26.2,0.245,30,"No")
testPima <- addObsPima(testPima,1,93,70,31,30.4,0.315,23,"No")
nrow(testPima) #5
testPima
#  npreg glu bp skin  bmi   ped age type
#1     2  88 58   26 28.4 0.766  22   No
#2     9 170 74   31 44.0 0.403  43  Yes
#3    10 101 76   48 32.9 0.171  63   No
#4     5 121 72   23 26.2 0.245  30   No
#5     1  93 70   31 30.4 0.315  23   No

#predict for testdata 
predictedClass <- predict(qdaOut,testPima)$class
predictedClass
#[1] No  Yes Yes No  No 
tab <- table(actual= testPima[,8], predicted=predictedClass)
tab

#missclassification rate on test set
mcQDA <- 1-sum(diag(tab))/sum(tab)
mcQDA #0.2


