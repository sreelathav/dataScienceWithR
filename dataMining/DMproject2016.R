######
######Project:   Mechanisms-based Classification of Musculoskeletal Pain
#######
library(cluster)
library(arules)
library(arulesViz)
library(nnet)
library(rpart)
library(e1071)
library(partykit)
library(adabag) 
library(randomForest)
library(kernlab)
library(caret)
library(mclust)
library(ipred)


# Reading data
projectdat<-read.table(url("http://mathsci.ucd.ie/~brendan/data/Physio.txt"))
head(projectdat)
str(projectdat)

# converting to matrix and eliminating categorical variables
projectdata <- as.matrix(projectdat[,-37])
str(projectdata)
head(projectdata)

#for (i in 1:36) data[i] <- split(projectdata,projectdata[,i])
#for(i in 1:36) X[,i] <- discretize(X[,i], "frequency" ,categories=2)
#
#handling NAs
#
X<-projectdata[!apply(is.na(projectdata),1,any),]
X0 <- projectdat[!apply(is.na(projectdat),1,any),]
#pairs(X0[,c(2,5,11,26,37)])

#Association analysis of project data
trans<-as(X,"transactions")
head(trans)
#inspect(head(trans, 1))
#as(head(trans, 3),"matrix")
summary(trans)
inspect(trans)
dim(trans)
paracoord(X0[,c(2,5,11,26,37)])


rulesA <- apriori(trans,parameter=list(confidence=0.9,support=0.65))
rulesA <- sort(rulesA,by="lift")
inspect(rulesA)
rules2A <- apriori(trans, parameter = list(supp = 0.4, conf = 0.9,maxlen = 2,minlen = 2))
inspect(rules2A)
rulesAM <- apriori(trans,parameter=list(confidence=0.9,support=0.55))
inspect(rulesAM)
#plots
plot(rulesA, method = "grouped")
#
#plot(rulesA, method = "graph")

#plot(rulesA,main="")
#par(mfrow=c(1,2))
plot(rulesA, measure=c("support", "lift"), shading="confidence", main="",interactive=FALSE)
plot(rulesA, shading="order",main="",col=rainbow(5))

plot(rulesA, method="paracoord",control=list(reorder=TRUE),variable.names=TRUE)
plot(rules2A, method = "graph", main="")
#par(mfrow=c(1,1))
#sel <- plot(rulesA, measure=c("support", "lift"), shading="confidence", interactive=FALSE)
#plot(fitA, method="paracoord", control=list(reorder=TRUE))

#plot(fitA, method="graph", control=list(type="itemsets"))

#plot(rules2A)

#plot(rules2A, method = "grouped")


dev.off()
#plot(rules2A, method="graph", control=list(type="itemsets"))



#************************cluster analysis**************

#X0 <- matrix(as.numeric(unlist(trans)),nrow=nrow(trans))
# Clustering of the data
X <- as(trans,'matrix')
fitK2 <- kmeans(X, centers = 2, nstart = 20)

# Choosing k

K <- 10
vecWithinSS <- rep(NA, K)

for (k in 1:10)
{
  vecWithinSS[k] <- kmeans(X, centers = k, nstart = 20 )$tot.withinss
}

plot(vecWithinSS)
plot(vecWithinSS,col="red", xlab="Number of Clusters", ylab="Within groups sum of squares",type="b")
# Look at the k = 3 results
fitK3 <- kmeans(X, centers = 3, nstart = 20)
fitK3
round(fitK3$centers,5)
#plot(fitK3)
matplot(t(fitK3$centers),xlab="Measurement", ylab="Cluster means")
clusplot(X0[,-37], fitK3$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main ='')


# Inspect the results further
d <- dist(X,method = "manhatan")
#d <- dist(X)^2
silK3 <- silhouette(fitK3$cluster, d)
par(mfrow=c(1,2))
plot(silK3,main="")
#pdf('my_plot.pdf')
#plot(sil)
#dev.off()

# Using k-medoids
d <- dist(X, method = "dice")
fitK3pam <- pam(d, k = 3)
silK3pam <- silhouette(fitK3pam$clustering, d)
plot(silK3pam,main="")
par(mfrow=c(1,1))
X[fitK3pam$medoids,]
# Compare results
tab <- table(fitK3$cluster, fitK3pam$clustering)
tab
#library(fpc)
#cluster.stats(d, fitK3$cluster, fitK3pam$cluster)
summary(fitk3pam)
# cluster connection to pain types
cluster_distribution <- table(fitK3pam$clustering)
cluster_distribution
pain_types_distribution <- table(X0[,37])
pain_types_distribution 

XK <- cbind(X0,cluster_number=fitK3$cluster)
paintabK <- table(XK[37:38])
paintabK
XM <- cbind(X0,cluster_number=fitK3pam$clustering)
paintabM <- table(XM[37:38])
paintabM

# For ploting, let's make a vector of numbers
# with each number corresponding to a pain type
#colvec<-as.numeric(X0[,37])
# Plot the "true" clustering.
# The plot symbol (pch) and color (col) correspond to species
#plot(X0[,-37],col=fitK3$cluster,pch=colvec)
#K <- 3
#colvec<-rainbow(K,alpha=0.2)
#pairs(projectdata,col=colvec[fitK3$cluster],pch=19)

classAgreement(tab)
matchClasses(tab)
classAgreement(paintabM)
matchClasses(paintabM)
classAgreement(paintabK)
matchClasses(paintabK)
#prediction with glm
fit.l <- multinom(assigned.labels~.,data=X0)
#summary(fit2.glm)
pred <- predict(fit.l,type="class",data=X0)
pred
# Hosmer-Lemeshow Test
HLtest(Rsq(fit.l))
# Pearson Test
X2GOFtest(Rsq(fit.l))
#Accuracy of prediction
tab.l <- table(X0$assigned.labels,pred)
tab.l
acc.l <- sum(diag(tab.l))/sum(tab.l)
acc.l
#
#Defining data sets
set.seed(1024)
N <- nrow(X0)
indtrain <- sample(1:N,size=0.75*N,replace=FALSE)
indtrain <- sort(indtrain)
#indvalid <- sample(setdiff(1:N,indtrain),size=0.25*N)
#indvalid <- sort(indvalid)
indtest <- setdiff(1:N,indtrain)
indtest <- sort(indtest)

#predictive analysis
#paintype <- as.factor(X0$assigned.labels)
fit2.glm <- multinom(assigned.labels~.,data=X0,subset=indtrain)
#summary(fit2.glm)
pred <- predict(fit2.glm,type="class",data=X0)
#Predictive Performance
tab.glm <- table(X0$assigned.labels[indtest],pred[indtest])
tab.glm
acc.glm <- sum(diag(tab.glm))/sum(tab.glm)
acc.glm
#plot residuals
res <- residuals(fit2.glm,type="pearson")
plot(pred,res)

# Let's look at goodness of fit



# Fit a model with classification
fit1.r<-rpart(assigned.labels~.,data=X0,subset=indtrain)
summary(fit1.r)
plot(as.party(fit1.r))
pred <- predict(fit1.r,type="class",data=X0)
pred
tab.r <- table(X0$assigned.labels[indtest],pred[indtest])
tab.r
acc.r <- sum(diag(tab.r))/sum(tab.r)
acc.r


# Fit a model with bagging

fit1.bag<-bagging(assigned.labels~.,data=X0,subset=indtrain)
#summary(fit1.bag)
pred <- predict(fit1.bag,type="class",data=X0)
#pred
tab.bag <- table(X0$assigned.labels[indtest],pred[indtest])
tab.bag
acc.bag <- sum(diag(tab.bag))/sum(tab.bag)
acc.bag
library(partykit)
par(mfrow=c(5,3))
for (j in 1:15)
{
  plot(fit1.bag$trees[[j]],main=paste("Example ",j,sep=""))
  text(fit1.bag$trees[[j]],use.n=TRUE,xpd=TRUE,col="red")
}
par(mfrow=c(1,1))


# Fit a model with bagging
fit1.rf<-randomForest(assigned.labels~.,data=X0[indtrain,])
summary(fit1.rf)
varImpPlot(fit1.rf)
pred <- predict(fit1.rf,type="response",data=X0[indtest,])
pred
tab.rf <- table(X0$assigned.labels[indtest],pred[indtest])
tab.rf
acc.rf <- sum(diag(tab.rf))/sum(tab.rf)
acc.rf

#getTree(fit1.rf,k=1)
#model with ksvm polydot
fit.svm<- ksvm(assigned.labels~.,data=X0[indtrain, ],kernel="polydot",
               C=5,cross=3)
fit.svm
pred.svm <- predict(fit.svm,X0)
pred.svm
tab.svm <- table(X0$assigned.labels[indtest],pred.svm[indtest])
tab.svm
acc.svm <- sum(diag(tab.svm))/sum(tab.svm)
acc.svm
######################
# Mechanisms-based Classification of Musculoskeletal Pain
# 
# The aim of the present project is 1. To visualise any interesting patterns present in the presence/absence of the clinical criteria for lower back pain in the given data. 2. To find, whether the observations fall into groups with similar presence/absence of the clinical criteria and to look whether these groups have a corresponding connection with the clinical pain types in the given data. 3. To predict the pain types and asses the accuracy of prediction using the measurements, presence/absence of the given clinical criteria.
# Data Description: 
#   The given physio data contained 464 observations of 36 clinical criteria (Clinical Criteria description in Appendix: 1) and a categorical variable, pain type assigned to each observation by specialist physiotherapists. Pain types are classified into 3 types and labelled as central Neuropathic, Nociceptive, Peripheral Neuropathic (definitions in Appendix: 2). The observations with unfilled columns have been eliminated as it is been ensured that, the analysis will not be effected by the fewer missing observations. The effective data set contained 425 observations. 
# Exploratory Analysis:
#   The variable distributions are presented in the following table. X2, X11, X26, X2, X8 occur in most observations. Out of 36 variables, mean number of variables present in an observation is 13. 235 observations are categorized as the Nociceptive pain type.
# Table: 1 Variable Distribution:
#   Most frequent variables
# X5     X11     X26      X2      X8 (Other)                   
# 325     325     324     311     298    3947 
# 
# element (variables) length distribution in observations:
#   sizes
# 5  6  7   8    9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
# 1  4 21 39 48 42 35 22 38 25 21 25 31 21 14 14 10  6   4  3   1 
# 
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 5.00    9.00       13.00   13.01    16.00       25.00 
# 
# Distribution of Categorical variable:
#   Central Neuropathic     Nociceptive        Peripheral Neuropathic
# 95                   235                               95
# 1. Visualising any interesting patterns present in the presence/absence of the clinical criteria for lower back pain in the given data: 
#   Association analysis is carried out, using a priori algorithm by creating transaction version of the given data to look at the possible associations. To control the number of rules to manageable, support threshold of 0.65 and confidence of 0.9 have been used. The rules are sorted by lift values. The resulting 19 rules have been tabulated in Table: 2. The associations seems to be involving only the most frequent variables tabulated in the exploratory analysis. To facilitate the understanding of the associations several plots have been produced.
# Table: 2 Association rules 
# lhs              rhs        support      confidence     lift    
# 
# 16   {X5,X11}  => {X2}    0.6611765   0.9429530    1.288601
# 11   {X2,X26}  => {X11}  0.6847059   0.9831081    1.285603
# 15   {X2,X5}   => {X11}    0.6611765  0.9825175    1.284831
# 13   {X11,X26} => {X2}    0.6847059  0.9387097    1.282803
# 18   {X5,X26}  => {X11}   0.6729412  0.9794521    1.280822
# 12   {X2,X11}  => {X26}   0.6847059  0.9700000    1.272377
# 4     {X11}     => {X2}         0.7058824  0.9230769    1.261440
# 3    {X2}      => {X11}        0.7058824  0.9646302    1.261440
# 19  {X5,X11}  => {X26}   0.6729412  0.9597315   1.258907
# 7    {X11}     => {X26}       0.7294118  0.9538462   1.251187
# 6    {X26}     => {X11}       0.7294118  0.9567901   1.251187
# 1    {X2}      => {X26}        0.6964706  0.9517685   1.248462
# 2    {X26}     => {X2}         0.6964706  0.9135802   1.248462
# 14  {X2,X11}  => {X5}     0.6611765  0.9366667   1.224872
# 17  {X11,X26} => {X5}    0.6729412  0.9225806   1.206452
# 5    {X2}      => {X5}          0.6729412  0.9196141   1.202572
# 9    {X11}     => {X5}        0.7011765  0.9169231   1.199053
# 10  {X5}      => {X11}      0.7011765  0.9169231   1.199053
# 8   {X26}     => {X5}        0.6870588  0.9012346  1.178538
# 
# From the association table, we can see that X11, X26 co-occur in 73% of the observations.
# And X26, X11 occur together with X5 1.25(lift) times more than they would occur independently . Given X5, X26, there will be 98% probabilty of X11. Some rules are true in reverse
# direction as the another rule, emphasaizing the co occurance of the measurements in 
# several observations. The scatter plot in Figure:  1 shows that the higher the lift value, the 
# higher is the confidence for the rule. There are more rules in the low support region.
# 
# 
# Figure:  1 Scatter plot for 19 rules           
# 
# To study  number of measurements per rule (order) in relation  to confidence and support
# two-key plot is produced and shown in Figure: 2. In the low support and high confidence
# region, there are more of 3 order rules and the high support and lower confidence region
# is dominated by 2 order rules. Decreasing the support threshold to even lower, might have
# been resulted in more higher order rules and the other more frequently occurred variable
# X8 associations would have been accounted for. The probability of a measurement in rhs
# is higher in 3rd order rule than in the 2nd order rule, when it were to present in both.  
# 
# 
# Figure:  2 Two-key plot for 19 rules                         
# 
# From Figure: 3, rules that contained X5 has lower support and low lift.  X26, X11 are mostly present in high support rules. X2, X11 belong to high lift rules. All the six rules with X11 in 
# rhs are clearly visisble with support and lift grades. Another pictorial representation of rules
# 
# Figure:  3 Grouped matrix for 19 rules         
# 
# 
# is in Figure: 4. In this we can see X11 in 1 position has rules with X26, X5, X2 in second 
# position giving different measures in rhs. X11 rule with X2 rhs  showing higher lift (intense 
#                                                                                       colour). X5, X26 with X11 in rhs has high support (thick line).    
# 
# 
# 
# Figure:  4 Parallel coordinates plot for 19 rules                         
# 
# 
# 
# The a priori algorithm also ran by restricting the min and max length of the allowed variables in a rule to 2 and with minimum support 40% threshold. It gave 11 association rules and the plot with graph method is shown in Figure: 5. X8 variable has little association with X11. When the algorithm is ran with higher support, this association is missing in the 19 rules as its support is little less than the minimum threshold set in that attempt.  The lift for this association is also less than the other significant associations.
# 
# Figure:  5 Graph plot for 11 rules                         
# 
# 
# Interesting pattern to note is that most frequent variables contributed more times to Nociceptive pain type, in fact more than the sum of the other 2 pain types. The details are shown in Table: 3.
# Table: 3 Pain-types  distribution in the most frequent variables
# 
# X2    Central Neuropathic   Nociceptive   Peripheral Neuropathic
# 0                  91                            15                        8
# 1                   4                            220                       87
# 
# X5    Central Neuropathic   Nociceptive   Peripheral Neuropathic
# 0                  72                               13                       15
# 1                  23                              222                     80
# X11  Central Neuropathic   Nociceptive    Peripheral Neuropathic
# 0                  85                               6                        9
# 1                  10                             229                     86
# 
# X26  Central Neuropathic   Nociceptive    Peripheral Neuropathic
# 0                  84                          11                         6
# 1                  11                         224                       89
# 
# X8    Central Neuropathic  Nociceptive   Peripheral Neuropathic
# 0                  64                           8                        55
# 1                  31                         227                     40
# 
# 
# 
# 2. Investigation to check whether the observations fall into groups with similar presence/absence of the clinical criteria and to look whether these groups have a corresponding connection with the clinical pain types in the given data:
#   k-means cluster analysis is carried out. To determine the number of clusters, the k-means is been applied for 1 to 10 values of k (number of clusters). The plot of within cluster sum of squares and number of clusters is presented in Figure: 5. The elbow in the plot suggest that, the optimal value for number of clusters is 3.  The cluster analysis is carried out with 3 clusters and the cluster output tabulated in Table: 3.
# 
# Figure: 5 WithinSS  vs Number of Clusters
# Three clusters were split into clusters of size 228, 90, 107. The ratio between sum of squares and the total sum of squares is 40% indicating the goodness of clustering is average. The cluster centres presented in the table and also given in the pictorial representation in Figure: 5. Each variable cluster mean distribution seen in the table can be easily noted from the Figure as well. Highlighted the more frequent variables noted in the initial analysis, from these distributions all the 4 measurements X2, X5, X11, X26 are well away from the 1 and 3 clusters and very near to the 2nd cluster centre. X8 measurement is away from 1st but somewhat near to the 2nd and 3rd clusters. We can do similar study of the other variables. Some are well away from 2 clusters and some are near to 2 clusters. Which effect the clear definition of the cluster plane and diffusion of the measurement contribution to a specific cluster to its neighbouring cluster. 
# 
# 
# Table: 3 Cluster output with k=3
# K-means clustering with 3 clusters of sizes 228, 90, 107
# 
# Within cluster sum of squares by cluster:
#   [1] 766.1711  461.0778   520.7664
# (between_SS / total_SS =  40.3 %)
# 
# Cluster centres:
#   
#   X1             X2               X3       X4           X5             X6       X7
# 1 0.19298 0.92105 0.09211 0.06140 0.92982 0.05263 0.17982
# 2 0.03333 0.02222 0.15556 0.90000 0.22222 0.80000 0.54444
# 3 0.16822 0.92523 0.97196 0.03738 0.86916 0.14953 0.63551
# X8          X9            X10         X11        X12            X13      X14
# 1 0.95175 0.08772 0.01754 0.97368 0.05263 0.01316 0.14035
# 2 0.31111 0.10000 0.67778 0.04444 0.06667 0.92222 0.45556
# 3 0.49533 0.96262 0.00935 0.92523 0.91589 0.00935 0.38318
# X15               X16     X17        X18        X19        X20        X21
# 1 0.04825 0.03070 0.10965 0.35965 0.79386 0.40789 0.41228
# 2 0.35556 0.74444 0.24444 0.87778 0.31111 0.00000 0.96667
# 3 0.48598 0.20561 0.83178 0.67290 0.67290 0.47664 0.33645
# X22            X23           X24     X25       X26           X27     X28
# 1 0.18860 0.12719 0.08772 0.14474 0.95614 0.11404 0.00439
# 2 0.77778 0.86667 0.81111 0.52222 0.05556 0.17778 0.86667
# 3 0.15888 0.08411 0.33645 0.45794 0.94393 0.89720 0.00935
# X29         X30        X31            X32       X33          X34     X35
# 1 0.04386 0.84211 0.03070 0.01316 0.20175 0.17105 0.07018
# 2 0.06667 0.26667 0.76667 0.33333 0.61111 0.30000 0.20000
# 3 0.73832 0.74766 0.00000 0.03738 0.27103 0.25234 0.57944
# X36
# 1 0.14474
# 2 0.90000
# 3 0.09346
# 
# Figure: 5 Cluster means for each variable
# To further investigate, how well the clusters are divided, the cluster analysis plot is produced and shown in Figure: 6, which is 2D plot of the present cluster analysis. It facilitated the visualisation of the individual clusters in separate planes and the observations common to 2 clusters and the observations which are in border can be visualised and so the clusters are not perfectly separated. This could be because of some measurements (variables) might be equal weighted in more than one cluster. There is overlap in the planes indicating tight packing of the observations near the border of the clusters 1-2 and 1-3, which makes it difficult to classify them to a particular cluster. In the first cluster 228 observations are tightly packed and the observations in the second cluster are more dispersed and even spread into other clusters. There are many observations near 3 to 1 cluster boarder.
# 
# Figure: 6 2D-Cluster analysis plot
# To quantify the clustering analysis silhoutte is calculated for k-means results with distance as squared Euclidean distance. Analysis is also been carried out with another algorithm K-medoids with distance calculated using ‘dice’ method. The silhoutte ploted for these results as well and both the plots presented in Figure 7 (a) and (b). the cluster sizes of k-means are 90, 228, 107 and k-medoids are 214, 120, 91. Average silhouette of k-means is 42% and of k-medoid is 3%. For this data k-means cluster approach performed better than k-medoids. In k-means the large cluster has high average silhouette and in k-medoids the smaller cluster has high average silhouette. Average silhoutte values of some individual observations have smaller indicating these could belong to neighbouring cluster. In k-medoids there are large number of observations with less average silhoutte values indicating its poor performance in clustering of the given data set. The clusters prdouced with different distance methods resulted in the similar outputs.
# 
# (a)                                                                                (b)
# Figure: 7 Silhouette Plots (a) With k-means (b) With K-medoids
# K-means assigns the cluster number randomly so cluster number in one algorithm analysis will not be in one to one correspondance with the cluster number in the algorithm analysis. For comparing the 2 results, how well they correlated with each other, confusion table has been calculated to correct for the random assignment of the clusters adjusted RAND index calculated and it shows 86% of the results agree with one another. Moreover the matchClasses method showed that cluster 2 in one method is cluster 3 in the other and viceversa and there are 95.3% matched pairs. 
# Table: 3 comparison of k-means, k-medoids results 
# K-means and K-medios  Confusion table 
# 1       2     3                                                                                diag  0.5035294
# 1    212  15   1                                                                               kappa  0.1942657
# 2    0      1      89                                                                              rand   0.9323862
# 3    2     104   1                                                                             crand  0.8574179
# Cases in matched pairs: 95.29 %
# 1 2 3 
# 1 3 2 
# To look for the  connection between the clusters and the categorical variable, assigned.labels, which is basically classified pain types with clinical names Central Neuropathic, Nociceptive, Peripheral Neuropathic (clinical definitions for these pain types are given in Appendix: 2), the cluster vector is appended to the data frame and so each observation is assigned to a cluster. Then the categorical variable is tabulated along with cluster number for the 2 algorithms analysis.     
# Table: 4 Clusters connection to categorical variable distribution
# k-means
# cluster_number                                                            Pain types
# assigned.labels                      1    2   3                                                                  Distribution
# Central Neuropathic           6  87   2                                                                       95
# Nociceptive                       220   2  13                                                                    235
# Peripheral Neuropathic       2   1  92                                                                      95
# 
# 6% misclassification in the clusters from the original pain types distribution
# 
# k-medoids
# cluster_number                                                          Pain types
# assigned.labels                       1   2   3                                                                Distribution
# Central Neuropathic            3   5  87                                                                       95
# Nociceptive                       207  26   213                                                                235
# Peripheral Neuropathic       4  89   2                                                                      95
# 
# 10% misclassification in the clusters from original pain type distribution
# 
# The measurements contributing to the observations in the borders of the cluster planes might have equal effect towards the individual pain types, making it difficult to place the observation in a particular pain type.
# Predictive Analysis:
#   3. Prediction of the clinical pain types and assessment of the accuracy of prediction using the measurements, presence/absence of the given clinical criteria:
#   Initially Multinomial logistic regression has been employed on the whole data to predict the pain types and compared with the given types and noted the prediction was 100% accurate. The results were tabulated in Table: 5.
# Table: 5 Multinomial regression 
# prediction
# Central               Nociceptive       Peripheral 
# Assigned.labels             Neuropathic                                  Neuropathic
# Central Neuropathic              95                        0                       0
# Nociceptive                               0                        235                    0
# Peripheral Neuropathic           0                         0                       95
# 
# Here the prediction was made on the trained data, considering the whole data set.  To predict on the new observations, prediction on the untrained data set will be ideal, to achieve this, the given data set has been split into 3:1, training to test set ratio at a fixed seed. Employed 5 classification algorithms multinomial regression, recursive partitioning classification, bagging, random forests and support vector machines to find which algorithm gives better prediction for the given data set. For this training set and test set ratio multinomial regression algorithm for type class predicted with 37% accuracy, which is not good enough. The residuals plot for Pearson type is shown in Figure: 8. The residual plot does not particularly look bad, when the observed value is equal to the expected value numerator becomes zero in the Pearson statistic. The model fitted is in the right direction. Easy solution to increase the model fit for accuracy using multinomial regression approach is increasing the sample size. Before exploring, increase in size of the training set, the other algorithms to be tested for their prediction performance.
# 
# Figure: 8 Pearson residual plot of Multinomial regression model 
# The Recursive Partitioning classification (rpart) algorithm predicted with accuracy 33.3%, bagging algorithm with 34.6%, Random Forest with 36% and Support vector Machines with polydot kernel predicted with accuracy of 92.5%. SVM predicted very well for the given data set. From Figure: 9 we can observe that for the Nociceptive pain type the rpart fit is trying to create 2 nodes, indicating that there are observations which are difficult to classify into single category. 
# 
# Figure: 9 Recursive partition algorithm model fit
# Random Forest algorithm is known to perform well for many problems with large data sets. The variable importance plot with mean decrease in Gini index is shown in Figure: 10.  Groups of variables have similar mean decrease in Gini index over all the trees in the forest they belong, adding up complexity to the task of decoupling the contribution of variables to the observations, which intern effecting the classification of observations. The size of the data set could be the reason for its under-performance.   SVM targeted all these problems very well for the given data set and hence high prediction accuracy. 
# To analyse if by increasing the sample set size, whether other algorithms works better, the data is sampled with training set size of 85% and employed the above algorithms to fit the data with training set and then predict on the test set. The process has been repeated by choosing a sample of 95% of the data as the training set (which leaves just 21 observations for the test set). The sampling of data is done with replacement = False and also with replacement = true. Only the results from the first type are tabulated, because the results by opting for second type are comparatively poorer. Also set the seed fixed for all the algorithms employed. By random sampling the data, there has been 3 to 15% variation in the accuracy of the predictions. Table: 6 presents the prediction accuracies of the 5 algorithms employed. 
# 
# 
# Figure: 10 Variable importance plot of Random Forest Model.
# Conclusions:
#   Association analysis is carried out to find out the interesting patterns in the given data set. Interestingly in the 19 rules that obtained by setting support threshold to .65 and confidence 9.0, the associations are of the most frequent variables X2, X5, X11, X26. Noted, that the contributions of these variables to one pain type, Nociceptive are higher than other 2 pain types. Also looked at the associations obtained by decreasing the support threshold to .55, the resultant rules involved the associations of the extra variables X8, X19, X30. As noted in the cluster means table the distribution of cluster means of 4 variables X2, X5, X11, X26 are similar and the distributions of the later 3 are similar.
# The cluster analysis carried out by k-means algorithm, helped in the thorough understanding of the distribution of the observations. The data is found to form 3 distinctive clusters with some observations fall near to 2 clusters as shown in 2d cluster plot Figure: 6. Augmenting the cluster vector to the data frame, each observation’s membership to a cluster is established. So the 3 clinical pain types are connected to these clusters with larger one being Nociceptive pain type. There seems to be few misclassified observations.  K-means performed better than k-medoids in cluster analysis for the given data set.
# Table: 6   Prediction Accuracies
# Training sample sizes
# 75%                  85%               95%
# Algorithm                                                Prediction Accuracies
# Multinom Regression                    37%                   50%              57%
# Recursive partition                         33.3%               52%              57%
# Bagging                                          34.6%               54%              52.4%
# Random Forest                               36%                 54%              66.7%
# SVM                                                 92.5%              90.6%          100%
# The predictive analysis is then carried out with various training set and test sample sizes and with different algorithms. Initially with single data set the regression analysis predicted with 100% accuracy. For predicting on the new data SVM worked better than other algorithms. The reason being SVM considers the linear combination of the variables and additional employment of the kernel accounts for the non-linearities. The polydot kernel worked well in targeting the classification of observations in the complex zone as in equal distance from the 2 cluster centres. Increase in the sample size improved the performance of the other algorithms. The complexity of the relations of the given 36 variables in classifying them to specific groups is higher, more observations could have facilitated other algorithms also to work better for the given data complexity. SVM could predict accurately on the given data.
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# Appendix: 1    http://mathsci.ucd.ie/~brendan/data/PainCodes.txt
# 1	Pain of recent onset.
# 2	Pain assoc'd trauma, pathology, movt.
# 3	History of nerve injury, trauma.
# 4	Pain disproportionate to injury, pathology.
# 5	Intermittent + sharp or constant dull ache
# 6	More constant, unremitting.
# 7	Burning, shooting, sharp, electric-shock like.
# 8	Localised to area of injury, dysfunction.
# 9	Referred in dermatomal, cutaneous distribution.
# 10	Widespread, non-anatomical distribution.
# 11	Mechanical nature to aggs + eases
# 12	Mechanical pattern assoc'd with movt, loading, compression of neural tissue.
# 13	Disproportionate, non-mechanical pattern to aggs + eases.
# 14	Spontaneous, paroxysmal pain.
# 15	Pain with dyesthesias.
# 16	Pain of high severity and irritability.
# 17	Pain with neurological symptoms.
# 18	Night pain, disturbed sleep.
# 19	Responsive to simple analgesia, NSAIDS.
# 20	Rapidly resolving, resolving with expected tissue healing, pathology recovery times.
# 21	Pain persisting beyond expected tissue healing, pathology recovery times.
# 22	History of failed interventions.
# 23	Strong association with maladaptive psychosocial factors.
# 24	Pain with high levels of functional disability.
# 25	Antalgic postures, movement patterns.
# 26	Consistent, proportionate pain reproduction on mechanical testing.
# 27	Pain, symptom provocation with mechanical tests that move,load,compress neural tissue.
# 28	Disproportionate, non-mechanical pattern of pain provocation on mechanical testing.
# 29	Positive neurological findings.
# 30	Localised pain on palpation.
# 31	Diffuse, non-anatomic areas of pain on palpation.
# 32	Positive findings of allodynia.
# 33	Positive findings of hyperalgesia.
# 34	Positive findings of hyperpathia.
# 35	Pain, symptom provocation on palpation of neural tissues.
# 36	Positive identification of psychosocial factors.
# 
# Appendix: 2
# • Nociceptive: Pain that arises from actual or threatened damage to non-neural tissue, occurring with a normally functioning somatosensory nervous system;
# • Peripheral Neuropathic: Pain initiated or caused by a primary lesion or dysfunction in the peripheral nervous system;
# • Central Neuropathic: Pain initiated or caused by a primary lesion or dysfunction in the central nervous system