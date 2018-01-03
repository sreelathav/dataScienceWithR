#Load the Old Faithful Data
data(faithful)
#Count how many observations are in the data
N <- nrow(faithful)
#Decide how many bootstrap samples to take
B <- 50
#Choose the maximum number of clusters to fit
Kmax <- 10
#Set up a matrix to store the results.
res<-matrix(NA,B,Kmax)
#Loop over the number of bootstrap samples (from 1 to B).
for (b in 1:B)
{
  #Loop over the number of clusters from 1 to Kmax.
  1
  for (k in 1:Kmax)
  {
    #Sample the observation indices with replacement.
    ind<-sample(1:N,replace=TRUE)
    # Make a copy of the data with each observation occurring
    #as often as its index happens
    xnew<-faithful[ind,]
    #Run k-means for this value of k.
    #Do 4 random starts to avoid local minima
    fit<-kmeans(xnew,centers=k,nstart=4)
    #Store the total within sum of squares
    res[b,k]<-fit$tot.withinss
    #Close the loop over k
  } #k
  # Close the loop over b.
} #b
apply(res,2,summary)
#Compute the 2.5%, 50% and 97.5% quantiles of the statistic for each k
withinss <- apply(res,2,quantile,probs=c(0.025,0.500,0.975))
withinss
#Plot the quantiles
matplot(t(withinss))
