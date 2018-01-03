library(arules)

# Load the PainData from the internet
load(url("http://mathsci.ucd.ie/~brendan/data/PainData.Rdata"))

# Look at the data (it's in transaction format)
inspect(X0)

# Look at the data (in matrix format)
X <- as(X0, "matrix")
X

# Look at the frequency of each pain criterion
summary(X0)

# Do a first analysis of the data using the apriori algorithm

fit <- apriori(X0, parameter = list(supp = 0.6, conf = 0.9))

fit <- sort(fit, by = "lift")

# Do another analysis to look at the relationships graphically.

library(arulesViz)
fit2 <- apriori(X0, parameter = list(supp = 0.3, conf = 0.9,maxlen = 2,minlen = 2))

plot(fit2)

plot(fit2, method = "grouped")

plot(fit2, method = "graph")
############################
# Load the PainData from the internet
load(url("http://mathsci.ucd.ie/~brendan/data/PainData.Rdata"))

# Look at the data (it's in transaction format)
inspect(X0)

# Look at the data (in matrix format)
X <- as(X0, "matrix")
X

# Clustering of the data

fit <- kmeans(X, centers = 2, nstart = 20)

# Choosing k

K <- 20
SSvec <- rep(NA, K)

for (k in 1:20)
{
  SSvec[k] <- kmeans(X, centers = k, nstart = 20 )$tot.withinss
}

plot(SSvec)

# Look at the k = 3 results
fit <- kmeans(X, centers = 3, nstart = 20)
fit

# Inspect the results further
library(cluster)

d <- dist(X)^2
sil <- silhouette(fit$cluster, d)
plot(sil)

# Using k-medoids
d <- dist(X, method = "binary")
fit2 <- pam(d, k = 3)

X[fit2$medoids,]
# Compare results
table(fit$cluster, fit2$clustering)
###########################################

