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