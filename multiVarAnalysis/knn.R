library(MASS)
N <- 900
muA <- c(0,0)
SigmaA <- matrix(c(10,3,3,2),2,2)

x <- mvrnorm(n=N, muA, SigmaA)
kdfitx <- kde2d(x[,1], x[,2], n=10)
plot.new()
contour(kdfitx, add=TRUE, col="red", nlevels=6)
#
muB <- c(5,3)
SigmaB <- matrix(c(12,2,2,15),2,2)
y <- mvrnorm(n=N, muB, SigmaB)
z <- rbind(x, y)
dim(z)
cls <- c(rep(1,N), rep(2,N))
z.dat <- data.frame(z, cls)
#
kdfity <- kde2d(y[,1], y[,2], n=10)
plot(z.dat[,1:2], type="n")
contour(kdfitx, add=TRUE, col="red", nlevels=6)
contour(kdfity, add=TRUE, col="blue", nlevels=6)
#
index <- c(1:600, 901:1500)
train <- z.dat[index, 1:2]
test <- z.dat[-index, 1:2]
knn(train, test, cl = z.dat[index,3], k=3)
result <- knn(train, test, cl=z.dat[index, 3], k=3)
(nrow(test) - sum(diag(table(result, z.dat[-index,3])))) / nrow(test)
#

#
WGSS <- rep(0,10)

WGSS[1] <- (N-1) * sum(apply(train, 2, var))
for(k in 2:10)
{
  
  WGSS[k] <- sum(knn(train, test, cl = z.dat[index,3], k)$withinss)
}
plot(1:10, WGSS, type="b", xlab="k", ylab="Within group sum of squares")
#
unknown1 <- mvrnorm(n=4, muA, SigmaA)
unknown2 <- mvrnorm(n=4, muB, SigmaB)
unknown <- rbind(unknown1, unknown2)
dim(unknown)
cls <- c(rep(1,4), rep(2,4))
unknown <- data.frame(unknown, cls)
unknown
res <- knn(train,unknown[,1:2],cl=z.dat[index,3],k=2)
res
(nrow(test) - sum(diag(table(res, unknown[,3])))) / nrow(test)
#
kdfitz<- kde2d(z.dat[,1], z.dat[,2], n=10)
plot(z.dat[,1:2], type="n")
contour(kdfitz, add=TRUE, col="red", nlevels=8)
points(1:2, unknown[1,1:2], type="p", col=unknown[,3])
points(1:2, unknown[2,1:2], type="p", col=unknown[,3])
points(1:2, unknown[3,1:2], type="p", col=unknown[,3])
points(1:2, unknown[4,1:2], type="p", col=unknown[,3])
points(1:2, unknown[5,1:2], type="p", col=unknown[,3])
points(1:2, unknown[6,1:2], type="p", col=unknown[,3])
points(1:2, unknown[7,1:2], type="p", col=unknown[,3])
points(1:2, unknown[8,1:2], type="p", col=unknown[,3])

#contour(kdfity, add=TRUE, col="blue", nlevels=6)
#