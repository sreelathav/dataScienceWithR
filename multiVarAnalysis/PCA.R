##########principal component analysis
data(iris)
fit = prcomp(iris[,1:4])
fit
#To see what values have been returned by the prcomp function we use:
names(fit)
fit$sdev
summary(fit)
round(fit$rotation, 2)
plot(fit)
newiris = predict(fit)
newiris
plot(newiris[,1], newiris[,2], type="n", xlab="PC1", ylab="PC2")
text(newiris[,1], newiris[,2], labels=substr(iris[,5],1,2),
       col=as.integer(iris[,5]))
apply(iris[,1:4], 2, var)
# variances are different so better standardise
iris.dat = iris[,1:4]
sds = apply(iris.dat, 2, sd)
iris.std = sweep(iris.dat, 2, sds, "/")
fit.std = prcomp(iris.std)
#
fit.std <- prcomp(iris[,1:4], scale.=T)
plot(fit.std)
newiris.std = predict(fit.std)
newiris.std
#
oliveoil<- read.csv('~/data/OliveOilData.csv')
head(oliveoil)
names(oliveoil)
n=  nrow(oliveoil)
#Next examine the variance associated with each variable in the original data set:
apply(oliveoil[,3:10], 2, var)
oil.std = sweep(oliveoil[,3:10], 2, sds, "/")
fit.std = prcomp(oil.std)
plot(fit.std)
#
newoil = predict(fit.std)
newoil
plot(newoil[,1], newoil[,2], type="n", xlab="PC1", ylab="PC2")
text(newoil[,1], newoil[,2], labels=substr(oliveoil[,1],1,2),
     col=as.integer(oliveoil[,1]))
