##########Multi dimensional scaling
library(mclust)
oliveoil<- read.csv('~/data/OliveOilData.csv')
head(oliveoil)
names(oliveoil)
n=  nrow(oliveoil)
S=sample(c(1:n)[oliveoil[,1] == "South"], size=30)
D=sample(c(1:n)[oliveoil[,1] == "Sardinia"], size=30)
N=sample(c(1:n)[oliveoil[,1] == "North"], size=30)
#Extract the columns of the data which record the fatty acids data for these sampled
#observations:
acids = oliveoil[c(S,D,N),3:10]
loc = cmdscale(dist(acids), k=2, eig=TRUE)
loc

#following code gives the proportion of variation accounted for when q = 2.
sum(abs(loc$eig[1:2]))/sum(abs(loc$eig))
#
x = loc$points[,1]
y = loc$points[,2]
cols = c(rep(1,30), rep(2,30), rep(3,30))
plot(x, y, type="n", xlab="", ylab="", main="Classical")
text(x, y, oliveoil[c(S,D,N),1], cex=1, col=cols)

###### metric least squares scaling
library(MASS)
?sammon
loc2= sammon(dist(acids),k=2,trace=TRUE)
#plot
x = loc2$points[,1]
y = loc2$points[,2]
cols = c(rep(1,30), rep(2,30), rep(3,30))
plot(x, y, type="n", xlab="", ylab="", main="Metric least squares")
text(x, y, oliveoil[c(S,D,N),1], cex=1, col=cols)

#####Kruskal’s non-metric scaling
?isoMDS
loc3= isoMDS(dist(acids),k=2,trace=TRUE)
#plot
x = loc3$points[,1]
y = loc3$points[,2]
cols = c(rep(1,30), rep(2,30), rep(3,30))
plot(x, y, type="n", xlab="", ylab="", main="Metric least squares")
text(x, y, oliveoil[c(S,D,N),1], cex=1, col=cols)

####procrustes analysis
#install.packages("vegan")
library(vegan)
? procrustes
proc12 = procrustes(loc$points, loc2$points)
proc23 = procrustes(loc2$points, loc3$points)
proc31 = procrustes(loc3$points, loc$points)
plot(proc12)
plot(proc23)
plot(proc31)
plot(proc12, kind=2)
plot(proc23, kind=2)
plot(proc31, kind=2)
residuals(proc12)
residuals(proc23)
residuals(proc31)
