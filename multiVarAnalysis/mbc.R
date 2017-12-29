                      ###Model based clustering
library(mclust)
data(thyroid)
pairs(thyroid[,-1])
res = Mclust(thyroid[,-1], G=1:5)
summ = summary(res)
summ
res$BIC
plot(res, what="BIC", legendArgs = list(x="topleft", cex=0.5,
                                        horiz=T), ylim=c(-7400, -4000))
plot(res, what="classification")
attributes(summ)
summ$pro
summ$mean
summ$variance
plot(res, what="uncertainty")
plot(res$uncertainty, type="h")
table(thyroid[,1], summ$classification)
adjustedRandIndex(thyroid[,1], summ$classification)
