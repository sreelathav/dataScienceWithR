##########factor scores

x = read.csv('~/data/SubjectMarks.csv')
res = factanal(x, 2, rotation="none")
sum(res$uniqueness)
#

plot(res$loadings, type="n", xlim=c(-0.5, 1), ylim=c(-0.5, 1))
text(res$loadings[,1], res$loadings[,2], colnames(x))
abline(v=0, lty=2, col=2)
abline(h=0, lty=2, col=2)
#
res = factanal(x, 2, rotation="none", scores="regression")
plot(res$scores, type="n")
text(res$scores[,1], res$scores[,2], rownames(x))
#
avg = apply(x, 1, mean)
N = nrow(x)
maxind = which(avg == max(avg))
minind = which(avg == min(avg))
plot(res$scores)
points(res$scores[maxind,1], res$scores[maxind,2], col=2, pch=2)
points(res$scores[minind,1], res$scores[minind,2], col=3, pch=3)
