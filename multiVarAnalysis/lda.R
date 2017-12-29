#######linear discriminant analysis
library(MASS)
diabetes <- read.csv("data/diabetes.csv")
head(diabetes)
pairs(diabetes)
pairs(diabetes[,2:4], main="Diabetes pairs plot", pch=22, 
      bg=c("red", "blue","green")[unclass(diabetes$class)])
#lda calculation
lda.res <- lda(class ~ .,data=diabetes)
attributes(lda.res)
lda.res$prior
lda.res$means
#
N <- nrow(diabetes)
N
G <- length(levels(diabetes$class))
G
#subset on class
diabetes.norm <- subset(diabetes, class == "normal")
diabetes.chem <- subset(diabetes, class == "chemical")
diabetes.over <- subset(diabetes, class == "overt")
#cov  in each class
cov_norm <- cov(diabetes.norm[,2:4])
cov_chem <- cov(diabetes.chem[,2:4])
cov_over <- cov(diabetes.over[,2:4])
cov_all<-((cov_norm*(nrow(diabetes.norm)-1)) + (cov_chem*(nrow(diabetes.chem)-1)) +
              
              (cov_over*(nrow(diabetes.over)-1)))/(N - G)
#function to calculate ldf
ldf <- function(x, prior, mu, covar)
   {
    
      x <- matrix(as.numeric(x), ncol=1)
      log(prior) - (0.5*t(mu)%*%solve(covar)%*%mu) + (t(x)%*%solve(covar)%*%mu)
}
#calculate ldf for nth patient
id <- 1
dfs <- rep(0, G)
for(g in 1:G)
{
  dfs[g] <- ldf(diabetes[id,2:4], lda.res$prior[g], lda.res$means[g,], cov_all)
}
dfs
levels(diabetes$class)[dfs == max(dfs)]
#using cv
lda.res.cv <- lda(class ~ .,CV= TRUE, data=diabetes)
tabLDA <- table(lda.res.cv$class, diabetes$class)
#dfs comparision
round(exp(dfs)/sum(exp(dfs)), 4)
round(lda.res.cv$posterior[id,], 4)
#miss classification rate lda
mcLDA <-  1-sum(diag(tabLDA))/sum(tabLDA)
#qda
qda.res.cv <- qda(class ~ glucose + insulin + sspg, CV=TRUE, data=diabetes)
tabQDA <- table(qda.res.cv$class, diabetes$class)
mcQDA <- 1-sum(diag(tabQDA))/sum(tabQDA)
mcQDA-mcLDA
