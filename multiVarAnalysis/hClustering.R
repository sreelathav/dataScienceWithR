#read olive oil data
oliveoil<- read.csv('~/data/OliveOilData.csv')
head(oliveoil)
names(oliveoil)
#
acids = oliveoil[,3:10]
names(acids)
#hclust with different linkage models
cl.average = hclust(dist(acids), method="average")
plot(cl.average)
cl.single = hclust(dist(acids), method="single")
plot(cl.single)
cl.complete = hclust(dist(acids), method="complete")
plot(cl.complete)
 #with different distance measures
 cl.completeEu = hclust(dist(acids,method="euclidean"), method="complete")
 plot(cl.completeEu)
 cl.completeMan = hclust(dist(acids,method="manhattan"), method="complete")
 plot(cl.completeMan)
 cl.completeMax = hclust(dist(acids,method="maximum"), method="complete")
 plot(cl.completeMax)
 
 #cut the dendogram of clustering
 hcl = cutree(cl.average, k = 3)
 hcl
 table(hcl)
 table(hcl, oliveoil[,1])
 #
 StDev = apply(acids, 2, sd)
 StDev
 stdacids = sweep(acids, 2, StDev, "/")
 stdacids
 #clustering using std data
 cl.comp = hclust(dist(stdacids,method="manhattan"), method="complete")
 plot(cl.comp)
#micro array gene expression data 
 microGene<- read.csv('~/data/microarraydata.csv')
 head(microGene,2)
 dim(microGene)
 microGene[2,1:100]
 #names(microGene)
 plot(1:6829, microGene[1, 2:6830], type="l", col=as.numeric(microGene[1,1]),
      xlab="Gene number", ylab="Expression level")
 points(1:6829, microGene[4,2:6830], type="l", col=as.numeric(microGene[4,1]))
 points(1:6829, microGene[5,2:6830], type="l", col=as.numeric(microGene[5,1]))
 #hclust with different linkage models
 cl.average = hclust(dist(microGene), method="average")
 plot(cl.average)
 
 cl.single = hclust(dist(microGene), method="single")
 plot(cl.single)
 cl.complete = hclust(dist(microGene), method="complete")
 plot(cl.complete)
 