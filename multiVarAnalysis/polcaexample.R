#install.packages("poLCA")
library("poLCA")

# By the way, for all examples in this article, you´ll need some more packages:
library("reshape2")
library("plyr")
library("dplyr")
library("poLCA")
library("ggplot2")
library("ggparallel")
library("igraph")
library("tidyr")
library("knitr")

# these are the defaults of the poLCA command
poLCA(formula, data, nclass=2, maxiter=1000, graphs=FALSE, tol=1e-10, na.rm=TRUE, probs.start=NULL, nrep=1, verbose=TRUE, calc.se=TRUE)

#estimate the model with k-classes
k<-3
lc<-poLCA(f, data, nclass=k, nrep=30, na.rm=FALSE, Graph=TRUE
          ################
          # select variables
          mydata <- data %>% dplyr::select(F29_a,F29_b,F29_c,F27_a,F27_b,F27_e,F09_a, F09_b, F09_c)
          
          # define function
          f<-with(mydata, cbind(F29_a,F29_b,F29_c,F27_a,F27_b,F27_e,F09_a, F09_b, F09_c)~1) #
          
          #------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC
          max_II <- -100000
          min_bic <- 100000
          for(i in 2:10){
            lc <- poLCA(f, mydata, nclass=i, maxiter=3000, 
                        tol=1e-5, na.rm=FALSE,  
                        nrep=10, verbose=TRUE, calc.se=TRUE)
            if(lc$bic < min_bic){
              min_bic <- lc$bic
              LCA_best_model<-lc
            }
          }    	
          LCA_best_model
          ##########
           