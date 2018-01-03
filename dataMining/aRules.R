Data Mining Code (Association Rules)
#
#
# This set of notes shows how we can use association rule mining to analyze voting data. 
# The example uses a famous data from the US Congress where there sixteen key votes from 1984 are considered. 
# There are 435 members of congress and they are either Democrat or Republican.
# We can investigate if there are structures in the voting behaviour of the congress members
# Further details on the data are available here: http://archive.ics.uci.edu/ml/datasets/Congressional+Voting+Records

# Turn on the arules package

library(arules)

# Read in data from the web

dat <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data",sep=",")

# Give sensible column names to data.

colnames(dat) <- c("Party",paste("Vote",1:16,sep=""))

# Look at the data

dat

# Note that the ?'s are people being absent from the vote.
# We recode the ?'s as n's

dat[dat=="?"] <- "n"

# I will remove "party" from the data so that we only analyze votes.

dat<-dat[,-1]

# Let's make a binary version of the data (1="y" and 0="n")

datnew <- 1*(dat=="y")
colnames(datnew) <- paste("Yes",1:16,sep="")

# Getting the data into the transaction format that arules uses. We look at the data again.

votes <- as(datnew,"transactions")
inspect(votes)

# We mine all assocation rules with support greater than 0.4 and confidence greater than 0.8

fit<-apriori(votes,parameter=list(support=0.4,confidence=0.8))
fit<-sort(fit,by="support")
inspect(fit)

# Note that we are treating the votes in an asymmetric manner. 
# It could be argued that treating "y" as the item of interest is arbitrary.
# We could instead move the focus to the "n"s. 

datnew <- 1*(dat=="n")
colnames(datnew)<-paste("No",1:16,sep="")

# Getting the data into the format that arules uses.

novotes <- as(datnew,"transactions")
inspect(novotes)

fit<-apriori(novotes,parameter=list(support=0.4,confidence=0.8))
fit<-sort(fit,by="support")
inspect(fit)

# It could be further argued that treating "n" or "y" is still arbitrary.
# We could construct the dataset with both "y" and "n" votes recorded. 

allvotes<-merge(votes,novotes)
inspect(allvotes)

fit<-apriori(allvotes,parameter=list(support=0.4,confidence=0.8))
fit<-sort(fit,by="support")
inspect(fit)

# We get many more rules so we may wish to consider changing the parameter settings. 
# In particular, we should increase the support threshold. 

fit<-apriori(allvotes,parameter=list(support=0.45,confidence=0.8))
fit<-sort(fit,by="support")
inspect(fit)
#################
#Load arules and data
library(arules)
data(Groceries)

# Items with support greater than threshold

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=1,maxlen=1))
fit <- sort(fit,by="support")
inspect(fit)

# Pairs of items with support greater than threshold

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=2,maxlen=2))
fit <- sort(fit,by="support")
inspect(fit)

# Pairs of items with support greater than threshold (confidence threshold too)

fit <- apriori(Groceries,parameter=list(confidence=0.5,support=0.01,minlen=2,maxlen=2))
fit <- sort(fit,by="support")
inspect(fit)

# Triples of items with support greater than threshold

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=3,maxlen=3))
fit <- sort(fit,by="support")
inspect(fit)

# Triples of items with support greater than threshold (confidence threshold too)

fit <- apriori(Groceries,parameter=list(confidence=0.5,support=0.01,minlen=3,maxlen=3))
fit <- sort(fit,by="support")
inspect(fit)

# Quadruples of items with support greater than threshold

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=4,maxlen=4))
fit <- sort(fit,by="support")
inspect(fit)
##################

##########
dat <-read.table("http://mathsci.ucd.ie/~brendan/data/Old/nltcs.txt",header=TRUE)
head(dat)
tail(dat)
# Count how many patterns occur in the data and call it M.
M <- nrow(dat)
#Make a vector of numbers from 1 to M.
indices <- 1:M
#Extract the count for how often each pattern happens.
counts <- dat$COUNT
#Construct a vector where the row number of each pattern
#is recorded the number of times that the pattern arises
#in the data. The rep() command is used for this.
rowindices <- rep(indices,counts)
# Now, let's create the matrix nltcsmat.
nltcsmat <- dat[rowindices,]
#Let's drop the last two columns because they're not needed
nltcsmat <- nltcsmat[,-(17:18)]
#Let's reorder the columns, so that they give disabilities
# from 1 to 16 instead of 16 to 1.
nltcsmat <- nltcsmat[,16:1]
#Let's coerce the data.frame into a matrix.
nltcsmat <- as.matrix(nltcsmat)
#writing data in transaction format
nltcs <- as(nltcsmat,"transactions")
#some analysis
table(nltcsmat[,7])
table(nltcsmat[,8])
table(nltcsmat[,7], nltcsmat[,8])
#Get a table of the outcome for each disability
apply(nltcsmat,2,table)
# Let's look at the distribution of the number of disabilities per person.
# First, store the number of disabilties per person and then compute
# some summaries
disabilitycount <- apply(nltcsmat,1,sum)
head(disabilitycount)
hist(disabilitycount)
summary(disabilitycount)
library("arulesViz")
rules <- apriori(nltcs, parameter=list(support=0.04, confidence=.99))
rules<-sort(rules,by="support")
inspect(head(rules))
plot(rules)
head(quality(rules))
plot(rules, measure=c("support", "lift"), shading="confidence")
interestMeasure(rules, c("support", "chiSquare", "confidence", "conviction",
                         "cosine", "coverage", "leverage", "lift", "oddsRatio"), nltcs)




#itemFrequencyPlot(nltcs,support=0.0)
#vignette("arules")

#vignette("arulesViz")
rules <- apriori(nltcsmat, parameter=list(support=0.2, confidence=0.99))
head(rules)
inspect(head(sort(rules, by ="lift"),3))




