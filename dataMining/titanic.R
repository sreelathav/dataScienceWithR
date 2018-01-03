#Setting Environment
library(ggplot2)
library(lattice)
library(rpart)
library(caret)
library(pROC)

#rm(list = ls(all.names = TRUE))
# Save the commands used during the session
#savehistory(file="mylog.Rhistory")
# Load the commands used in a previous session
#loadhistory(file="mylog.Rhistory")
# Display the last 25 commands
#history()
#setwd("~/DataMiningR/codeR")

# Reading data
titanic<-read.table("http://math.ucdenver.edu/RTutorial/titanic.txt",sep="\t"
                    ,header=TRUE)
#library(data.table)
#DT <- data.table(titanic)
#setkey(DT, Age)
#DT[,Age := ifelse(is.na(Age), median(Age, na.rm=TRUE),Age)]
#
#titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm=TRUE)

#glmexp <- glm(Survived~Age+PClass+Sex,data=titanic,family=binomial)
#
#Descriptive Analysis of the Titanic data
#
#attach(titanic)
head(titanic)
dim(titanic)
N<-nrow(titanic)
N
summary(titanic)
str(titanic)
#titanic[!complete.cases(titanic),]
n = sum(is.na(titanic$Age))
perNas = round(100*n/N,2)
#Exploratory Analysis
childrenSurvived <- titanic[which(titanic$Age <16 & titanic$Survived==1),]
femaleSurvived <- titanic[which(titanic$Sex=='female' & titanic$Survived==1),]
maleSurvived <- titanic[which(titanic$Sex=='male' & titanic$Survived==1),]
firstSurvived <- titanic[which(titanic$PClass=='1st' & titanic$Survived==1),] 
secondSurvived <- titanic[which(titanic$PClass=='2nd' & titanic$Survived==1),]
thirdSurvived <- titanic[which(titanic$PClass=='3rd' & titanic$Survived==1),]
#
perChildrenSurvived <- round(100*47/73,2)
perFemaleSurvived <- round(100*308/462,2)
permaleSurvived <- round(100*142/851,2)
perfirstSurvived <- round(100*193/322,2)
perSecondSurvived <- round(100*119/280,2)
perThirdSurvived <- round(100*138/711,2)

#with(titanic, plot(Survived, Age))
#qplot(log(carat), log(price), data = diamonds)
#ggplot(titanic, aes(x = Age)) + geom_bar(aes(fill = Sex))
ggplot(titanic, aes(x = Age)) + geom_bar(aes(fill = as.factor(Survived)))
#ggplot(titanic, aes(x = Age)) + geom_bar(aes(fill = PClass))
ggplot(titanic, aes(x = as.factor(Survived)))+ geom_bar(aes(fill = PClass))
ggplot(titanic, aes(x = as.factor(Sex))) + geom_bar(aes(fill = as.factor(Survived)))
age=titanic$Age
hist(age, breaks = 10, freq = F)
#xyplot(titanic$Survived~age)
# calculate mean, variance and standard deviation of "age" by excluding missing values

mean.age = mean(age, na.rm = T)
var.age = var(age, na.rm = T)
sd.age = sd(age, na.rm = T)
max.age = max(age, na.rm = T)
#
#Age.histogram = hist(age, breaks = 10, freq = F)
#age.ylim.normal = range(0, max.age, dnorm(age, mean = mean.age, sd = sd.age), na.rm = T)
png('histoWithGammaDensityAge.png')

hist(age, breaks = 10, freq = F,  xlim = c(0, 80), ylim = c(0, 0.035), xlab = 'age (ppb)', ylab = 'Relative frequency', main = 'Histogram of Age in Titanic Data with Gamma Density Curve')

curve(dgamma(x, shape = mean.age^2/var.age, scale = var.age/mean.age), add = T)
dev.off()
#lines(density(age, na.rm = T, from = 0, to = max.age))
#curve(dnorm(x, mean = mean.age, sd = sd.age), add = T)
#Data Manipulation
#creating Title covariant from name 
titanic$Title <- ifelse(grepl('Mr ',titanic$Name),'Mr',ifelse(grepl('Mrs ',titanic$Name),'Mrs',ifelse(grepl('Miss',titanic$Name),'Miss','NoTitle')))
cleanTitanic <- na.omit(titanic)
head(cleanTitanic)
manipulatedTitanic <- titanic[is.na(titanic$Age),]

#predicting age to substitute for the missing values.
#avoided replacing missing values with the mean age value
#
predAge <- lm(Age ~ PClass + Sex + Title + Survived,
                       data=titanic[!is.na(titanic$Age),])
#titanic$Age[is.na(titanic$Age)] <- predict(predAge, titanic[is.na(titanic$Age),])
par(mfrow=c(1,2))
hist(titanic$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04),xlab="")
hist(predict(predAge, titanic[is.na(titanic$Age),]), freq=F, main='Age: predicted age', 
     col='lightgreen', ylim=c(0,0.04),xlab="",ylab="")
par(mfrow=c(1,1))
manipulatedTitanic$Age <- round(predict(predAge, titanic[is.na(titanic$Age),]),2)
head(manipulatedTitanic)
#
#Define data sets Train and test
#selecting the testset from clean data
set.seed(54321)
dim(cleanTitanic)  

#Sample Indexes
indexes = sample(1:nrow(cleanTitanic), size=0.66*nrow(cleanTitanic))

# Split data
remainData = cleanTitanic[indexes,]
dim(remainData)  
testData = cleanTitanic[-indexes,]
dim(testData)
#
testData$Survived <- 0
trainData <- rbind(reminData,manipulatedTitanic)
dim(trainData)
#then add rest of the data with manipulated data to get training set
# get names of all caret supported models 
names(getModelInfo())
getModelInfo()$gbm$type
getModelInfo()$glmnet$type
# create caret trainControl object to control the number of cross-validations performed
gbmControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)


# run model
cfit <- rpart( as.factor(trainData$Survived)~Title+Age+Sex+PClass, data = trainData)
glmfit <- glm(trainData$Survived~Title+Age+PClass, data = trainData)
glmfit2 <- glm( trainData$Survived~Title+Age+Sex+PClass, data = trainData)
# find out variable importance
summary(cfit)
varImp(cfit)
#predict survival on the test set

testData$Survived <- predict(cfit, testData,type = "class")
#glmtest<- predict(glmfit, testData, type="terms")
#glmtest2<- predict(glmfit2, testData,type="terms")
anova <- anova(glmfit2, glmfit, test ="Chisq")
library(lmtest)
lrtest(glmfit2, glmfit)


#detach(titanic)