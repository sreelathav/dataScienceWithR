#– X%*%Y calculates XY
#– t(X) calculates X T

A = matrix(c(1, 2, 4, 2, 1, 3, 4, 3, 0),
           nrow=3,
           ncol=3,
           byrow = TRUE)
B = matrix(c(-2, 3, 0, 3, 3, -1, 0, 1, 5),
           nrow=3,
           ncol=3,
           byrow = TRUE)
#  (AB) -1 = B -1 A -1
#1. A −1
Ainv = solve(A)
#2. B -1
Binv = solve(B)
#3. (AB) −1
ABinv = solve(A%*%B)
#4. (BA) −1
BAinv = solve(B%*%A)
Ainv
###############
A = matrix(c(2, 3, 2, -4),
           nrow=2,
           ncol=2,
           byrow = TRUE)
B = matrix(c(-2, 1),
           nrow=2,
           ncol=1,
           byrow = TRUE)
X = solve(A)%*%B
X
#4y 1 + 3y 2 + 2y 3 = 10
#6y 1 + 5y 2 + 10y 3 = 0
#10y 1 + y 2 + 6y 3 = −5
A = matrix(c(4, 3, 2, 6, 5, 10, 10 ,1, 6),
           nrow=3,
           ncol=3,
           byrow = TRUE)
B = matrix(c(10, 0, -5),
           nrow=3,
           ncol=1,
           byrow = TRUE)
X = solve(A)%*%B

###########multiple regression
# fit a multiple regression model to these data.
# E(Y |X 1 , X 2 ) = Xβ + ε
# • Use R to find:
#   1. The least squares estimate, β̂, of β.
# 2. The hat matrix H = X(X T X) −1 X T .
# 2. The hat matrix H = X(X T X) −1 X T .
# 3. The fitted values Ŷ = HY .
# 4. The residuals ε̂.
vendors <- read.csv('~/data/wages.csv')
vendors
reg <- lm(wage ~ edu+exp, data= vendors)
summary(reg)

beta = coefficients(reg) #least squares estimates
beta
hat= hatvalues(reg) #hat values
hat

yhat = fitted(reg) # predicted values
yhat

epsilon = residuals(reg) # residuals
epsilon
###########
bodyfat <- read.csv('~/data/BodyFat.csv')
head(bodyfat)
fit <- lm(Body_fat ~ ., data= bodyfat)
summary(fit)

beta = coefficients(fit) #least squares estimates
beta
hat= hatvalues(fit) #hat values
hat

yhat = fitted(fit) # predicted values
yhat

epsilon = residuals(reg) # residuals
epsilon
#correlation
rentData<- read.csv('~/data/Rent.csv')
rentData
# explore data
pairs(rentData)
cor(rentData)

#model fitting
fitall <- lm(RentalRate~ Age + Expenses + VacancyRate
             + Size ,data=rentData)
summary(fitall)
#step wise fitting var one by one
fitnull <- lm(RentalRate~ 1 ,data=rentData)
fitfull <- lm(RentalRate~ Age + Expenses + VacancyRate + Size ,data=rentData)
step(fitnull, scope=list(lower=fitnull, upper=fitfull), direction="forward")

#anova
anova(fitfull)
coefficients(fitfull)
confint(fitfull, level=0.95)
fitted(fitfull)
#rent data - study for interaction terms
head(RentData)
pairs(RentData)
RentData$cen_Age = RentData$Age - mean(RentData$Age)
RentData$cen_Expenses = RentData$Expenses - mean(RentData$Expenses)
fit <- lm(RentalRate~ Size + cen_Age + cen_Expenses +
            cen_Age*cen_Expenses,data=RentData)
summary(fit)
#without interaction fit
fit2 <- lm(RentalRate~ Size + cen_Age + cen_Expenses, data=RentData)
summary(fit2)
anova(fit, fit2)
#
#########
fit <- lm(RentalRate~ Size + Age + Expenses,data=rentData)
summary(fit)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)
#
plot(rentData$RentalRate)
lines(fitted(fit))
points(38, rentData$RentalRate[38], col = "red")
points(65, rentData$RentalRate[65], col = "red")
points(80, rentData$RentalRate[80], col = "red")

#############
#categorical predictors
TeenGambData<- read.csv('~/data/teengamb.csv')
pairs(TeenGambData)

Gamfit = lm(gamble ~ sex + status + income +verbal
            + sex*status + sex*verbal + sex*income,data=TeenGambData )
summary(Gamfit)

#step wise analysis of betterfit
fitnull= lm(gamble ~ sex,data=TeenGambData )
fitfull = lm(gamble ~ sex + status + income +verbal 
             + sex*status + + sex*verbal + sex*income,data=TeenGambData )
step(fitfull, scope=list(lower=fitnull, upper=fitfull), direction="backward")

#best fit and plot
fit3 = lm(formula = gamble ~ sex + income + verbal + sex:income, data = TeenGambData)
summary(fit3)
plot(TeenGambData$gamble)
lines(fitted(fit3))
#############

#############
steroidsData<- read.csv('~/data/Steroids.csv')
head(steroidsData)
pairs(steroidsData)
#create a new variable which is a centred version of Age using
steroidsData$cen_Age = steroidsData$Age - mean(steroidsData$Age)
#fit data
reg = lm(Steroid ~ cen_Age + I(cen_Age^2) + I(cen_Age^3),data=steroidsData)
summary(reg)
#fit with quadratic
reg1 = lm(Steroid ~ cen_Age + I(cen_Age^2), data=steroidsData)
summary(reg1)
anova(reg, reg1)
#####
crimeData<- read.csv('~/data/crime.csv')
head(crimeData,51)
pairs(crimeData)
fitC <- lm(VR~ M + P + S ,data=crimeData)
summary(fitC)
anova(fitC)
#
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fitC)
hist(crimeData$S)
#
crimeData$State[9]

crimeData$State[51]

crimeData$State[25]
###########
crime <- read.csv('~/data/crime.csv')

#
leaps<-regsubsets( VR ~ MR+M+W+H+P+S, nbest=10, data=crime)
# view results
summary(leaps)
sw <- lm(VR ~ S + W, data = crime)
summary(sw)
# plot a table of models showing variables)
#forward step
fitnull <- lm(VR~ 1 ,data=crime)
fitfull <- lm(VR~ MR+M+W+H+P+S ,data=crime)
step(fitnull, scope=list(lower=fitnull, upper=fitfull),
     direction="forward")
#The variables added or dropped at each stage are detailed 
#in the output. 
#
##################
library(ISLR)
data(Hitters)
names(Hitters)
dim(Hitters)
#322 20
sum(is.na(Hitters$Salary))
#59
Hitters=na.omit(Hitters)
dim(Hitters)
#263 20
sum(is.na(Hitters))
x=model.matrix(Salary~.,Hitters )[,-1]
y=Hitters$Salary
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
#
attributes(ridge.mod)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
round(coef(ridge.mod)[,50],4)
sqrt(sum(coef(ridge.mod)[-1,50]^2))
#6.36
ridge.mod$lambda[60]
round(coef(ridge.mod)[,60],4)

sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20 ,]
#############
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
ridge.mod=glmnet(x[train ,],y[train],alpha =0, lambda =grid ,
                 thresh =1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
y.test=y[test]
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
#114783
lm(y ~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients") [1:20 ,]
###
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha =0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
#212
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test ,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam )[1:20 ,]
#2 Lasso Regression
lasso.mod=glmnet(x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)
##
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test ,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam )[1:20 ,]
lasso.coef
##############
library(lattice)
calcium <- read.csv('~/Documents/AdvPredAnalytics/data/calcium.csv')
head(calcium)
xyplot(y ~ batch, data=calcium)
calcium$batch = as.factor(calcium$batch)
av = aov(y ~ batch, data= calcium)
summary(av)
#
library(lme4)
lmer_cal = lmer(y ~ 1 + (1 | batch), data=calcium)
summary(lmer_cal)
#
library(ICC)
calcium <- read.csv('~/data/calcium.csv')
head(calcium)
ICCest(as.factor(batch), y, CI.type = "S",data=calcium)
calcium$ICC
#[1] 0.4756249
calcium$LowerCI
#[1] -0.1317221
ICCest(as.factor(batch), y, CI.type = "S",alpha=0.01,data=calcium)
0.003973 /(0.004380+0.003973)



#
plasma <- read.csv('~/data/plasma.csv')
head(plasma)
attach(plasma)
ESR_num = as.integer(ESR)
ESR_log = (ESR_num == 2)
fit = glm(ESR_log~fibrinogen + globulin, family="binomial")
summary(fit)
confint(fit)
exp(coef(fit))
exp(confint(fit))
coef(fit)
#
polyps <- read.csv('~/data/polyps.csv')
head(polyps)
attach(polyps)
treat_num = as.integer(treat)
treat_log = (treat_num == 1)
fit1 = glm(number~age + treat_log, family="poisson")
summary(fit1)
confint(fit1)
predict(fit1, type="response") # predicted values
residuals(fit1, type="deviance") # residuals
exp(coeff(fit1))
############
library("readxl")
dataPGA4<- read_excel('~/data/PGAwinnings04.xlsx')
head(dataPGA4)
data04 <- dataPGA4[-c(0:3)]
tail(data04)
round(cor(data04),4)

#full model
fitfull <- lm(TotalWinnings ~ Age + AvgDrive + GreensReg + AvPutts ,data=data04)
summary(fitfull)

#model fitting
fitnull <- lm(TotalWinnings~ 1 ,data=data04)
fitfull <- lm(TotalWinnings ~ Age + AvgDrive + GreensReg + AvPutts ,data=data04)
step(fitnull, scope=list(lower=fitnull, upper=fitfull), direction="forward")

#final model
fitfull <- lm(TotalWinnings ~  AvgDrive + GreensReg + AvPutts ,data=data04)
summary(fitfull)

############
census<- read.csv('~/data/USCensusData.csv')
sum(is.na(census))
nrow(census)
head(census)
pairs(census)
#
analysis <- aov(Life.Exp ~ .-X ,data=census)
analysis
#multiple liner regression model for life expectancy 
#with all the other variables except X(state name)
fitfull <- lm(Life.Exp ~ .-X ,data=census)
#F- value at 1,41  5%  is 4.08
anova(fitfull)       #[,4] >4.08
summary(fitfull)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fitfull)
#step wise fitting var one by one
#backward elimination
fitnull <- lm(Life.Exp ~ 1 ,data=census)
model1 <- step(fitfull, scope=list(lower=fitnull, upper=fitfull), direction="backward")
summary(model1)
anova(model1)
# fit chosen model by stepAIC and check for any insignificant variables
fitF <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost,data=census)
step(fitF, scope=list(lower=fitnull, upper=fitF), direction="backward")
anova(fitF)
summary(fitF)
#
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fitF)
#new data to predict life expectancy
colaradoData = data.frame( X="Colarado", Population=2541, Income=4884, 
                           Illiteracy=0.7, Life.Exp=0.0, Murder=6.8, HS.Grad=63.9, Frost=166,   Area=103766)
#attributes(fitF)
#predict using resultant model litterally
fitF$coefficients
life_exp_colarado <- fitF$coefficients[1]+fitF$coefficients[2]*2541+
  fitF$coefficients[3]*6.8+fitF$coefficients[4]*63.9+fitF$coefficients[5]*166
life_exp_colarado
#1 
#71.00941 
#using predict function
predict(fitF,colaradoData,interval="predict")
#   fit      lwr      upr
#1 71.00941 69.50559 72.51323
#


