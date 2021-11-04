
#LINEAR REGRESSION
install.packages("MASS")
#install.packages("ISLR")
library(MASS)
#library(ISLR)
library(ISLR)
# Simple Linear Regression using Boston data set

??boston
plot(Boston)
names(Boston)
lm.fit=lm(medv~lstat)  
lm.fit=lm(medv~lstat,data=Boston)  #fits linear model
attach(Boston)
#medv => response
#lstat => predictor
#Standard form -> lm(y~x), where x is the predictor and y is the response

lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)  #basic information about the model : p-values
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=5), interval="confidence")  #confidence interval
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")  #confidence intervals
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")  #prediction intervals
plot(lstat,medv)  #there is some evidence of nonlinearity- will explore this later
abline(lm.fit) #plot the regression line
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
abline(lm.fit,lwd = 5)
plot(lm.fit)    #diagnostic plots also reveal that there is some nonlinearity


# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
plot(lm.fit)
summary(lm.fit)
lm.fit.all=lm(medv~.,data=Boston)  #shortcut that fits all variables -instead of typing one by one
summary(lm.fit.all)
lm.fit.all

#what if we want to include all variables but one
# for example fit a model with all variables but remove age
lm.fit1=lm(medv~. - age,data=Boston)  
summary(lm.fit1)

      #or alternatively we can use  update() function to remove after we fit a model with all variables
      lm.fit1=update(lm.fit.all,~.-age)
      summary(lm.fit1)

# Interaction Terms
      #syntax lstat:age only adds interaction term
            # lstat*age includes single terms and their interaction
plot(lstat~age)
summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)  #hypothesis test to compare two models
                        #null hypo is that the two models fit equally well
                        #checks whether second model reduced the Residual Sum of Squares compared to the first model
#par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(medv~poly(lstat,5))  #polynomial regression 5th order
summary(lm.fit5)
summary(lm(medv~log(rm),data=Boston)) #log transformation

detach(Boston)

# Qualitative Predictors using Carseats data
install.packages("ISLR")
library(ISLR)

data(Carseats)
names(Carseats)
attach(Carseats)
contrasts(ShelveLoc)  
class(ShelveLoc)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)



# Best subsets and stepwise functions - SELF LEARNING

#Best subsets regression
# function for best subsets is regsubsets()
# install "leaps" package and use library leaps

?regsubsets
??regsubsets

library(leaps)
attach(Boston)
best.subset <- regsubsets(medv~., data=Boston, nvmax=13)
best.subset.summary<-summary(best.subset)
best.subset.summary
best.subset.by.adjr2 <- which.max(best.subset.summary$adjr2)
best.subset.by.adjr2   #returns the model that has the highest Adjusted_R^2

best.subset.by.Cp <- which.min(best.subset.summary$cp)
best.subset.by.Cp   #returns the model that has the lowest Cp

best.subset.by.bic <- which.min(best.subset.summary$bic)  #lowest Bayesian Information Criterion
best.subset.by.bic
#they all returned the same model -model 11 with the variables with *


# stepwise Regression (stepAIC() function in MASS package)
smallest_model<-lm(medv~1,data=Boston)
m2<-stepAIC(lm(medv~.,data=Boston),scope=smallest_model,direction="backward",steps=1000)
summary(m2)
m2$anova

m3<-stepAIC(lm(medv~.,data=Boston),direction="both",steps=1000)
summary(m3)
m3$anova



