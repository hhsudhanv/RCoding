# The Validation Set Approach

library(ISLR)
set.seed(1) #specify so that you may get same estimate values. 


#we will use Auto data which has 392 observations
dim(Auto)
train=sample(392,196)  #sample 196 numbers from 1:392 --> holdout 50% of data , Random Sampling

lm.fit=lm(mpg~horsepower,data=Auto,subset=train) #fit linear reg. only using train set
attach(Auto)
mean()
mean((mpg-predict(lm.fit,Auto))[-train]^2)  #predict response - 
                                            #and calculate mean squared error(MSE) for remaining holdout data [-train]
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train) #fit polynomial (quadratic in this case)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train) #fit 3rd order polynomial
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2) #sample different training set 
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# Leave-One-Out Cross-Validation

glm.fit=glm(mpg~horsepower,data=Auto)   #glm can also fit linear regression if we don't use family="binomial"
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)  #default cross validation is LOOCV if you don't identify K
cv.glm()
cv.err$delta
cv.error=rep(0,5) #create a vector of 0 values with size=5  
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)  #fit different polynomial regression models with i^th order
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]      #store the cv error in cv vector's i^th element
}
cv.error

# k-Fold Cross-Validation

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]   #K=10 argument for 10-fold cross validation
}
cv.error.10

# The Bootstrap

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)

# Estimating the Accuracy of a Linear Regression Model using Bootstrap approach

boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)  #use boot() to estimate standard errors of 10000 bootstrap estimates for intercept and slope
# Bootstrap Statistics :
#   original      bias    std. error
# t1* 39.9358610  0.02972191 0.860007896   --> st error for B0
# t2* -0.1578447 -0.00030823 0.007404467  ---> st error for B1


summary(lm(mpg~horsepower,data=Auto))$coef  #get st. errors from lm function
                                            # they are different than bootstrap st.errors
                                    #which one is better?
                                    #st. errors from linear reg. relies on the assumption that model is good fit
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))  #this model was a better fit to the data
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef 
#when we compare st errors from lm and bootstrap we get similar results


