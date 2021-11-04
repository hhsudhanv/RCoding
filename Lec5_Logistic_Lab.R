data(efault)
library(ISLR)
library(MASS)
attach(Default)


plot(balance,income, col=ifelse(default=="Yes","orange","blue"))

default_logistic1<-glm(default~balance,family="binomial")  #glm() is the function we use for logistic regression
# family="binomial" should be included for logistic regression  
summary(default_logistic1)

is.factor(student)
default_logistic2<-glm(default~student,family="binomial")
summary(default_logistic2)


default_logistic<-glm(default~.,data=Default, family="binomial")
summary(default_logistic)

contrasts(default)  # to see which value of categorical variable is assumed 0
#because glm() wil lestimate the probability of default=yes
# we need to know the assigned 0-1 values to classify predictions

predicted_default_prob<-predict(default_logistic,data=default, type="response") #predict probabilities

predicted_default<-ifelse(predicted_default_prob>0.5,"Yes","No") # convert probabilities to class labels

table(default,predicted_default)
#missclassification
(40+228)*100/10000
#accuracy
mean(default==predicted_default) #fraction of data that is correctly predicted
1-mean(default==predicted_default) #fraction of missclassified data

# Or you can use more detailed function from library(gmodels)
library(gmodels)
a<-CrossTable(x=default,y=predicted_default,prop.chisq=F)
amatrix=a$prop.tbl
if(dim(amatrix)[2]==2)
{
  TN=amatrix[1,1]   #true 0 prediction percentage
  TP=amatrix[2,2]   #true 1 prediction percentage
  accuracy=(TP+TN)
  accuracy
  print("Logistic Regression % ACCURACY")
  print(accuracy*100 ) 
}

detach()
#In class exercise:
# Use StockMarket data in ISLR library
# 

library(ISLR)
names(Smarket)
?Smarket
dim(Smarket)
summary(Smarket)
pairs(Smarket)

attach(Smarket)

# Q1) Look at the correlations between variables using cor() function
# you wil lget an error because Direction is qualitative, 
# get correlations of all other variables but Direction

cor(Smarket$Year,Smarket$Lag1)
cor(Year,Lag2)
cor(Year,Lag3)
cor(Year,Lag4)
cor(Year,Lag5)
cor(Year,Volume)
cor(Year,Today)
cor(Lag1,Lag2)
cor(Lag1,Lag3)
cor(Lag1,Lag4)
cor(Lag1,Lag5)
cor(Lag1,Volume)
cor(Lag1,Today)
cor(Lag2,Lag3)
cor(Lag2,Lag4)
cor(Lag2,Lag5)
cor(Lag2,Volume)
cor(Lag2,Today)
cor(Lag3,Lag4)
cor(Lag3,Lag5)
cor(Lag3,Volume)
cor(Lag3,Today)
cor(Lag4,Lag5)
cor(Lag4,Volume)
cor(Lag4,Today)
cor(lag5,Volume)
cor(lag5,Today)
cor(Volume,Today)

#Q2 fit a logistic regression model with Lag1-5 and Volume to predict Direction

direction.logistic <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = "binomial")
summary(direction.logistic)
#Q3  Estimate probability of market going up and keep the probabilities in object named glm.probs

glm.probs_prob <- predict(direction.logistic, type="response")

#Q4: check contrasts() of Direction and classify predictions based on threshold=0.5
contrasts(Direction)

glm.probs <- ifelse(glm.probs_prob>0.5,"Up","Down")
glm.probs

#Q5 : Table the observed vs Predicted Directions

table(Direction,glm.probs)

x<- 1:4
y <-2
x+y


x <- list(2, "a", "b", TRUE)
x[[1]]
#Q6: calculate missclassification error and prediction accuracy

(141+457)*100/10000

mean(Direction==glm.probs) #fraction of data that is correctly predicted
1-mean(Direction==glm.probs)#fraction of missclassified data