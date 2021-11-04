#KNN using the same SMarket data set we used for logistic regression
library(ISLR)
names(Smarket)
dim(Smarket)

head(Smarket)
attach(Smarket)


library(class)
train=(Year<2005)  #This creates indices for training set. use years <2005 for training, keep 2005 for holdout
train
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]  #creates test set response vector (Direction at year 2005)
Direction.2005

#we will only use variables lag1, lag2 for predictors
train.X=cbind(Lag1,Lag2)[train,] #training set predictors
test.X=cbind(Lag1,Lag2)[!train,]#test set predictors
train.Direction=Direction[train] #training set response (Direction in years before 2005)
set.seed(1)

knn.pred=knn(train.X,test.X,train.Direction,k=1)  #predict direction fr test data Direction.2005 using k=1
knn.pred
table(knn.pred,Direction.2005)
      # Direction.2005
      # knn.pred Down Up
      # Down   43 58
      # Up     68 83

(83+43)/252 #accuracy =50%
(58+68)/252 #misclassification error
knn.pred=knn(train.X,test.X,train.Direction,k=3)  #first argument train set predictors,second:test set predictors,third:train set response
table(knn.pred,Direction.2005)   
    # Direction.2005
    # knn.pred Down Up
    # Down   48 54
    # Up     63 87
mean(knn.pred==Direction.2005)
(48+87)/252  #%accuracy = 53.6 : slight increase with k=3 compared to k=1


Error <- cbind(0.5,0.46,0.44,0.43,0.427,0.56)
k <- cbind(1,3,5,6,7,8)
k
plot(Error,k)

#In Class Exercise:
####################################################
# Use Caravan Data set in ISLR library

# The data contains 5822 real customer records. Each record consists of 86 variables, 
# containing sociodemographic data (variables 1-43) and product ownership (variables 44-86). 
# The sociodemographic data is derived from zip codes. 
# All customers living in areas with the same zip code have the same sociodemographic attributes. 
# Variable 86 (Purchase) indicates whether the customer purchased a caravan insurance policy. 
# Further information on the individual variables can be obtained at http://www.liacs.nl/~putten/library/cc2000/data.html
dim(Caravan)
attach(Caravan)
names(Caravan)
summary(Purchase)


#1)Standardize the data(exclude 86th variable-response)


Caravan.normdata <- scale(Caravan[,-86])
names(Caravan.normdata)

#2) split test set (first 1000 observations), and training set (remaining)

test.caravan <- Caravan.normdata[1:1000,]
train.caravan <- Caravan.normdata[1001:5822,]

#3)create a separate vectors for test and training sets using 86th variable (purchase)

test.purchase <- Caravan$Purchase[1:1000]
test.purchase
train.purchase <- Caravan$Purchase[1001:5822]


#4) set.seed(1) and use knn to predict response(purchase) for test set using k=1

set.seed(1)

knn.pred.purchase <- knn(train.caravan,test.caravan,train.purchase,k=1)
knn.pred.purchase

#5) create the confusion matrix with table(predicted,observed) in the test set

table(knn.pred.purchase,test.purchase)



#6) calculate the fraction of individuals that are correctly predicted to purchase- when k=1

#               test.purchase
# knn.pred.purchase  No Yes
#               No  873  50
#               Yes  68   9

(873+9)/1000

#7) Use k=3 and calculate the fraction of individuals that are correctly predicted to purchase. compare to k=1

knn.pred.purchase <- knn(train.caravan,test.caravan,train.purchase,k=3)
table(knn.pred.purchase,test.purchase)

(920+5)/1000


#8) fit a logistic regression model and predict Purchase=Yes when probability is >0.5
#calculate the fraction of individuals that are correctly predicted to purchase

index <- 1:1000

test.log.Caravan <- Caravan[index,]
train.log.Caravan <- Caravan[-index,]

glm.purchase <- glm(train.log.Caravan$Purchase~.,data = train.log.Caravan, family = "binomial")
summary(glm.purchase)

purchase.pred.prob <- predict(glm.purchase,test.log.Caravan,type = "response")

purchase.pred <-ifelse(purchase.pred > 0.5,"Yes","No")
table(purchase.pred,test.purchase)

#9) Use cutoff >0.25 for predicting purchase using fitted logistic regression model.
#calculate the fraction of individuals that are correctly predicted to purchase

purchase.pred <-ifelse(purchase.pred > 0.25,"Yes","No")
table(purchase.pred,test.purchase)


#10) compare all these cases. whic one is better at correctly predicting purchases

