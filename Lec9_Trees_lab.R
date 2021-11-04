#LEcture 9-Lab: Decision Trees

# Fitting Regression Trees
install.packages("tree")

rm(list=ls())   #this is a new thing to learn today: how to clean your R environment

library(tree)
library(ISLR)
library(MASS)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
names(Boston)
Boston$medv



#we wil lfit a regression tree for predicting median value of houses in Boston data set
#tree() function to fit a classification or regression tree (no interactions)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
              # Regression tree:
              #   tree(formula = medv ~ ., data = Boston, subset = train)
              # Variables actually used in tree construction:
              #   [1] "lstat" "rm"    "dis"  
              # Number of terminal nodes:  8 
              # Residual mean deviance:  12.65 = 3099 / 245    
              # Distribution of residuals:
              #   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
              # -14.10000  -2.04200  -0.05357   0.00000   1.96000  12.60000 

plot(tree.boston)
text(tree.boston,pretty=0,cex=0.8)

cv.boston=cv.tree(tree.boston)  #cv.tree: cross validation for choosing tree complexity
plot(cv.boston$size,cv.boston$dev,type='b')  #for this particular example, most complex tree is the best

# if we want to prune the tree based on a tree size we found in cross validation:
prune.boston=prune.tree(tree.boston,best=5)  #prune.tree -> cost complexity pruning of tree object
                                            #best: size of the tree you request. 
                                              #for example number of terminal nodes
plot(prune.boston)
text(prune.boston,pretty=0)

# we will use the unpruned tree to make predictions, since it is the best tree for this example
yhat=predict(tree.boston,newdata=Boston[-train,])
yhat
boston.test=Boston[-train,"medv"]
boston.test
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)   #mean squared errors 
#if we want to get the root mean square errors
sqrt(mean((yhat-boston.test)^2))   #This model leads ot predictions that are within $5,000 of the true median home price



# Fitting Classification Trees

library(tree)
library(ISLR)
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")    #converted continuous resonse to binary response
High

Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats) #build classificaion tree
summary(tree.carseats)
          # Classification tree:
          #   tree(formula = High ~ . - Sales, data = Carseats)
          # Variables actually used in tree construction:
          #   [1] "ShelveLoc"   "Price"       "Income"      "CompPrice"   "Population"  "Advertising"
          # [7] "Age"         "US"         
          # Number of terminal nodes:  27 
          # Residual mean deviance:  0.4575 = 170.7 / 373 
          # Misclassification error rate: 0.09 = 36 / 400 

#we see that the training error rate is 9%, tree has 27 terminal nodes


plot(tree.carseats)
text(tree.carseats,pretty=0,cex=0.8) #pretty=0 

tree.carseats  #Print out each split of the tree. The branches with * are leading to terminal node
       #partial output: number of obs in that branch, deviance, overall prediction for the branch
                                #and fraction of observations with yes and no in the response 
              # 2) ShelveLoc: Bad,Medium 315 390.600 No ( 0.68889 0.31111 )  
              # 4) Price < 92.5 46  56.530 Yes ( 0.30435 0.69565 )  
              # 8) Income < 57 10  12.220 No ( 0.70000 0.30000 )  
              # 16) CompPrice < 110.5 5   0.000 No ( 1.00000 0.00000 ) *
  
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]

names(Carseats)

High.test=High[-train]
High[train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")  #size of the tree
plot(cv.carseats$k,cv.carseats$dev,type="b")    # k: cost complexity parameter alpha
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0,cex=0.7)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200

