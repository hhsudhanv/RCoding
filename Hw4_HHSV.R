#Hari hara sudhan Venkateswaran
#Homework 4

# Use set.seed(1)
# Consider the OranjeJuice.csv data file posted in CANVAS. 
# The data contains 1070 purchases where the customer either purchased Citrus Hill or Minute Maid Orange Juice. 
# A number of characteristics of the customer and product are recorded.

rm(list=ls())
library(tree)
set.seed(1)

OranjeJuice = read.csv("OranjeJuice.csv")

head(OranjeJuice)

# 1.	Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations.

train = sample(nrow(OranjeJuice),800)
OranjeJuice.train <- OranjeJuice[train,]
nrow(OranjeJuice.train)
OranjeJuice.test <- OranjeJuice[-train,]
head(OranjeJuice.test)

  # 2.	Fit a tree to the training data, with "Purchase" as the response and the other variables as predictors. 
#Use the "summary()" function to produce summary statistics about the tree, and describe the results obtained. 
#What is the training error rate ? How many terminal nodes does the tree have ?

OranjeJuice.tree.train = tree(Purchase~.,OranjeJuice.train)
summary(OranjeJuice.tree.train)

# 3.	Create a plot of the tree, and interpret the results.

plot(OranjeJuice.tree.train)
text(OranjeJuice.tree.train,cex = 0.8,pretty = 0)

# 4.	Predict the response on the test data,
#and produce a confusion matrix comparing the test labels to the predicted test labels. What is the test error rate ?

OranjeJuice.predict <- predict(OranjeJuice.tree.train,newdata = OranjeJuice[-train,],type = "class")
dim(OranjeJuice.predict)
OranjeJuice.predict
table(OranjeJuice.predict,OranjeJuice.test$Purchase)

(12+49)/270

# 5.	Apply the "cv.tree()" function to the training set in order to determine the optimal size tree.

cv.OranjeJuice <- cv.tree(OranjeJuice.tree.train, FUN = prune.misclass)
cv.OranjeJuice

# 6.	Produce a plot with tree size on the x-axis and cross-validated classification error rate on the y-axis

plot(cv.OranjeJuice$size,cv.OranjeJuice$dev,type="b",xlab = "Tree Size")

# 7.	Which tree size corresponds to the lowest cross-validated classification error rate ?

# Tree size with lowest error rate - 5

# 8.	Produce a pruned tree corresponding to the optimal tree size obtained using cross-validation.
#If cross-validation does not lead to selection of a pruned tree, then create a pruned tree with five terminal nodes.

prune.OranjeJuice=prune.misclass(OranjeJuice.tree.train,best=2)
prune.OranjeJuice.1=prune.misclass(OranjeJuice.tree.train,best=5) #Testing error rate for second best tree size based on results from cv.trees

summary(prune.OranjeJuice)
summary(prune.OranjeJuice.1)

plot(prune.OranjeJuice)
text(prune.OranjeJuice,pretty = 0, cex = 0.8)

plot(prune.OranjeJuice.1)
text(prune.OranjeJuice.1,pretty = 0, cex = 0.8)


# 9.	Compare the training error rates between the pruned and unpruned trees. Which is higher ?
#Pruned

summary(prune.OranjeJuice)
(146/800)

#Unpruned
summary(OranjeJuice.tree.train)
(132/800)

#the pruned tree has a higher error rate indicating tree size might needed to be larger

# 10.	Compare the test error rates between the pruned and unpruned trees. Which is higher ?

#Pruned
prune.pred <- predict(prune.OranjeJuice,newdata = OranjeJuice.test,type = "class")
table(prune.pred,OranjeJuice.test$Purchase)
(30+40)/270

#Unpruned
OranjeJuice.predict
table(OranjeJuice.predict,OranjeJuice.test$Purchase)
(12+49)/270


#Test error is higher for pruned tree indicating that the tree size might need to be larger.
#Further tests with tree size as 5 shows similar error rate to unpruned tree. It is preferable as a result to consider 5 as the optimal tree size.
