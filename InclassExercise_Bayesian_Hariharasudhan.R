#Hari hara sudhan Venkateswaran , Nandhini Varadharajan

######################################

#In-class exercise 4- 3/21/2018

#####################################
#Use IRis data set to predict Species with naiveBayes() function
# Use Iris data set
library(e1071)
iris
is.data.frame(iris)
iris<-data.frame(iris)

is.data.frame(iris)

head(iris)
iris$Species<-factor(iris$Species)

is.factor(iris$Species)

set.seed(10)

#Q1)randomly sample 120 cases out of 150 without replacement for training set, remaining will be test
#divide iris data accordingly to train and test sets

train = sample(150,120)

iris.train = iris[train,]
dim(iris.train)

#iris.train$Species <- NULL
iris.test = iris[-train,]
#iris.test$Species <- NULL

#create separate class vectors for response for training  and test 

species.train <- iris.train$Species
species.test <- iris.test$Species

#Q2) Use naiveBayes() to build a model for classifying Species

bayes_Species <- naiveBayes(species.train~.,data = iris.train)
bayes_Species

#Q3) Predict species for test set

Species_Predictions=predict(bayes_Species,newdata=iris.test,type="class")
Species_Predictions

#Q4) create confusion matrix and calculate test missclassification error and accuracy

confusion.matrix <- table(Species_Predictions,species.test)
confusion.matrix

test.error.setosa<- mean(Species_Predictions!=species.test)
test.error.setosa

#Q5) Check the independence assumption for predictors that have the same class label
#Does conditional independence assumption hold?

subset.setosa <- iris[iris$Species == "setosa",]
subset.setosa

#considering a threshold df 0.5 for correlation
cor(subset.setosa$Sepal.Length,subset.setosa$Sepal.Width)  #Not independent
cor(subset.setosa$Sepal.Length,subset.setosa$Petal.Length) #Independent
cor(subset.setosa$Sepal.Length,subset.setosa$Petal.Width)  #Independent
cor(subset.setosa$Sepal.Width,subset.setosa$Petal.Length)  #Independent
cor(subset.setosa$Sepal.Width,subset.setosa$Petal.Width)   #Independent
cor(subset.setosa$Petal.Length,subset.setosa$Petal.Width)  #Independent
