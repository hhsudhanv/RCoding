#Naive Bayes example

install.packages("e1071")
library(e1071)

set.seed(5)


data("Titanic")
#Save into a data frame and view it
Titanic_df=as.data.frame(Titanic)
Titanic_df

dim(Titanic_df)

sum(Titanic_df$Freq)
#Creating data from table with frequencies: 
#This will repeat each combination equal to the frequency of each combination
repeating_sequence=rep.int(1:dim(Titanic_df)[1], Titanic_df$Freq) #This will repeat each combination equal to the frequency of each combination


repeating_sequence

#Create the dataset by row repetition created
Titanic_dataset=Titanic_df[repeating_sequence,]
Titanic_dataset
tail(Titanic_dataset)
#We no longer need the frequency, drop the feature
Titanic_dataset$Freq=NULL
Titanic_dataset$Class

head(Titanic_dataset)
Titanic_dataset$Class
as.factor(Titanic_dataset$Class)
#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(Survived ~., data=Titanic_dataset)
#What does the model say? Print the model summary
Naive_Bayes_Model

#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,Titanic_dataset,type="raw")
NB_Predictions

#Confusion matrix to check accuracy
confusion.matrix=table(NB_Predictions,Titanic_dataset$Survived)
confusion.matrix

(1364+349)/2201


#error
error=(confusion.matrix[1,2]+confusion.matrix[2,1])/dim(Titanic_dataset)[1]
error
1-error

#OR 
error=mean(NB_Predictions!=Titanic_dataset$Survived)
error


#Now let's do it with training and test data sets
n=dim(Titanic_dataset)[1]
n
n1=floor(n*(0.6))
n1
n2=n-n1
n2

train=sample(1:n,n1)
titanic.train=Titanic_dataset[train,]
titanic.test=Titanic_dataset[-train,]

table(titanic.train)

#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(Survived ~., data=titanic.train)
#What does the model say? Print the model summary
Naive_Bayes_Model

#Prediction on the train dataset
NB_Predictions=predict(Naive_Bayes_Model,newdata=titanic.train,type="class")
table(NB_Predictions)
NB_Predictions_new=predict(Naive_Bayes_Model,newdata=titanic.train,type="raw") #type="raw" produces probabilities
head(NB_Predictions_new)

table(NB_Predictions_new)

#Confusion matrix to check accuracy
confusion.matrix=table(NB_Predictions,titanic.train$Survived)
confusion.matrix
train.error=mean(NB_Predictions!=titanic.train$Survived)
train.error


#Prediction on the test dataset
NB_Predictions=predict(Naive_Bayes_Model,newdata=titanic.test,type="class")
#Confusion matrix to check accuracy
confusion.matrix=table(NB_Predictions,titanic.test$Survived)
confusion.matrix
test.error=mean(NB_Predictions!=titanic.test$Survived)
test.error


Accuracy=1-test.error
Accuracy

new_test <- cbind(Class = "2nd", Sex = "Female", Age = "Adult")
new_test

new_test1 <- cbind(Class = "2nd", Sex = "Male", Age = "Child")
new_test1

predict(Naive_Bayes_Model,new_test,type = "raw")
predict(Naive_Bayes_Model,new_test1,type = "raw")
subset = Titanic_dataset[Titanic_dataset$Sex == "Male",]
subset
subset1 = subset[subset$Age == "Child",]
subset1
dim(subset)
predict(Naive_Bayes_Model,subset1, type = "raw")
#what about the conditional independence assumptions?
################################
subset.survived=Titanic_dataset[Titanic_dataset$Survived=="Yes",]
subset.survived

#test for independence. H0:independent, H1:not independent (they are associated)
chisq.test(subset.survived$Class,subset.survived$Sex)
chisq.test(subset.survived$Class,subset.survived$Age)
+chisq.test(subset.survived$Sex,subset.survived$Age)
a<-table(subset.survived$Class,subset.survived$Sex)
mosaicplot(a)  
#In a mosaic plot if we see same areas accross the other predictor levels, that shows that predicots are independent
#however, we see different areas, for different levels of predictors- >which shows association btw Class&sex


subset.Notsurvived=Titanic_dataset[Titanic_dataset$Survived=="No",]

#test for independence. H0:independent, H1:not independent (they are associated)
chisq.test(subset.Notsurvived$Class,subset.Notsurvived$Sex)
chisq.test(subset.Notsurvived$Class,subset.Notsurvived$Age)
chisq.test(subset.Notsurvived$Sex,subset.Notsurvived$Age)


b<-table(subset.Notsurvived$Class,subset.Notsurvived$Sex)
mosaicplot(b)  
#In a mosaic plot if we see same areas accross the other predictor levels, that shows that predicots are independent
#however, we see different areas, for different levels of predictors- >which shows association btw Class&sex





######################################
#In-class exercise
#####################################
#Use IRis data set to predict Species with naiveBayes() function
# Use Iris data set
iris<-data.frame(iris)
head(iris)
iris$Species<-factor(iris$Species)
is.factor(iris$Species)

set.seed(10)

#Q1)randomly sample 120 cases out of 150 without replacement for tranning set, remaining will be test
#divide iris data accordingly to train and test sets

train = sample(150,120)

iris.train = iris[train,]
iris.test = iris[-train,]


#create separate class vectors for response for training  and test 




#Q2) Use naiveBayes() to build a model for classifying Species



#Q3) Predict specied for test set



#Q4) create confusion matrix and calculate test missclassification error and accuracy

#Q5) Check the independence assumption for predictors that have the same class label
#Does conditional independence assumption hold?

