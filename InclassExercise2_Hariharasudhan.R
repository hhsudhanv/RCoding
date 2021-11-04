## ISE 240
# Hari hara sudhan Venkateswaran


#InClassExercise 2-Question 1
# 1 . Create a function "mythirdfn" and assign the values 1:10 
mythirdfn <- function(x)
{
a <- c(1:x)
a
  }
mythirdfn(10)
#

###In Class Excercise2- Question 2:
#Working with the 'iris' dataset that is in the base package

?iris
iris

#2.1 #1. Check the class of each variable the 'iris' dataset

sapply(iris,class)
#2.2 #2. Get all rows of Species 'versicolor' in a new data frame. # Call this data frame: 'iris.vers'
iris.vers = subset(iris, Species == "versicolor"); iris.vers 
iris.vers
#2.3 #3. Get a vector called 'sepal.dif' with the difference between 
#'Sepal.Length' and 'Sepal.Width' of 'versicolor' plants.

sepal.dif = iris.vers$Sepal.Length-iris.vers$Sepal.Width

#2.4 #4. Update (add) 'iris.vers' with the new column 'sepal.dif'.

cbind(iris.vers,sepal.dif)

#2.5 #5.Extract Sepal.Length from the "iris" dataset and call the resulting vector mysepal

mysepal <- iris$Sepal.Length
mysepal

#2.6 #6.Get the summation, mean, median, max and min of mysepal
 
sum(mysepal)
mean(mysepal)
median(mysepal)
max(mysepal)
min(mysepal)

#2.7 #7. Get the summary of mysepal and compare the results with #6

summary(mysepal)
