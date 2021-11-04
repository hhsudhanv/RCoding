#Lecture 2:

# R's basic data types are character, numeric, integer, logical, complex. 
# R's basic data structures include the vector, list, matrix, data frame, and factors

# R provides many functions to examine features of vectors and other objects
#  typeof(), length(), class() and str() 
x<-c(2, 3, 4)
class(x)


##################################
#DATA STRUCTURES:
##################################

#1)vectors :A vector is a collection of elements that are most commonly of mode character, logical, integer or numeric.
#Atomic vectors: vector only holds a data of a single type.
a <- c(1,2,5.3,6,-2,4) # numeric vector
b <- c("one","two","three") # character vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE) #logical vector

#2)matrices
x<-matrix(1:20, nrow=5,ncol=4) # generates 5 x 4 numeric matrix 
x[,4] # 4th column of matrix
x[3,] # 3rd row of matrix 
x[2:4,1:3] # rows 2,3,4 of columns 1,2,3

#3)arrays:Arrays are similar to matrices but can have more than two dimensions. See help(array) for details.

#4)data frame: A data frame is more general than a matrix, 
#different columns can have different modes (numeric, character, factor, etc.)
d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
names(mydata) <- c("ID","Color","Passed") # variable names
mydata

mydata[2:3] # columns 2,3 of data frame
mydata[,2:3] # columns 3,4,5 of data frame
mydata[2:3,] # What does this do?
mydata[c("ID","Passed")] # columns ID and Age from data frame
mydata$ID # variable ID in the data frame

#5)Lists:An ordered collection of objects (components)
#A list allows you to gather a variety of (possibly unrelated) objects under one name.
w <- list(name="Fred", mynumbers=a, mymatrix=x, age=5.3)  # a string, a numeric vector, a matrix, and a scaler
w

w[[2]] # 2nd component of the list named w
w[["mynumbers"]] # component named mynumbers in list named w
w$age   # component named age in the list named w

w2<-list(name2="John",mynumbers2=d,age2=3)
v <- c(w,w2) # example of a list containing two lists 
str(v)   #str() function: Compactly displays the internal structure of an R object

#6)factors
gender <- c(rep("male",20), rep("female", 30)) 
mode(gender)

gender <- factor(gender)  # R now treats gender as a nominal variable 
mode(gender)
is.factor(gender)
summary(gender)
table(gender)

################################################### Working with data.frames

n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 
b = c(TRUE, FALSE, TRUE) 
df = data.frame(n, s, b)       # df is a data frame
df
?airmiles   # airmiles is one of R's built in datasets 
df_airmiles = airmiles

df_airmiles

head(airmiles) # first 6 rows

tail(airmiles) # last 6 rows
summary(df_airmiles)
str(df_airmiles)

summary(mtcars) # mtcars is another dataset provided by R
mtcars

plot(1937:1960,df_airmiles)

plot(mtcars) # simple xy plot function of R Base

hist(airmiles) # histogram

head(mtcars)

sum(mtcars$wt)

attach(mtcars) # attach to R session environment
search()

sum(wt) # now R knows which data.frame to use since it is attached

detach(mtcars) # remove it from environment

sum(wt) # error message since mtcars it not attached any more

mtcars[2,6]

mtcars[c(2,5,8),6]



##################################
### FUNCTIONS in R
##################################

# Brief description: R functions are OBJECTS
# They do calculations for you
# R function structure: function name <- function (argument) {statements for calculations}
# The arguments specify the components to be used in the function

myfirstfn <- function(x) {x+x}
myfirstfn(10)

# stepwise working functions

mysecondfn <- function(t,z) {
  value = z*3
  value = value *t
  print(value)}

t= 5
z= 9
mysecondfn(t,z)

#InClassExercise 2-Question 1
# 1 . Create a function "mythirdfn" and assign the values 1:10 

#


##################################
### LOOPs in R
##################################
## Loops - loops and functions are a crucial part in programming
# FOR loops allow a certain operation to be repeated a fixed number of times
# This is opposed to the While loop where the rep. number is not prefixed
# The syntax looks like this: for (name in vector) {commands to do the operations}

for (i in 1:15) {print (i)}
for (z in 1:15) {print (z)}

# variable name does not matter although you will see i quite often
# Can be used for quite complex calculations

####Conditional
height <- c(1.75, 1.8, 1.65, 1.9)
height[height>1.7] #extract the values that are larger than 1.7
x <- 5
if(x > 0){
  print("Positive number")
}
if(x > 0) print("Non-negative number") else print("Negative number")

# Example finding all primes with the Eratosthenes method up to a limit (the oldest known systematic method)
PrimVec = function(n){
  # to start from 2
  if (n>=2) {
    # to further specify the sequence we want to work with
    s = seq(2,n)
    # p will be the container for our primes,
    # numbers will be moved from s to p step by step if they meet the criteria
    p = c()
    # we start the loop
    for (i in seq(2,n)){
      
      # we use any to check that i (of this loop round) is still in s, multiples of i will be removed
      if(any(s==i)){
        # we store i if it meets our criteria in p together with the previous p
        p = c(p,i)
        # to search for numbers with a remainder at modulus division
        s = c(s[(s%%i) != 0],i)
      }}
    return(p) }
  # to specify the output if n < 2 (optional)
  else{
    stop("Input at least 2")
  }}
PrimVec(100)


