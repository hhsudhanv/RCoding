# This is a comment. Put '#' to make comments
x = 4 
y = 6
z = (x+y)/(y-x)

weight = c(60,72,57,90)

weight = c(60,72,57,90,56,100)

height = c(1.75,1.8,1.65,1.9)

bmi <- weight/height

bmi # prints bmi value at the console

x <- c(4,5,6)
c(4,5.2,6) -> y #arrows work both directions

Column = cbind(x,y)
row = rbind(x,y)
Column
row
m <- 1:12

# Arranging the values from 1 to 12 into a matrix. use either nrow or ncol to determine number of rows or columns
# byrow - if True arranges the values by rows and if false, arranges the values by columns 
matrix(1:12,nrow = 3,byrow = T)
matrix(1:12,nrow = 3,byrow = F)
matrix(1:12,ncol = 3,byrow = T)
m <- matrix(1:12,ncol = 3,byrow = F)
m

rownames(m) <- LETTERS(1:4)
colnames(m) <- c("Name", "Address","GOT")

m

ls() #lists all objects/variables created

rm("x") #removes the variable

x <- c(y,4,y) #joins the y values into the x vector
x

sum(x)
sqrt(x)
sum(sqrt(x))

x[8]

install.packages("Stats") #Base package so cannot be redownloaded
install.packages("ggplot2") 
install.packages("data.table")
?print #help function which shows the use of the keyword

rnorm(15)
?rnorm

library("ggplot2")
?ggplot2
plot