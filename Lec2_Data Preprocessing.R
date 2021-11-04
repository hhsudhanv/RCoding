###Data extraction, transformation and visualization
#Create a data frame to store student information
Student_Info = data.frame(Name = c("Paul", "Kim", "Nora", "John", "Joe","Lan","Kitty","Mary"),
                  Number = c(1:8),
                  Grade = c("A", "A", "B", "B", "C", "B", "A", "C"),
                  Height = rnorm(n=8,mean=5,sd=0.5)); Student_Info
#To get an overview of a data set
head(Student_Info)
tail(Student_Info)
summary(Student_Info)
Str(Student_Info)    # similar to summary() but it works with lists
#Check the class in a data frames
sapply(Student_Info, class)
#extract Infomation form a data frame
Student_Info[2,3] #[row,col]
Student_Info[2,]
Student_Info[,4]
Student_Info[2,c(3,4)]
Student_Info[Student_Info$Height>=5,]
subset(Student_Info,Height>=5)
#Calculate the sum of the "Height"
sum(Student_Info$Height)
sum(Student_Info$Grade) #sum is not meaningful for factor
sum(Height)# error message
attach(Student_Info) #The data set is attached to the R search path.
sum(Height)#correct
detach(Student_Info) #The data set is detached to the R search path.
sum(Height)# wrong again
#Transfer a variable in a dataset to integer
attach(Student_Info)
newStudent_Info = data.frame(Name, Number, Grade, as.integer(Height))
sapply(newStudent_Info, class)
Student_Info
newStudent_Info
#Treatment of a missing value
Student_Info_MissingValue = data.frame(Name = c("Paul", "Kim", "Nora", "John"),
                           Exam.1 = c(3.3, 4.0,2.3,2.5),
                           Exam.2 = c(3.7, 4.0,2.7,NA)); Student_Info_MissingValue
complete.cases(Student_Info_MissingValue)
d <- Student_Info_MissingValue[complete.cases(Student_Info_MissingValue),]; d
#Get all rows of the students with Grade equal to A in a new data frame. 
#Call this data frame: 'student.A'
Student.A = subset(Student_Info, Grade == "A"); Student.A
#Add a column/row to the exsiting data set
Gender <- c("M", "M", "M", "M", "M", "F", "F", "F"); Gender 
Student_Info = data.frame(Student_Info,Gender);Student_Info#add a new column
Exam_diff = d$Exam.1 - d$Exam.2
Exam_diff
cbind(d,Exam_diff);
newrow <- data.frame(Name='Mat', Number=9, Grade='B',Height=6, Gender='M')
rbind(Student_Info,newrow)



###In Class Excercise2- Question 2:
#Working with the 'iris' dataset that is in the base package
#1. Check the class of each variable the 'iris' dataset
#2. Get all rows of Species 'versicolor' in a new data frame. 
# Call this data frame: 'iris.vers'
#3. Get a vector called 'sepal.dif' with the difference between 
#'Sepal.Length' and 'Sepal.Width' of 'versicolor' plants.
#4. Update (add) 'iris.vers' with the new column 'sepal.dif'.
#5.Extract Sepal.Length from the "iris" dataset and call the resulting vector mysepal
#6.Get the summation, mean, median, max and min of mysepal
#7. Get the summary of mysepal and compare the results with #6
