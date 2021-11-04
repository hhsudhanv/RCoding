## Read CSV file (header assumed), then put that into "csv.data" data object (any name is ok).

mydata2 <- read.csv("D:/ISE 240/R files/StudentList.csv")
mydata2
rm(mydata2)
#what happened? could not find the file

## This gives you a dialogue to choose a file, then the file is passed to read.csv() function
mydata2 <- read.csv(file.choose())

#Or you can either change your working directory to the folder that contains the file and then read
mydata2 <- read.csv("StudentList.csv")

# OR, you can provide the path of the folder than contains file
mydata2 <- read.csv("/Users/aycaerdogan/Dropbox/CLASSES_SJSU/ISE240_ANALYTICSforSysEng/LectureNotes/Lecture2/StudentList.csv",header=T)
mydata2 <- read.table("/Users/aycaerdogan/Dropbox/CLASSES_SJSU/ISE240_ANALYTICSforSysEng/LectureNotes/Lecture2/StudentList.csv", sep=",",header=T) #comma separated

mydata2 <- read.table("/Users/aycaerdogan/Dropbox/CLASSES_SJSU/ISE240_ANALYTICSforSysEng/LectureNotes/Lecture2/StudentList.txt", sep="\t")  #tab separated

install.packages("xlsx", dep = T)      # If you have not installed it before
library(xlsx)

## You need to specifiy the sheetIndex (sheet number)
mydata3 <- read.xlsx("/Users/aycaerdogan/Dropbox/CLASSES_SJSU/ISE240_ANALYTICSforSysEng/LectureNotes/Lecture2/StudentList.xlsx",sheetIndex = 1)




#Excel file (Mac OS X): gdata package is the simplest. Use read.xls() function.

#install.packages("gdata", dep = T)      # If you have not installed it before
#library(gdata)
#excel.data <- read.xls("file.xls")

#Write to a file
n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 
df = data.frame(n, s)
df
write.table(df,"mydata.txt", sep="\t") #sep="\t": tab deliminated output
write.table(df, file="mydata.csv",sep=",",row.names=F) 

#where did it place the file?

df1 <- read.csv("mydata.csv")
df2 <- read.csv("mydata.csv",header = T)

df2
df1
