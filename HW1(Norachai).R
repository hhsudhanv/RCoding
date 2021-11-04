### Homework1 ###         #Norachai Sanguansaringkhan 012432550#


#1. Create a table that shows how many alumni were in each Class.Year. And use a barchart to plot this table
donation_data <- read.csv("/Users/most/Documents/SJSU Spring 2018/ISE 240/HW1/donation_data_Hw1.csv")
donation_data
getOption("max.print")
?fread
Alumni = fread("donation_data_Hw1.csv")
Alumni
Count = table(G=Alumni$Gender, C=Alumni$`Class Year`)
Count
barplot(Count, main = "Number of Alumni in each class year", xlab = "Class year", ylab = "Number of Male and Female", beside = TRUE, legend.text = c("MALE", "FEMALE"), args.legend =list(title="Gender", x="topleft"))

#2. Create a new column that shows the total donation from 2000-2004 for each person and add it to this data set (sum of FYYearGiving columns)
Total_donations = (Alumni$FY00Giving+Alumni$FY01Giving+Alumni$FY02Giving+Alumni$FY03Giving+Alumni$FY04Giving)
Total_donations
Sum_of_FYYearGiving = data.frame(Alumni,Total_donations)
Sum_of_FYYearGiving

#3. Create a histogram for this new column you created for Total donation. What does it show? Use also summary() function for descriptive statistics about total donations
hist(Sum_of_FYYearGiving$Total_donations, ylab = "Frequency", xlab = "Total donations", main = "Histrogram of Total dontions" )
summary(Sum_of_FYYearGiving$Total_donations)

#4. Create another histogram that only considers the data from people who donated more than $0 and less than $2000
Condition_data = subset(Sum_of_FYYearGiving, Sum_of_FYYearGiving$Total_donations>0 & Sum_of_FYYearGiving$Total_donations<=2000)
Condition_data
hist(Condition_data$Total_donations, main = "Histrogram of total dontaions between $0 to $2000", xlab = "Total donations", ylab = "Frequency", col="Yellow" )

#5. Create separate boxplots that show Total donation by gender, by Class.Year, and by marital status. (use boxplot() command and use argument “outline=FALSE”. This will prevent the boxplot to draw outliers) (Label axes appropriately, give title to each plot)
boxplot(Sum_of_FYYearGiving$Total_donations~Sum_of_FYYearGiving$Gender, outline = FALSE, ylab = "Total donation", xlab = "Genders", main = "Total donation by Gender" )
boxplot(Sum_of_FYYearGiving$Total_donations~Sum_of_FYYearGiving$Class.Year, outline = FALSE, ylab = "Total donations", xlab = "Class Year", main = "Total donation by Class Year")
boxplot(Sum_of_FYYearGiving$Total_donations~Sum_of_FYYearGiving$Marital.Status, outline = FALSE, ylab = "Total donations", xlab ="Martial Status", main = "Total donation by Martial Status")
par(mfrow=c(2,2))
par(mfrow=c(1,1))
#6. Calculate total donations by Major. Use tapply() function for this
Total_by_Major = tapply(Sum_of_FYYearGiving$Total_donations, Sum_of_FYYearGiving$Major, sum)
Total_by_Major

#7. Calculate mean donations by Major. Use tapply() function for this
Mean_by_Major = tapply(Sum_of_FYYearGiving$Total_donations, Sum_of_FYYearGiving$Major, mean)
Mean_by_Major

#8. Create a table that shows many alumni were in each major
Count_Alumni_by_Major = table(Sum_of_FYYearGiving$Major)
Count_Alumni_by_Major

#9. Combine the information in Questions 6,7,8 in one data frame and call it mydata
mydata = data.frame(Total_by_Major, Mean_by_Major, Count_Alumni_by_Major)
mydata
mydata = data.frame(Total_by_Major, Mean_by_Major, as.data.frame(Count_Alumni_by_Major)$Freq)
mydata
colnames(mydata) = c("Total_by_Major", "Mean_by_Major", "Count_by_Major")
mydata

#10. Create a barchart using mydata object that you created that plots mean donations by majors that only has more than 20 alumni
condition_major = subset(mydata$Mean_by_Major,mydata$Count_by_Major>20)
condition_major
barplot(condition_major, main = "Mean donations by majors that only has more than 20 alumni", ylab = "Mean Donation", xlab = "Major")









