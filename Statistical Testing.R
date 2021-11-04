###Tests for normal distribution 

#let's get the dataset. We are getting a normal vector  
set.seed(0194) 
ourdata = rnorm(1000, mean=600, sd=80) 

hist(ourdata) 

qqnorm(ourdata) 

#Let's try a uniform data vector 

set.seed(0194) 

ouruniformdata = runif(1000) 
hist(ouruniformdata) 

qqnorm(ouruniformdata) 

# H0 of the two tests = normality of the dataset 
# Shapiro Wilk Test 

? shapiro.test 

shapiro.test(ourdata) 

shapiro.test(ouruniformdata) # dataset max 5K observations 

# you can also extract a column from a dataset 

shapiro.test(iris$Sepal.Length) 


# Anderson Darling Test 
install.packages("nortest")
library(nortest) # get the nortest library 
?ad.test 
ad.test(ourdata) 

ad.test(ouruniformdata) 
###Exercises: Test for normality 

#- Dataset = diamonds, library = ggplot2 
#- Get familiar with the diamonds dataset. What does the column depth tell us? 
#- Perform at least 2 graphical tests for normality 
#- Get familiar with the package nortest and perform at least 3 different tests for normality.
# - hint: for Shapiro Wilk test, use sample() to sample a subset of data for testing 

####1 sample T test (for population means) 
#- normally distributed data, 1 variable 

? t.test 
hist(ourdata) 

#- we need to specify, x, mu, alternative and if needed the confidence level 
#- here we are stating H0: mean is 300 or higher (mu1>=mu0)
t.test(x=ourdata, mu=300, alternative="less", conf.level=0.95) 

#- H0: mean is 300 or smaller (mu1<=mu0)
t.test(x=ourdata, mu=300, alternative="greater", conf.level=0.95) 

#- 2 sided test - default 
#- here we are stating H0: mean is equal to 300 (mu1=mu0)
t.test(x=ourdata, mu=300, alternative="two.sided", conf.level=0.95) 

##2 sample independent t test (Welch Test) 

#- parametric, 2 sample or population means can be compared  
# compare numeric outcome variable Y vs categorical explanatory variable X (2 levels - e.g. yes vs no) 

head(mtcars) 
#- am is our 2 level categorical variable (although it is factorized) 

attach(mtcars) 

#- visual orientation 
boxplot(data=mtcars, wt~am) 

#- H0: mean wt am1 = mean wt am0 - two sided - independent : paired=F 

t.test(mtcars$wt~mtcars$am, alt="two.sided", conf=0.95,  
       mu=0, paired=F, var.equal=F) 
#- most of this arguments are not mandatory 
t.test(mtcars$wt~mtcars$am) 

#- an alternative way to write it without the tilde 
t.test(mtcars$wt[mtcars$am==0], mtcars$wt[mtcars$am==1]) 

#- how to find out if to assume equal variance: 
#- you can check the boxplot, do the levene Test or check that variance 
var(mtcars$wt[mtcars$am==0]); var(mtcars$wt[mtcars$am==1]) 
#- in this case the var is not equal 

#- Paired T Test for means of 2 dependent or paired populations (same length) 
#- the same command can be used but: paired=T 
# Weight of the mice before treatment
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# Weight of the mice after treatment
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
# Create a data frame
Mydata_forPairedTest <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  weight = c(before,  after)
); Mydata_forPairedTest
t.test(Mydata_forPairedTest$weight~Mydata_forPairedTest$group,paired=T) 
###Exercises: T Test 
#-library = ISwR
#- Do the values of the react data set (notice that this is a single vector, not a data frame) 
#look reasonably normally distributed? Does the mean differ signi???cantly from zero according 
#to a t test?
#-In the data set vitcap, use a t test to compare the vital capacity for the two groups. 
#Calculate a 99% con???dence interval for the difference. The result of this comparison may be misleading. Why?
#- Perform graphical checks of the assumptions for a paired t test in the intake data set.
intake
attach(intake)
t.test(pre,post,paired = T)
#- dataset = ships, library = MASS 
#- get familiar with the dataset ships 
#- we are interested in the relationship of period and incidents
#- use an appropriate graphical tool to show that relationship 
#- use a T test to compare the incidents in the 2 periods (code it in 2 different ways) 
#- are there significant differences in period?


###ANOVA 
#- for normally distributed and independent data sets - we can use it to 
#compare the means of different groups 

head(iris) 
attach(iris) 

#- let's check if our grouping variable is a factor 
is.factor(Species) 

#- let's check the levels of it 
levels(Species) 

#- for a first visual impression 
boxplot(Sepal.Length~Species) 

#- let's get the means for all groups 
by(Sepal.Length, Species, mean)   #mean sepal length for each species
?aov
#- the command for ANOVA is aov 
myANOVA <- aov(Sepal.Length~Species, data=iris) 
myANOVA 

summary(myANOVA) 

#- Tukey test for pairwise comparison and same group size 
TukeyHSD(myANOVA) 

#- to plot the 95% CI level 
plot(TukeyHSD(myANOVA)) # as we can see all groups differ (> 0) 

###Levene Test 

#- used to compare the variances of different groups (homoscedasticity) - similar application as aov 
library(car) 
leveneTest(Sepal.Length, Species, data=iris, center="mean") 
#- in this case since there is a significant p value we can assume var is unequal 

###Exericse ANOVA 
?rnorm
set.seed(0194) 
myobject = data.frame(group=rep(c("a","b","c"),10), 
                      numeric=c(rnorm(5,5),6:15, rep(c(1,20,98),5))) 

myobject
#- Create the object myobject as stated above. There are 3 groups in it and every group has 
#- 10 observations in the column numeric 
#- get 3 different visual impressions of this data 
#- perform an ANOVA and do tests for multiple comparison! 





