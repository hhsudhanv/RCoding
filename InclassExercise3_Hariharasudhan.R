#Hari hara sudhan Venkateswaran

### Graphs Exercise

data()   #shows all builtin R data sets
install.packages("datasets-package")

# 1. get familiar with "rivers" - how many observations?
?rivers
length(rivers)
#Number of observations = 141

# 2. plot rivers against its index (hint: number of observation on x)
plot(rivers)

# 3. add: header (red), label names
plot(rivers,main = "Length of Rivers of \n North America",xlab = "River ID",ylab = "River Length")

# 4. change the point symbol and point color
plot(rivers, pch = 8, main="Length of Rivers of \n North America")

install.packages("metRology")
library(metRology)
library("stats")
pt.scaled(c(0.025,0.975), Inf, mean=10, sd=1)

pt.scaled(c(2600,3200), Inf, mean=10, sd=1) #10 +- 1.96*sd

require(graphics)
hist(pt.scaled(10000, df=4, mean=11, sd=0.7), breaks=50, probability=TRUE)
x<-seq(0,25, 0.05)
lines(x,dnorm(x,mean=11, sd=0.7), col=2)
