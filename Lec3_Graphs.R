### Graphs in R Base

# 3 main data viz systems: 
# ggplot2, lattice and R Base
# install.packages("ggplot2")
# library(ggplot2)
# install.packages("lattice")
# library(lattice)


# simple scatterplot

x=5:7 # 3 data points, integers
y=8:10


# default plot output here is a scatterplot (default plot function under graphics package)
plot(x,y)

# data is a time series, default here is a line plot
plot(lynx)

# title, color, title color, title magnification
plot(lynx, main="Lynx Trappings", col="red",
     col.main="Green", cex.main=2)
plot(lynx, main="Lynx Trappings", col="red",
     col.main="orange", cex.main=1.5)
plot(lynx, main="Lynx Trappings", col="red",
     col.main="orange", cex.main=2.5)


# label names
plot(lynx,main = "Canadian Lynx Trappings", ylab="Lynx Trappings", xlab="Year",col.main = "Red")

# label orientation
# 0 and 2 - labels are parallel to each other
# 1 and 3 -  labels are perpendicular to each other

plot(lynx, ylab="Lynx Trappings", xlab="", las=0)
plot(lynx, ylab="Lynx Trappings", xlab="", las=1)   # las - 0:3
plot(lynx, ylab="Lynx Trappings", xlab="", las=2)
plot(lynx, ylab="Lynx Trappings", xlab="", las=3)

# changing the session parameter, 2*2 plot matrix  : 4plots on one page
par(mfrow=c(2,2), col.axis="red")
#creates a 2 by 2 screen with 4 plots on one page

plot(1:8, las=0, xlab="xlab", ylab="ylab", main="LAS = 0")
plot(1:8, las=1, xlab="xlab", ylab="ylab", main="LAS = 1")
plot(1:8, las=2, xlab="xlab", ylab="ylab", main="LAS = 2")
plot(1:8, las=3, xlab="xlab", ylab="ylab", main="LAS = 3")

# color manipulation

colors()

# point symbol types

#return plot back to original
par(mfrow=c(1,1))

?pch

x=2:4

plot(x, pch="c") # using letters as point symbols

plot(x, pch=13) # symbol nr 13
plot(x, pch=10) # symbol nr 13

plot(x, pch=11,lty = 1) # symbol nr 13

# Line Types

par(mfrow=c(1,1), col.axis="black") # setting parameters back to default

install.packages("ablineclip")
library(plotrix) # add on package for "ablineclip", install if not yet available

plot(1:7, ylab="", main="Line Types lty 0:6", xlab="lty 0:6") # test plot
abline(a=0,b=3,lty=1, col="sienna2", lwd=2) #abline(a=0,b=1 creates a line with slope=1)
abline(v=1, lty=1, col="sienna2", lwd=2) # solid (default)
abline(v=2, lty=2, col="sienna2", lwd=2) # dashed
abline(v=3, lty=3, col="sienna2", lwd=2) # dotted
abline(v=4, lty=4, col="sienna2", lwd=2) # dotdash
abline(v=5, lty=5, col="sienna2", lwd=2) # longdash
abline(v=6, lty=6, col="sienna2", lwd=5) # twodash, thicker for comparison
abline(v=7, lty=0, col="sienna2", lwd=2) # blank

abline(h=4, lty=4, col="sienna2", lwd=2) # dotdash

# plot types of R Base plot

? plot

# by using "type" we can specify which kind of plot we want

plot(lynx) # plot for time series data

plot(lynx, type="p", main="Type p") # points (default)
plot(lynx, type="l", main="Type l") # lines (default for time series)
plot(lynx, type="b", main="Type b") # points connected by lines
plot(lynx, type="b", main="Type c") # lines only of b
plot(lynx, type="o", main="Type o") # points overlaid by lines
plot(lynx, type="h", main="Type h") # high density
plot(lynx, type="s", main="Type s") # steps
plot(lynx, type="n", main="Type n") # no plot

# Example: advanced line plot with R Base

par(mar=c(4,4,4,4), col.axis="darkgreen") # change of plot margins
cars$speed

plot(cars$speed,main = "", type="s", col="red", bty="n", xlab="Cars ID", ylab="Speed")

#let's go back and change margins by playing the par(mar=c(,,,)) function above and plotting again

#mar(,,,) Sets the bottom, left, top and right margins respectively

text(8, 14, "Speed in mph", cex=0.85, col="red") # adding the explanatory text to plot 1 at specified x,y location



par(new=T) # allows 2 in 1 plot after plotting the first one, it plots a second one on top

plot(cars$dist, type="s", bty="n", ann=F, axes=F, col="darkblue")
axis(side=4, col = "darkblue") # y axis for plot 2
text(37, 18, "Stopping distance in ft", cex=0.85, col="darkblue") # explanations to plot 2
title(main="Speed and Stopping\n Distances of Cars") # main title

legend("topleft",legend=c("speed","distance"),col=c("red","blue"),lty=1:1, cex=0.8)

#??? graphical parameters

?par

par()





### Graphs Exercise

data()   #shows all builtin R data sets
install.packages("datasets-package")

# 1. get familiar with "rivers" - how many observations?
# 2. plot rivers against its index (hint: number of observation on x)
# 3. add: header (red), label names
# 4. change the point symbol and point color

## Solution

?rivers # 141 observations

x = 1:141

y = rivers

plot(x,y, col = "green", pch = 1,
     main = "Lengths of\nMajor N. American Rivers",
     col.main ="red", xlab = "",
     ylab = "length in miles")



#  MORE PLOT TYPES
barplot(rivers)
hist(rivers)


boxplot(iris[,1:3])





install.packages("lattice")
install.packages("nutshell")
library(lattice)
library(nutshell)

data(births2006.smpl)
births2006.smpl[1:5,]
dim(births2006.smpl)


births.dow=table(births2006.smpl$DOB_WK)  #table number of births at each day of wk
births.dow 
barchart(births.dow,ylab="Day of Week",col="black")   #barchart() belongs lattice package

dob.dm.tbl=table(WK=births2006.smpl$DOB_WK,MM=births2006.smpl$DMETH_REC)
dob.dm.tbl
trellis.device()  #another graphics device
dev.cur()  #which graphic device is the current one
dev.list()

barchart(dob.dm.tbl,ylab="Day of Week")

dev.off()

#dotplot(births2006.smpl$DOB_WK,ylab="Day of Week")
hist(births2006.smpl$DOB_WK,ylab="Day of Week")
densityplot(births2006.smpl$DOB_WK,data=births2006.smpl,layout=c(1,1),plot.points=FALSE,col="black")


DOW_SEX<-table(births2006.smpl$DOB_WK,births2006.smpl$SEX)
barchart(DOW_SEX)



births2006.smpl$SEX<-factor(births2006.smpl$SEX)
births2006.smpl$DPLURAL<-factor(births2006.smpl$DPLURAL)
boxplot(births2006.smpl$SEX)
is.factor(births2006.smpl$SEX)
bwplot(births2006.smpl$DBWT~births2006.smpl$DPLURAL) #birth weight by #of babies at birth
bwplot(births2006.smpl$DBWT~births2006.smpl$SEX) #birth weight by gender

densityplot(~DBWT|DPLURAL,data=births2006.smpl,layout=c(1,5),plot.points=FALSE,col="black")
densityplot(~DBWT,groups=DPLURAL,data=births2006.smpl,plot.points=FALSE)

