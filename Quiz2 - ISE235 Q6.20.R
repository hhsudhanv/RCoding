rawdata = read.csv("Q6.20Data.csv", 
                   header=TRUE)

rawdata
n=5
phaseIEnd = 20

A2 = 0.577 # x-bar chart
D3 = 0.0 # R chart's LCL
D4 = 2.114 # R chart's UCL
d2 = 2.326 # to get unbiased estimate of standard deviation

#Phase I
rawdata$Number <- NULL
rawdata
rawdata[1:phaseIEnd,1:n]
xBarChart = qcc(rawdata[1:phaseIEnd,1:n], 
                type="xbar",
                title="Critical Dimension of Part",
                xlab="Parts",
                ylab="Dimension")

xBarBar <- xBarChart$center
xBarBar
RChart = qcc(rawdata[1:phaseIEnd,1:n], 
                type="R",
                title="Critical Dimension of Part",
                xlab="Parts",
                ylab="Dimension")

RChart$center


#Phase II

PhaseIIEnd = 30
xBarChart = qcc(rawdata[1:phaseIEnd,1:n], 
                type="xbar",
                newdata = rawdata[21:PhaseIIEnd,1:n],
                title="Critical Dimension of Part",
                xlab="Parts",
                ylab="Dimension")


RChart = qcc(rawdata[1:phaseIEnd,1:n], 
             type="R",
             newdata = rawdata[21:PhaseIIEnd,1:n],
             title="Critical Dimension of Part",
             xlab="Parts",
             ylab="Dimension")


#Phase III

rawdata1 = read.csv("Q6.20Data-Phase3.csv", 
                   header=TRUE)
rawdata1$Number <- NULL

rawdata1
xBarChart = qcc(rawdata[1:20,1:5],
                newdata = rawdata1[21:30,1:5],
                type="xbar",
                title="Critical Dimension of Part",
                xlab="Parts",
                ylab="Dimension")
rawdata


sBarChart = qcc(rawdata[1:phaseIEnd,1:5], 
                type="S",
                title="Critical Dimension of Part",
                xlab="Parts",
                ylab="Dimension")


sBarChart$center

RChart = qcc(rawdata[1:phaseIEnd,1:n], 
             type="R",
             newdata = rawdata1[21:PhaseIIEnd,1:n],
             title="Critical Dimension of Part",
             xlab="Parts",
             ylab="Dimension")

l = 3
n = 5
k = 5
n = n^(1/2)

p1 = l-(k*n)
p2 = (-l-(k*n))
power = pnorm(p1,mean = 0,sd = 1) - pnorm(p2,mean = 0,sd = 1)

L = 3
deltaXBar = 5
sigmahat = 17.55
n = 5
sem = sigmahat/sqrt(n)
sem
#Power Calculation
# The power (1 - beta) for the given mean shift.  Power is the probability
# that a true out-of-control signal is detected.
power = pnorm(-L-(deltaXBar/sem), 0, 1) + (1 - pnorm(L-(deltaXBar/sem), 0, 1)) # Montgomery Eq. 6.19
print(power)
beta = 1-power
print(beta)

ARL1 = 1/power
ARL1
