# Type of Control Chart:  Montgomery back cover.
controlChart = "CUSUM" # Cumulative Sum

# Rational subgroups:  Montgomery Figs. 5.10-11
rationalSubgroups = "Random Sample"

# LIBRARIES

# Quality control charts:  https://cran.r-project.org/web/packages/qcc/qcc.pdf
library(qcc) 

# Q-Q normality plot with confidence intervals:  https://cran.r-project.org/web/packages/car/car.pdf
library(car) 

# Normality tests, including Andersen-Darling:  https://cran.r-project.org/web/packages/nortest/nortest.pdf
library(nortest) 

# PHASE I - RETROSPECTIVE ANALYSIS
#
# Use historical data to ensure that we we have a process that
# is performing as we would like -- in control and capable of
# meeting requirements.

# DEFINE

# Alpha significance to use for hypothesis tests.  NOT to be confused with the alpha
# used to calculate control limits.
sigAlpha = 0.05

# Quality Characteristic:  A property that can be assessed to distinguish higher from lower quality.
# See https://en.wikipedia.org/wiki/Quality_(business).
# 
# To identify quality characteristics...
# For product, use 8 dimensions of quality:  https://en.wikipedia.org/wiki/Eight_dimensions_of_quality
# For service, use SERVQUAL:  https://en.wikipedia.org/wiki/SERVQUAL.
# For process, use factors that affect product or service quality.
#
# The quality characteristic also serves as y-axis label.
# Be sure to include the units for clarity.
qualityCharacteristic = "Shellac Viscosity (Pa-s)" # Pa-s = Pascal-seconds
yLab = "CUSUM (Standard Errors)" # Not in Pa-s, unfortunately

# The quality characteristic's target value.  If no target value then use
# the Phase I mean, x-bar.
mu0 = 2929

# The items being sampled.  Used for the x-axis label.
# It's useful to include the sample size for clarity of presentation.
sampleDescription = "Batch (n=1)" # x-axis

# Chart titles
cusumTitle = "Cumulative Sum for Shellac Viscosity (Pa-s)" # CUSUM chart title

# Sample size & time between samples
m = 20 # Phase I samples
n = 1 # sample size; must be consistent with type of control chart
hSample = 1.0 # time between samples (hours)

# Useful control chart parameters from Montgomery Appendix VI and
# Montgomery Tables 9.3 & 9.4.
d2 = 1.128 # Always use n=2 in table; MR shows differences of 2 points
k = 0.5 # Slack in std. errs.  WARNING:  Not the same k as for x-bar and x charts!
h = 4 # Decision interval in std. errs.  WARNING:  Not the same h as for x-bar and x charts!
ARL0 = 168 # Table 9.3

# The mean shift that we want to detect
deltaXBar = 100 # Pa-s (dynamic viscosity)

# The max average time to signal of true out-of-control point (ATS1).
ATS1Star = 24 # hours

# Data table column names.
sample = "Sample"
metric = "Viscosity"

# MEASURE

# Raw data read into data frame.
# If this results in an error, you need to set the working directory.
# In RStudio's Session menu, choose "Set Working Directory".
# The easiest approach is to put the data files and the R files
# in the same directory and choose "To Source File Location".
rawdata = read.csv("M6E22.csv", 
                   header=TRUE, 
                   sep=",")

# If a transformation (e.g., ln, exp or Box-Cox) is needed to satisfy 
# assumptions, below, do it here.
# rawdata[metric] = log(rawdata[metric])
# print(rawdata)

# Phase I & phase II breakpoints.
rawPhaseIEnd = m * n # m groups of size n
rawPhaseIIStart = rawPhaseIEnd + 1
print(rawdata[1:rawPhaseIEnd,])

# DataFrame reformatted frame with each row being a sample group of size n.
# If the data is already in the correct format, set data = rawdata.
data = rawdata

# Phase I & phase II breakpoints and sample size.
# If the data was already in the correct format, above, don't divide by n.
phaseIEnd = rawPhaseIEnd
phaseIIStart = phaseIEnd + 1

# x for each row
x = data[metric]
print(x[1:phaseIEnd,])

# x-bar is the average of the x's.
xBar = mean(x[1:phaseIEnd,])
print(xBar)

# MR difference in successive x's.  Note that this results in m-1 MRs.
MRData = data.frame(start=x[2:nrow(rawdata),], end=x[1:(nrow(rawdata)-1),])
MR = abs(MRData[,"start"] - MRData[,"end"])
print(MR[1:(phaseIEnd-1)])

# MR-bar is the average of the MRs.  Note that there are m-1 MRs.
MRBar = mean(MR[1:(phaseIEnd-1)]) # Montgomery 6.4
print(MRBar)

# Where the MR data's Phase I ends.
MRPhaseIEnd = phaseIEnd-1
MRPhaseIIStart = phaseIIStart-1

# Unbiased estimate of the standard deviation.
sigmaHat = MRBar / d2 # Montgomery Eq. 6.6
print(sigmaHat)

# Mean shift to detect in standard deviations.
kSigmaShift = deltaXBar / sigmaHat
print(kSigmaShift)

# Control chart parameters in standard deviations.
K = k * sigmaHat # slack, in quality characteristic's units
H = h * sigmaHat # decision interval, in quality characteristic's units
print(K)
print(H)

# ANALYZE

# Key assumptions:  
# 1. Independence of data points.
# 2. Constant variance (homoscedasticity)
# 3. Normality of the control chart parameter, x-bar

# Trend checks.  See if there are any non-random patterns,
# such as correlations across samples or autocorrelations.
# If there are (auto)correlations, independence is violated.
# If the variance seems to be increasing or shrinking, there
# may be heteroscedasticity (i.e., non-homoscedasticity).
# The R chart is also a good way to lok at this.
matplot(y=data[1:phaseIEnd, metric], 
        type="l", 
        lty=1, 
        xlab=sample, 
        ylab=metric, 
        main="Trend Check")

# Normality of x.  
#
# The Central Limit Theorem says x should tend towards normal as n -> infiniity.
# Graphically check for normality.  
hist(x[1:phaseIEnd, metric], xlab=metric, ylab="Count", main="x Distribution")
stripchart(x[1:phaseIEnd, metric], 
           method="stack", 
           xlab=metric, 
           main="x-bar Distribution")
boxplot(x[1:phaseIEnd, metric], ylab=metric, main="x-bar Distribution")
qqPlot(x[1:phaseIEnd, metric],
       main="Check for Normality:  Normal Q-Q Plot",
       xlab="Expected from Cumulative Normal Distribution",
       ylab="Actual Distribution from Data",
       envelope=(1-2*sigAlpha)) # the 1-2*sigAlpha confidence interval to use

# Test for NON-normality of x (or x for n=1) using Andersen-Darling Test.
# H0: Normal, H1: Not normal.
# If p-value < sigAlpha then reject H0 and conclude x-bar is non-normal.
ad = ad.test(x[1:phaseIEnd, 1])
print(ad$p.value)
xIsNormal = (ad$p.value >= sigAlpha)
print(xIsNormal)

# If x is non-normal, we may need to try a transformation on x. 
# Common transformations include natural log and Box-Cox.  Note,
# however, that the x-bar chart is fairly robust to non-normality.

# Normality of MR
hist(MR[1:MRPhaseIEnd], xlab=metric, ylab="Count", main="MR Distribution")
stripchart(MR[1:MRPhaseIEnd], method="stack", xlab=metric, main="MR Distribution")
boxplot(MR[1:MRPhaseIEnd], ylab=metric, main="MR Distribution")
qqPlot(MR[1:MRPhaseIEnd],
       main="Check for Normality:  Normal Q-Q Plot",
       xlab="Expected from Cumulative Normal Distribution",
       ylab="Actual Distribution from Data",
       envelope=(1-2*sigAlpha)) # the 1-sigAlpha confidence interval to use

# Test for NON-normality of MR using Andersen-Darling Test.  
# H0: Normal, H1: Not normal.
# If p-value < sigAlpha (e.g., 0.05) then reject H0 and conclude MR is non-normal.
ad = ad.test(MR[1:MRPhaseIEnd])
MRIsNormal = (ad$p.value >= sigAlpha)
print(MRIsNormal)

# Control Charts
#
# Use historical data to ensure that we we have a process that
# is performing as we would like -- in control and capable of
# meeting requirements.

# CUSUM chart.  The CUSUM chart is good for
# detecting small mean shifts.  For n=1, sigma = standardDeviation.
# Uses slack = 0.5 and decision interval = 5 for ARL0 = 370.
# The CUSUM assumes normality of x. [Montgomery 9]
# 
# Note:  When n>1, use x-bar instead of x for each sample group.
# 
# Unfortunately, the qcc CUSUM chart's y-axis is in standard errors,
# not the quality characteristic's units.  
cusumChart = cusum(data[1:phaseIEnd,metric], 
                   center=mu0,
                   std.dev=sigmaHat, 
                   se.shift=kSigmaShift, 
                   decision.interval=h,
                   title=cusumTitle,
                   xlab=sampleDescription,
                   ylab=yLab)

# If the process is out-of-control, estimate the mean shift.


# If the cprocess is out-of-control, it is
# useful to have the x series, the positive CUSUM, and
# the negative CUSUM.

print(cusumChart$violations)
print(x[1:phaseIEnd,])
print(cusumChart$pos)
print(cusumChart$neg)

# Points that are out-of-control are typically due to special cause
# variation and need to be investigated.

# AVERAGE RUN LENGTH (ARL) and AVERAGE TIME TO SIGNAL (ATS) - CUSUM [Montgomery 9]
#
# ARL and ATS assume x is normal.

# Determine the average run length for a false alarm.
# This was already determined from Table 9.3.
print(ARL0)

# Average run length for a mean shift of kSigmaShift.
# Look up the result in Montgomery Table 9.3.
# For sensitivity analysis, we can pick different
# k & h
print(kSigmaShift) # mean shift
print(k) # slack
print(h) # decision interval
ARL1 = 13.3 # from Montgomery table 9.4
print(ARL1) # Number of points needed to detect shift
power = 1/ARL1 
print(power) # probability a true out-of-control occurrence is detected
beta = 1 - power
print(beta) # probability a true out-of-control occurrence is NOT detected

# Determine the average time to signal (ATS) for 
# false alarms (ATS0) and true alarms (ATS1).
ATS0 = hSample * ARL0
print(ATS0)
ATS1 = hSample * ARL1
print(ATS1)

# Did we meet our average time to signal target?
print(ATS1 < ATS1Star)

# IMPROVE

# If ATS1 is not less than ATS1Star, we need to 
# adjust our design parameters.  The easiest to change
# is sampling frequency.
hSample2 = hSample * ATS1Star / ATS1
print(hSample2) # Time between samples to achieve ATS1Star

# If Phase I's had substantial violations of assumptions, was
# out-of-control, or did not meet the ATS1 target, address
# these issues and redo Phase I.  Otherwise proceed to Phase II.

# PHASE II - PROCESS MONITORING (CONTROL)

# Create an Out-of-Control Action Plan (Montgomery Fig. 5.6)
# so we can react quickly when the process goes out of control.

# Monitor the process as we go to ensure it stays in control.
# Each data point should be added and assessed as it occurs 
# (i.e., Don't wait for several data points -- the point is to
# catch special causes ASAP.)
#
# If Phase I was out-of-control, did not satisfy assumptions
# to our satisfaction, or did not meet ATS1Star target, 
# DO NOT proceed to Phase II.

# CUSUM chart.  Phase I parameters are used.
cusumChart2 = cusum(data[1:phaseIEnd, metric], 
                    center=mu0,
                    std.dev=sigmaHat, 
                    se.shift=kSigmaShift, 
                    decision.interval=h, 
                    newdata=data[phaseIIStart:nrow(data), metric],
                    title=cusumTitle,
                    xlab=sampleDescription,
                    ylab=yLab)

# If the chart shows the process is out-of-control, it is
# useful to have the x series, the positive CUSUM, and
# the negative CUSUM.
print(cusumChart2$violations)
print(x)
print(cusumChart2$pos)
print(cusumChart2$neg)

# If process is out-of-control, execute the Out-of-Control Action Plan.  

# If the process is changed, or if the product or service produced
# is changed, the center line and control limits
# are no longer valid so go back to Phase I and start over.
# Otherwise continue Phase II with the next data point.