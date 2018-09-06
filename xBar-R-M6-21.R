# Type of Control Chart:  Montgomery back cover.
controlChart = "x-bar, R"

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
qualityCharacteristic = "Compressive Strength (psi)" # y-axis;  

# The items being sampled.  Used for the x-axis label.
# It's useful to include the sample size for clarity of presentation.
sampleDescription = "Parts (n=5)" # x-axis

# Chart titles
RTitle = "R for Compressive Strength (psi)" # R chart title
xBarTitle = "x-bar for Compressive Strength (psi)" # x-bar chart title

# Sample size & time between samples
m = 20 # Phase I samples
n = 5 # sample size; must be consistent with type of control chart
h = 0.5 # time between samples (hours)

# The number of standard errors of the mean (sem) to calculate control limits.
L = 3 # One false positive per 370 samples on average.  See ARL0, below.

# Useful control chart parameters from Montgomery Appendix VI and
# Montgomery front cover.
# Need to reset based on sample size, n.
A2 = 0.577 # x-bar chart
D3 = 0.0 # R chart's LCL
D4 = 2.114 # R chart's UCL
d2 = 2.326 # to get unbiased estimate of standard deviation

# The mean shift that we want to detect
deltaXBar = 5.0 # psi

# The max average time to signal of true out-of-control point (ATS1).
ATS1Star = 1.0 # hours

# Data table column names.
sample = "Sample"
metric = "Strength"

# MEASURE

# Raw data read into data frame.
# If this results in an error, you need to set the working directory.
# In RStudio's Session menu, choose "Set Working Directory".
# The easiest approach is to put the data files and the R files
# in the same directory and choose "To Source File Location".
rawdata = read.csv("M6E12-13.csv", 
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
xRaw = rawdata[[metric]]

# DataFrame reformatted frame with each row being a sample group of size n.
# If the data is already in the correct format, set data = rawdata.
data = qcc.groups(rawdata[[metric]], 
                  rawdata[[sample]])

# Phase I & phase II breakpoints and sample size.
# If the data was already in the correct format, above, don't divide by n.
phaseIEnd = rawPhaseIEnd / n
phaseIIStart = phaseIEnd + 1
print(data[1:phaseIEnd,1:n])

# x-bar for each row = the average of each row.
xBar = apply(data[,1:n], 1, mean)
print(xBar[1:phaseIEnd])

# x-bar-bar is the average of the x-bars.
xBarBar = mean(xBar[1:phaseIEnd]) # Montgomery Eq. 6.2
print(xBarBar)

# R for each row = max - min for each row
RData = data
R = apply(RData[,1:n], 1, max) - apply(RData[,1:n], 1, min)
print(R)

# R-bar is the average of the Rs.  
RBar = mean(R[1:phaseIEnd]) # Montgomery Eq. 6.3
print(RBar)

# Where the R data's Phase I ends.
RPhaseIEnd = phaseIEnd
RPhaseIIStart = phaseIIStart

# Unbiased estimate of the standard deviation.
sigmaHat = RBar / d2 # Montgomery Eq. 6.6
print(sigmaHat)

# Standard error of the mean.
sem = sigmaHat / sqrt(n) # Montgomery Eq. 6.1
print(sem)

# Mean shift to detect in standard deviations.
# k = Number of standard deviations of mean shift to detect.
k = deltaXBar / sigmaHat 
print(k)

# x-bar chart's control, warning and one-sigma limits.  (Montgomery 6.1, 6.4, 6.6, 6.7)
xBarUCL = xBarBar + L * sem                           # upper control limit
xBarUWL = xBarBar + (2/3) * L * sem                   # upper warning limit
xBarOneSigmaLimit = xBarBar + (1/3) * L * sem         # one-sigma limit
xBarNegativeOneSigmaLimit = xBarBar - (1/3) * L * sem # negative one-sigma limit
xBarLWL = xBarBar - (2/3) * L * sem                   # lower warning limit
xBarLCL = xBarBar - L * sem                           # lower control limit
print(xBarUCL)
print(xBarLCL)

# R chart's control limits.  We usually don't worry about warning and
# one-sigma limits.  (Montgomery 6.5)
RUCL = D4 * RBar
RLCL = D3 * RBar
print(RUCL)
print(RLCL)

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
matplot(y=data[1:phaseIEnd, 1:n], 
        type="l", 
        lty=1, 
        xlab=sample, 
        ylab=metric, 
        main="Trend Check")

# Normality of x-bar.  
#
# The Central Limit Theorem says xBar should tend towards normal as n -> infiniity.
# Graphically check for normality.  
hist(xBar[1:phaseIEnd], xlab=metric, ylab="Count", main="x-bar Distribution")
stripchart(xBar[1:phaseIEnd], 
           method="stack", 
           xlab=metric, 
           main="x-bar Distribution")
boxplot(xBar[1:phaseIEnd], ylab=metric, main="x-bar Distribution")
qqPlot(xBar[1:phaseIEnd],
       main="Check for Normality:  Normal Q-Q Plot",
       xlab="Expected from Cumulative Normal Distribution",
       ylab="Actual Distribution from Data",
       envelope=(1-2*sigAlpha)) # the 1-2*sigAlpha confidence interval to use

# Test for NON-normality of xBar (or x for n=1) using Andersen-Darling Test.
# H0: Normal, H1: Not normal.
# If p-value < sigAlpha then reject H0 and conclude x-bar is non-normal.
ad = ad.test(xBar[1:phaseIEnd])
print(ad$p.value)
xBarIsNormal = (ad$p.value >= sigAlpha)
print(xBarIsNormal)

# If xBar is non-normal, we may need to try a transformation on x. 
# Common transformations include natural log and Box-Cox.  Note,
# however, that the x-bar chart is fairly robust to non-normality.

# Normality of R
hist(R[1:RPhaseIEnd], xlab=metric, ylab="Count", main="R Distribution")
stripchart(R[1:RPhaseIEnd], method="stack", xlab=metric, main="R Distribution")
boxplot(R[1:RPhaseIEnd], ylab=metric, main="R Distribution")
qqPlot(R[1:RPhaseIEnd],
       main="Check for Normality:  Normal Q-Q Plot",
       xlab="Expected from Cumulative Normal Distribution",
       ylab="Actual Distribution from Data",
       envelope=(1-2*sigAlpha)) # the 1-sigAlpha confidence interval to use

# Test for NON-normality of R using Andersen-Darling Test.  
# H0: Normal, H1: Not normal.
# If p-value < sigAlpha (e.g., 0.05) then reject H0 and conclude R is non-normal.
ad = ad.test(R[1:RPhaseIEnd])
RIsNormal = (ad$p.value >= sigAlpha)
print(RIsNormal)

# Control Charts
#
# Use historical data to ensure that we we have a process that
# is performing as we would like -- in control and capable of
# meeting requirements.

# x-bar chart.  The x-bar chart is usually robust even when its assumptions
# are violated, though major violations may lead us to decide not to use it.
xBarChart = qcc(data[1:phaseIEnd,1:n], 
                type="xbar",
                title=xBarTitle,
                xlab=sampleDescription,
                ylab=qualityCharacteristic)

# Check zone rules 1-9.  Montgomery Table 5.1 (except #10).
# See https://en.wikipedia.org/wiki/Western_Electric_rules#Zone_rules.
# Points that are in-control exhibit common cause variation from the mean.
# That means the system is behaving as designed (though it may not be
# producing to spec -- being in-control and to-spec are different things).
# Points that are out-of-control are typically due to special cause
# variation and need to be investigated.

# If any of the points are out of control, it's helpful to get their
# actual values.
print(xBar[1:phaseIEnd])
print(xBarBar)

# Draw the R chart.  Note that the R chart is more sensitive to non-normality
# than the x-bar chart so be careful. [Montgomery 6]
RChart = qcc(RData[1:RPhaseIEnd,1:n], 
             type="R",
             title=RTitle,
             xlab=sampleDescription,
             ylab=qualityCharacteristic)

# Check zone rule 1 only.  Montgomery Table 5.1 #1.
# For broader list of rules, see https://en.wikipedia.org/wiki/Western_Electric_rules#Zone_rules.
# The above notes about common vs. special cause variation apply here, too.
print(R[1:RPhaseIEnd])
print(RBar)
  
# Average Run Length (ARL) and Average Time to Signal (ATS) - xBar Chart [Montgomery 6]
#
# Assume xBar is normal.

# Determine the average run length for a false positive, i.e., outside
# control limits due to common cause variation.
alpha = (1-pnorm(L, mean=0, sd=1)) * 2 # lookup pnorm in Montgomery Appendix II
print(alpha)
ARL0 = 1 / alpha # Montgomery Eq. 6.20
print(ARL0)

# The power (1 - beta) for the given mean shift.  Power is the probability
# that a true out-of-control signal is detected.
power = pnorm(-L-(deltaXBar/sem), 0, 1) + (1 - pnorm(L-(deltaXBar/sem), 0, 1)) # Montgomery Eq. 6.19
print(power)
beta = 1-power
print(beta)

# Average run length for a true alarm that the means shifted.
ARL1 = 1 / power # Montgomery Eq. 6.21
print(ARL1)

# Average time to signal for false alarms (ATS0) and true alarms (ATS1).
ATS0 = h * ARL0
print(ATS0)
ATS1 = h * ARL1
print(ATS1)

# Did we meet our average time to signal target?
print(ATS1 < ATS1Star)

# IMPROVE

# If ATS1 is not less than ATS1Star, 
# we need to increase our sample size (n), increase allowable shift,
# or reduce the time between samples (h).
# Note that the allowable shift should be determined 
# externally (i.e., by the customer), so sample size and
# sampling frequency are what we're most interested in changing.
# 
# Look at the Operating Characteristic (OC) curves 
# to determine the appropriate tradeoff.
oc = oc.curves(xBarChart)
print(oc)

# The OC curve uses shift in standard deviations, k.
print(k)

# Now that we have looked at the OC curves, we can try different 
# sample sizes and see what they do to ARL1 and ARL2.
deltaXBar2 = deltaXBar # since we don't want to change this
n2 = 10 # play with this
h2 = 0.1 # play with this
sem2 = sigmaHat / sqrt(n2)
xBarBar2 = xBarBar
LCL2 = xBarBar2 - L * sem2
UCL2 = xBarBar2 + L * sem2
power2 = pnorm(L-(deltaXBar/sem2), 0, 1) - pnorm(-L-(deltaXBar/sem2), 0, 1)
print(power2)
newARL1 = 1 / power2
print(newARL1)
newATS1 = h2 * newARL1
print(newATS1)

# Determine the average time to signal (ATS) for 
# false alarms (ATS0) and true alarms (ATS1).
ATS0 = h * ARL0
print(ATS0)
ATS1 = h * ARL1
print(ATS1)

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

# x-bar chart.  Phase I centerline and control limits are used.
xBarChart2 = qcc(data[1:phaseIEnd,1:n], 
                 type="xbar", 
                 newdata=data[phaseIIStart:nrow(data),1:n],
                 title=xBarTitle,
                 xlab=sampleDescription,
                 ylab=qualityCharacteristic)

# Check zone rule 1 for special cause variations.  (Rules 2+ are 
# not checked in Phase II because they lead to too many false positives.)  
# If found, execute the Out-of-Control Action Plan.  

# If any of the points are out of control, it's helpful to get their
# actual values.
print(xBar[phaseIIStart:nrow(data)])

# R chart.  Phase I centerline and control limits are used.
RChart2 = qcc(RData[1:RPhaseIEnd,], 
              type="R", 
              newdata=RData[RPhaseIIStart:nrow(RData),],
              title=RTitle,
              xlab=sampleDescription,
              ylab=qualityCharacteristic)

# Check zone rule 1 for special cause variations.  If found, 
# execute the Out-of-Control Action Plan.

# If any of the points are out of control, it's helpful to get their
# actual values.
print(R[RPhaseIIStart:nrow(RData)])

# If the process is changed, or if the product or service produced
# is changed, the center line and control limits
# are no longer valid so go back to Phase I and start over.
# Otherwise continue Phase II with the next data point.