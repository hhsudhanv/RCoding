# Type of Control Chart:  Montgomery back cover.
# If all samples have equal sample size, p and np charts are equivalent
# so choose the one that is most easily understood in the context of
# your organization.  If the sample sizes are unequal, then p charts
# are often more easily explained.
controlChart = "c & u"

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
# The quality characteristic.
# Be sure to include the units for clarity.
qualityCharacteristic = "Defects" # vs. specifications 

# The items being sampled.  Used for the x-axis label.
# It's useful to include the sample size for clarity of presentation.
sampleDescription = "Ft (n=1000)" # x-axis

# Chart titles
uTitle = "Defects per Unit" # u chart title
cTitle = "Defect Count" # c chart title

# Chart y-axis labels
uYLab = "Defects Density (Defects per Ft)"
cYLab = "Defects (Defects per KFt)"

# Sample groups & time between samples.  Sample size, n, per
# sample group is in the data file.
m = 22 # number of sample groups
h = 1.0 # time between samples (hours)

# The number of standard errors of the mean (sigmaHat) to calculate control limits.
L = 3 # One false positive per 370 samples on average.  See ARL0, below.

# The mean defectsPerUnit shift to detect
deltaCBar = 4.4
deltaUBar = deltaCBar / 1000

# The max average time to signal of true out-of-control point (ATS1).
ATS1Star = 2 # hours

# Data table column names.  Note that R replaces spaces with periods (".").
sample = "KFt"
sampleSize = "Sample.Size" # n
metric = "Number.of.Nonconformities" # c

# MEASURE

# Raw data read into data frame.
# If this results in an error, you need to set the working directory.
# In RStudio's Session menu, choose "Set Working Directory".
# The easiest approach is to put the data files and the R files
# in the same directory and choose "To Source File Location".
rawData = read.csv("M7E15.csv", 
                   header=TRUE, 
                   sep=",")

# The raw data's phase I & phase II breakpoints.
rawPhaseIEnd = 22
rawPhaseIIStart = rawPhaseIEnd + 1

# DataFrame reformatted frame with each row being a sample group of size n.
# If the data is already in the correct format, set data = rawdata.
data = rawData

# The data's phase I & phase II breakpoints.
phaseIEnd = rawPhaseIEnd
phaseIIStart = phaseIEnd + 1
print(data[1:phaseIEnd,])

# Nonconformities per Unit, u-hat's.  (Montgomery eq. 7.18)
defectsPerUnit = "Nonconformities.per.Unit"
data[defectsPerUnit] = data[metric] / data[sampleSize]

# c-bar is the average of the c's.
cBar = mean(data[1:phaseIEnd, metric])
print(cBar)

# u-bar is the average of the u-hats's.  If n is constant,
# npBar = n * pBar.
uBar = mean(data[1:phaseIEnd, defectsPerUnit])
print(uBar)

# n-bar is the average of the n's (sample sizes).
# Usually n is constant across all sample groups.
nBar = mean(data[1:phaseIEnd, sampleSize])
print(nBar)

# Unbiased estimates of the c and u's standard deviations.
# If n is constant, sigmaHatC = sigmaHatU * n.
sigmaHatC = sqrt(cBar)
sigmaHatU = sqrt(uBar / nBar)
print(sigmaHatC)
print(sigmaHatU)

# c chart's control, warning and one-sigma limits.  (Montgomery Eq 7.17)
# The control limits, like c, must be >= 0.
# If n is constant, divide by n to get u chart's control limits.
cUCL = cBar + L*sigmaHatC                             # upper control limit
cUWL = cBar + (2/3)*L*sigmaHatC                       # upper warning limit
cOneSigmaLimit = cBar + (1/3)*L*sigmaHatC             # one-sigma limit
cNegativeOneSigmaLimit = max(cBar-(1/3)*L*sigmaHatC, 0) # negative one-sigma limit
cLWL = max(cBar - (2/3) * L * sigmaHatC , 0)           # lower warning limit
cLCL = max(cBar - L * sigmaHatC, 0)                   # lower control limit
print(cUCL)
print(cLCL)

# ANALYZE

# Key assumptions:  
# 1. Independence of data points.
# 2. Constant variance (homoscedasticity)
# 3. Normality of the control chart parameter, p-bar, including
# validity of normal approximation of binomial distribution.

# Trend checks.  See if there are any non-random patterns,
# such as autocorrelation (i.e., correlation of successive points).
# If there is autocorrelation, independence is violated.
# If the variance seems to be increasing or shrinking, there
# may be heteroscedasticity (i.e., non-homoscedasticity).
matplot(y=data[1:phaseIEnd, metric], 
        type="l", 
        lty=1, 
        xlab=sample, 
        ylab=cYLab, 
        main="Trend Check")

# Normal approximation is good for Poisson if n >= 10.  
print(nBar)
cIsNormal = (nBar >= 10)
print(cIsNormal)

# The Central Limit Theorem says p-bar should tend towards normal as m -> infinity.
# Graphically check for normality.
hist(data[1:phaseIEnd, metric], 
     xlab=metric, 
     ylab="Count", 
     main="c Distribution")
stripchart(data[1:phaseIEnd, metric], 
           method="stack", 
           xlab=cYLab, 
           main="c Distribution")
boxplot(data[1:phaseIEnd, metric], 
        ylab=cYLab, 
        main="c Distribution")
qqPlot(data[1:phaseIEnd, metric],
       main="Check for Normality:  Normal Q-Q Plot",
       xlab="Expected from Cumulative Normal Distribution",
       ylab="Actual Distribution from Data",
       envelope=(1-2*sigAlpha)) # the 1-alpha confidence interval to use

# Test for NON-normality using Andersen-Darling Test.  
# H0: Normal, H1: Not normal.
# If p-value < alpha (e.g., 0.05) then reject H0 and conclude non-normal.
ad = ad.test(data[1:phaseIEnd, metric])
print(ad$p.value)
cIsNormal = (ad$p.value >= sigAlpha)
print(cIsNormal)

# Control Charts
#
# Use historical data to ensure that we we have a process that
# is performing as we would like -- in control and capable of
# meeting requirements.

# u chart.  (Note:  Use the metric, c, not u, for the data.)
uChart = qcc(data[1:phaseIEnd, metric], 
             sizes=data[1:phaseIEnd, sampleSize], 
             type="u",
             title=uTitle,
             xlab=sampleDescription,
             ylab=uYLab)

# c chart.
cChart = qcc(data[1:phaseIEnd, metric], 
              sizes=data[1:phaseIEnd, sampleSize], 
              type="c",
              title=cTitle,
              xlab=sampleDescription,
              ylab=cYLab)

# Check zone rules 1-9.  Montgomery Table 5.1 (except #10).
# See https://en.wikipedia.org/wiki/Western_Electric_rules#Zone_rules.
# Points that are in-control exhibit common cause variation from the mean.
# That means the system is behaving as designed (though it may not be
# producing to spec -- being in-control and to-spec are different things).
# Points that are out-of-control are typically due to special cause
# variation and need to be investigated.

# If any of the points are out of control, it's helpful to get their
# actual values.
print(data[1:phaseIEnd, defectsPerUnit]) # uHat
print(data[1:phaseIEnd, metric]) # np

# Average Run Length (ARL) and Average Time to Signal (ATS) - x Chart [Montgomery 6]
#
# Assume p-hat is normal.

# The average run length for a false positive, i.e., outside
# control limits due to common cause variation.
alpha = (1-pnorm(3, mean=0, sd=1))*2
print(alpha)
ARL0 = 1/alpha
print(ARL0)

# The power (1 - beta) for the given mean shift.  Power is the probability
# that a true out-of-control signal is detected.  (Montgomery Eq 7.15)
c1 = cBar + deltaCBar
power <- ppois(cLCL, lambda=c1, lower.tail=TRUE) + (1 - ppois(cUCL, lambda=c1, lower.tail=TRUE))
beta = 1 - power
print(power)
print(beta)

# The average run length for a true alarm that the
# means shifted.
ARL1 = 1 / power
print(ARL1)

# The average time to signal (ATS) for 
# false alarms (ATS0) and true alarms (ATS1).
ATS0 = h * ARL0
print(ATS0)
ATS1 = h * ARL1
print(ATS1)

# Did we meet our average time to signal target?
print(ATS1 < ATS1Star)

# IMPROVE

# If the u chart's LCL <= 0 (equivalently, if the c chart's LCL <=0),
# the normality approximation of uBar is hard to justify 
# because uBar cannot take on values < 0.  Ideally, we increase the sample
# size so 0 < LCL.  Estimate the minimum sample
# size needed to get LCL >= 0.
print(nBar)
nMin = 9 / uBar
print(nMin)

# For the normal approximation of the binomial to hold, we
# want n >= 10.
print(nBar)
nMin = 10
print(nMin)

# If ARL1 is not < ARL0 or if ATS1 is not <= ATS1*, 
# we need to increase the allowable shift or reduce 
# the time between samples (h).
#
# Note:  Because c follows a Poisson distribution, changing 
# the sample size will have no impact on 
# ARL1 and, thus, on ATS1*!
#
# Note that the allowable shift should be determined 
# externally (i.e., by the customer), so the
# sampling frequency is what we're most interested in changing.
# 
# Look at the Operating Characteristic (OC) curves 
# to determine the appropriate tradeoff between the mean
# count, c-bar, and beta.
oc = oc.curves(cChart)
print(oc)

# Now that we have looked at the OC curves, we can try different 
# sampling frequencies and see what they do to ARL0 and ARL1.
# 
# WARNING:  If LCL2 <= 0 below, we could end up with
# newARL1 > ARL1 because of the mass at LCL2, even if
# nBar2 > nBar.
deltaCBar2 = deltaCBar # since we don't want to change this
nBar2 = nBar # not used
h2 = 0.5
cBar2 = cBar
standardError2 = sqrt(cBar2)
cLCL2 = max(cBar2 - 3*standardError2, 0)
cUCL2 = cBar2 + 3*standardError2
c12 = cBar2 + deltaCBar2
power2 = ppois(cLCL2, lambda=c12, lower.tail=TRUE) + (1 - ppois(cUCL2, lambda=c12, lower.tail=TRUE))
print(power2)
newARL1 = 1 / power2
print(newARL1)
newATS1 = h2 * newARL1
print(newATS1)

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

# Generate the u chart.
pChart2 = qcc(data[1:phaseIEnd, metric], 
              sizes=data[1:phaseIEnd, sampleSize], 
              type="u", 
              newdata=data[phaseIIStart:nrow(data), metric],
              newsizes=data[phaseIIStart:nrow(data), sampleSize],
              title=uTitle,
              xlab=sampleDescription,
              ylab=uYLab)

# Generate the c chart.
npChart2 = qcc(data[1:phaseIEnd, metric], 
               sizes=data[1:phaseIEnd, sampleSize], 
               type="c", 
               newdata=data[phaseIIStart:nrow(data), metric],
               newsizes=data[phaseIIStart:nrow(data), sampleSize],
               title=cTitle,
               xlab=sampleDescription,
               ylab=cYLab)

# Check zone rule 1 for special cause variations.  If found, 
# execute the Out-of-Control Action Plan.

# If any of the points are out of control, it's helpful to get their
# actual values.
print(data[phaseIIStart:nrow(data),])

# If the process is changed, or if the product or service produced
# is changed, the center line and control limits
# are no longer valid so go back to Phase I and start over.
# Otherwise continue Phase II with the next data point.

