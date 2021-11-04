#Homework 3 Type 2
# Hari hara sudhan Venkateswaran,Jalpeshkumar Bhavsar,Nandhini Varadharajan, Siddhant Kansal


#       NOTE :All output pasted in the code as comments.

# 1) Read the data file. Variable "dayweek" shows the day of the week the flights are
# scheduled. Recode this variable such that it will take value of 1 for Sunday and Monday,
# and it will take value of 0 for all other days(tue,wed,thu,fri,sat).

myData <- read.csv("Hw3_data.csv",stringsAsFactors = FALSE)
#attach(myData)

myData$delay

head(myData)
tail(myData)

myData$dayweek[myData$dayweek<3] <- 1
myData$dayweek[myData$dayweek>2] <- 0
# head(myData)
# attach(myData)
# detach(myData)

myData$delay[myData$delay == "ontime"] <- 1
myData$delay[myData$delay == "delayed"] <- 0

myData$delay <- as.numeric(myData$delay)

#Different Method of splitting data

index = 1:1541

myData_Train <- myData[index,]
myData_Test <- myData[-index,]
dim(myData_Train)
dim(myData_Test)
head(myData_Train)


# glm.Train <- glm(myData_Train$delay~myData_Train$dayweek, data = myData_Train,family = "binomial")
# summary(glm.Train)

# 2) Use Set.seed(1) and apply holdout method by sampling %70 of your data for training set,
# and remaining for holdout set.

set.seed(1)
train = sample(2201,1541) # Take random 1541 values from the myData set

# 3) Fit a logistic regression to predict delay and comment on your findings.

glm.delay <- glm(myData$delay~myData$dayweek,data = myData, subset = train,family = "binomial")
glm.delay
summary(glm.delay)

glm.delay.all <- glm(myData$delay~.,data = myData, subset = train,family = "binomial")
glm.delay.all

summary(glm.delay.all)

# glm(formula = myData$delay ~ ., family = "binomial", data = myData, 
#     subset = train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.4334   0.3787   0.5323   0.6978   1.0338  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.857e+00  5.170e-01   3.592 0.000329 ***
#   X           -2.206e-04  1.081e-04  -2.042 0.041189 *  
#   carrierDH    3.005e-01  4.811e-01   0.625 0.532258    
# carrierDL    8.709e-01  4.711e-01   1.849 0.064510 .  
# carrierMQ   -1.490e-01  4.501e-01  -0.331 0.740584    
# carrierOH    1.619e+00  9.195e-01   1.761 0.078288 .  
# carrierRU    2.641e-01  3.804e-01   0.694 0.487519    
# carrierUA    8.975e-01  9.219e-01   0.974 0.330269    
# carrierUS    1.075e+00  4.773e-01   2.252 0.024341 *  
#   destJFK      2.384e-01  2.866e-01   0.832 0.405545    
# destLGA     -3.723e-02  3.038e-01  -0.123 0.902453    
# originDCA    7.005e-01  3.365e-01   2.082 0.037367 *  
#   originIAD    2.782e-01  3.332e-01   0.835 0.403698    
# weather     -1.676e+01  3.159e+02  -0.053 0.957702    
# dayweek     -1.593e-01  1.501e-01  -1.062 0.288426    
# sched       -7.592e-02  1.622e-02  -4.682 2.85e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1530.5  on 1540  degrees of freedom
# Residual deviance: 1384.4  on 1525  degrees of freedom
# AIC: 1416.4
# 
# Number of Fisher Scoring iterations: 14

head(myData)

# Comment on Findings:
# From above results we can see that P-Value of the carrierDL, carrierUS, originDCA, 
# dayweek, sched are less than the alfa value(assuming 0.05 level of significance). 
# So, dayweek and shed have effect on the delay. 
# Also, we can see more delays in carrier DL & US, and flight with origin as DCA.
# Rest of the factors do not have any effect on the delay as the P-Values are less than the alfa value.


#probability of ontime flights during monday and sundays (x = 1)

exp(1.59208-0.57351*1)/(1+exp(1.59208-0.57351*1))
# 0.734694

#probability of ontime flights on rest of the days

exp(1.59208-0.57351*0)/(1+exp(1.59208-0.57351*0))
# 0.8309085

mean((myData$delay-predict(glm.delay,myData))[-train]^2) #Q5
# plot(myData$dayweek,myData$delay)
# abline(glm.delay)
# glm.delay.prob<-predict(glm.delay,data=myData, type="response") #predict probabilities
# glm.delay.prob


# 4) Estimate your traininag error


glm.delay.predict.prob <- predict(glm.delay.all, data = myData, type="response")
glm.delay.predict <- ifelse(glm.delay.predict.prob>0.5,1,0)
confusionmatrix_train <- table(myData$delay[train],glm.delay.predict)

confusionmatrix_train

(277/1541)

# Training Error - 0.1797534*100 = 17.97%


# 5) Estimate test error using holdout set

glm.delay.predict.prob_test <- predict(glm.delay,myData)[-train]
glm.delay.predict_test <- ifelse(glm.delay.predict.prob_test > 0.5,1,0)

table(myData$delay[-train],glm.delay.predict_test)


#Misclassification
((1+125)/660)
#Test Error - 19.09%

#MSE for test

test_error.dayweek <- mean((myData$delay-predict(glm.delay,myData))[-train]^2)
test_error <- mean((myData$delay-predict(glm.delay.all,myData))[-train]^2)

test_error.dayweek
#0.5217668
test_error
#5.415368

# 6) Now, instead of holdout method apply 10-fold cross validation and repeat this for 10
# times. Report CV errors. (set.seed(18))

library(boot)
set.seed(18)

glm.fit2=lm(myData$delay~poly(myData$dayweek,2),data=myData)


cv.error.10=rep(0,10)

for (i in 1:10){
  glm.fit=glm(myData$delay~(myData$dayweek),data=myData, family = "binomial")
  cv.error.10[i]=cv.glm(myData,glm.fit,K=10)$delta[1]   #K=10 argument for 10-fold cross validation
}

#CV Error
cv.error.10

#0.1574921 0.1576331 0.1574690 0.1573697 0.1573852 0.1574068 0.1573475 0.1574192 0.1573511 0.1574085

