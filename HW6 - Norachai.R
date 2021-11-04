#### Homework6 #####
# Norachai Sanguansaringkhan    012432550
# Shilen Jhaveri                012485798
# Kamakshi Andhale              012481339
# Malav Patel                   012444042

install.packages("hunspell")
install.packages("spelling")
install.packages("rJava")

library(spelling)
library(hunspell)
library(readxl)

#1a.Check spelling mistakes to make sure every category is entered correctly, 
#if not, correct the entries. Explain which corrections are made in your report.

Fashion_data = read_excel("FashionDataSet.xlsx")
Fashion_data
attach(Fashion_data)
colnames(Fashion_data)
Fashion_data$Style = as.character(Fashion_data$Style)
Fashion_data$Price = as.character(Fashion_data$Price)
Fashion_data$Size = as.character(Fashion_data$Size)
Fashion_data$Season = as.character(Fashion_data$Season)
Fashion_data$NeckLine = as.character(Fashion_data$NeckLine)
Fashion_data$SleeveLength = as.character(Fashion_data$SleeveLength)
Fashion_data$waiseline = as.character(Fashion_data$waiseline)
Fashion_data$Material = as.character(Fashion_data$Material)
Fashion_data$FabricType = as.character(Fashion_data$FabricType)
Fashion_data$Decoration = as.character(Fashion_data$Decoration)
Fashion_data$Pattern.Type = as.character(Fashion_data$Pattern.Type)
class(Fashion_data$NeckLine)
class(Style)
summary(Fashion_data)

SpellingCk1 = hunspell_check(Fashion_data$Style)
SpellingCk1
levels(Fashion_data$Style)
hunspell_suggest(Fashion_data$Style[!SpellingCk1])
table(Fashion_data$Style)
Fashion_data$Style[Fashion_data$Style == "sexy"] <- "Sexy"
table(Fashion_data$Style)

SpellingCk2 = hunspell_check(Fashion_data$Price)
SpellingCk2
table(Fashion_data$Price)
Fashion_data$Price[Price == "low"] <- "Low"
Fashion_data$Price[Price == "high"] <- "High"
table(Fashion_data$Price)

SpellingCk3 = hunspell_check(Fashion_data$Size)
SpellingCk3
table(Fashion_data$Size)
Fashion_data$Size[Fashion_data$Size == "s"] <- "S"
Fashion_data$Size[Fashion_data$Size == "small"] <- "S"
table(Fashion_data$Size)

SpellingCk4 = hunspell_check(Fashion_data$Season)
SpellingCk4
hunspell_suggest(Fashion_data$Season[!SpellingCk4])#Autumn
table(Fashion_data$Season)


Fashion_data$Season[Fashion_data$Season == "Automn"] <-"Autumn"
Fashion_data$Season[Fashion_data$Season == "spring"] <-"Spring"
Fashion_data$Season[Fashion_data$Season == "summer"] <-"Summer"
Fashion_data$Season[Fashion_data$Season == "winter"] <-"Winter"
table(Fashion_data$Season)


SpellingCk5 = hunspell_check(Fashion_data$NeckLine)
SpellingCk5
hunspell_suggest(Fashion_data$NeckLine[!SpellingCk5])
Fashion_data$NeckLine
table(Fashion_data$NeckLine)
Fashion_data$NeckLine[NeckLine == "bowneck"] <- "bow-neck"
Fashion_data$NeckLine[Fashion_data$NeckLine == "mandarin-collor"] <- "mandarin-collar"
Fashion_data$NeckLine[Fashion_data$NeckLine == "NULL"] <- "null"
Fashion_data$NeckLine[Fashion_data$NeckLine == "peterpan-collor"] <- "peter-pan-collor"
Fashion_data$NeckLine[Fashion_data$NeckLine == "sqare-collor"] <- "square-collor"
Fashion_data$NeckLine[Fashion_data$NeckLine == "Sweetheart"] <- "sweetheart"
table(Fashion_data$NeckLine)


SpellingCk6 = hunspell_check(Fashion_data$SleeveLength)
SpellingCk6
hunspell_suggest(Fashion_data$SleeveLength[!SpellingCk6])
table(Fashion_data$SleeveLength)
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "capsleeves"] <- "cap-sleeves"
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "halfsleeve"] <- "half-sleeve"
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "half"] <- "half-sleeve"
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "NULL"] <- "null"
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "sleeevless"] <- "sleeveless"
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "sleevless"] <- "sleeveless"
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "sleveless"] <- "sleeveless"
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "threequarter"] <- "three-quarter"
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "threequater"] <- "three-quarter"
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "thressqatar"] <- "three-quarter"
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "turndowncollor"] <- "turndown-collor"
Fashion_data$SleeveLength[Fashion_data$SleeveLength == "urndowncollor"] <- "turndown-collor"
table(Fashion_data$SleeveLength)

SpellingCk7 =hunspell_check(Fashion_data$waiseline)
SpellingCk7
table(Fashion_data$waiseline)

SpellingCk8 =hunspell_check(Fashion_data$Material)
SpellingCk8
hunspell_suggest(Fashion_data$Material[!SpellingCk8])
table(Fashion_data$Material)
Fashion_data$Material[Fashion_data$Material == "chiffonfabric"] <- "chiffon-fabric"
Fashion_data$Material[Fashion_data$Material == "milksilk"] <- "milk-silk"
Fashion_data$Material[Fashion_data$Material == "model"] <- "modal"
Fashion_data$Material[Fashion_data$Material == "polyster"] <- "polyester"
Fashion_data$Material[Fashion_data$Material == "sill"] <- "silk"
table(Fashion_data$Material)

SpellingCk9 = hunspell_check(Fashion_data$FabricType)
SpellingCk9
hunspell_suggest(Fashion_data$FabricType[!SpellingCk9])
table(Fashion_data$FabricType)
Fashion_data$FabricType[Fashion_data$FabricType == "flannael"] <- "flannel"
Fashion_data$FabricType[Fashion_data$FabricType == "knitting"] <- "knitted"
Fashion_data$FabricType[Fashion_data$FabricType == "sattin"] <- "satin"
Fashion_data$FabricType[Fashion_data$FabricType == "shiffon"] <- "chiffon"
Fashion_data$FabricType[Fashion_data$FabricType == "wollen"] <- "woolen"
table(Fashion_data$FabricType)

SpellingCk10 = hunspell_check(Fashion_data$Decoration)
SpellingCk10
hunspell_suggest(Fashion_data$Decoration[!SpellingCk10])
table(Fashion_data$Decoration)
Fashion_data$Decoration[Fashion_data$Decoration == "embroidary"] <- "embroidery"
Fashion_data$Decoration[Fashion_data$Decoration == "hollowout"] <- "hollow out"
table(Fashion_data$Decoration)


SpellingCk11 = hunspell_check(Fashion_data$Pattern.Type)
SpellingCk11
hunspell_suggest(Fashion_data$Pattern.Type[!SpellingCk11])
table(Fashion_data$Pattern.Type)
Fashion_data$Pattern.Type[Fashion_data$Pattern.Type == "leapord"] <- "leopard"
table(Fashion_data$Pattern.Type)

print(Fashion_data)

#1b. How would you treat the missing information (entries with “null”) in this data set.
#You can search the alternative methods for imputing missing values and pick one approach.

install.packages("DMwR")
library(DMwR)

Fashion_data[Fashion_data == "null"] <- NA
Fashion_data[Fashion_data == ""] <- NA
Fashion_data[Fashion_data == "Null"] <- NA

Fashion_data$Dress_ID <- NULL

Fashion_data$Style = as.factor(Fashion_data$Style)
Fashion_data$Price = as.factor(Fashion_data$Price)
Fashion_data$Size = as.factor(Fashion_data$Size)
Fashion_data$Season = as.factor(Fashion_data$Season)
Fashion_data$NeckLine = as.factor(Fashion_data$NeckLine)
Fashion_data$SleeveLength = as.factor(Fashion_data$SleeveLength)
Fashion_data$waiseline = as.factor(Fashion_data$waiseline)
Fashion_data$Material = as.factor(Fashion_data$Material)
Fashion_data$FabricType = as.factor(Fashion_data$FabricType)
Fashion_data$Decoration = as.factor(Fashion_data$Decoration)
Fashion_data$`Pattern Type` = as.factor(Fashion_data$`Pattern Type`)
Fashion_data$Recommendation = as.factor(Fashion_data$Recommendation)

Imputed_Fashion_data = knnImputation(Fashion_data)
anyNA(Imputed_Fashion_data)

Imputed_Fashion_data

#2 Use atleast three methods to build classification model to predict recommendation.
# Logistic Regression
install.packages("ISLR")
library(ISLR)
library(MASS)
is.factor(Fashion_data$Recommendation)
Fashion_logistic=glm(Imputed_Fashion_data$Recommendation~., data = Imputed_Fashion_data, family = "binomial")
Fashion_logistic
summary(Fashion_logistic)

predicted_recommendation_prob<-predict(Fashion_logistic,data=Imputed_Fashion_data, type="response")
predicted_recommendation<-ifelse(predicted_recommendation_prob>0.5,"1","0")
predicted_recommendation

table(Imputed_Fashion_data$Recommendation, predicted_recommendation)
# predicted_recommendation
#             0   1
#         0 235  55
#         1  72 138
Accuracy_Logistic = mean(predicted_recommendation==Imputed_Fashion_data$Recommendation)
Accuracy_Logistic
#[1] 0.746
misclassification_error_Logistic = 1-Accuracy_Logistic
misclassification_error_Logistic
#[1] 0.254
#----------------------------------------------------------------------------------------------------------

#Naive Bayes
install.packages("e1071")
library(e1071)
NaiveBayes_Fashion = naiveBayes(Imputed_Fashion_data$Recommendation~., data = Imputed_Fashion_data)
NaiveBayes_Fashion

NaiveBayes_predicted = predict(NaiveBayes_Fashion, Imputed_Fashion_data, type = "class")
table(NaiveBayes_predicted, Imputed_Fashion_data$Recommendation)
#NaiveBayes_predicted   0   1
#                   0 239  93
#                   1  51 117

Accuracy_Naive = mean(NaiveBayes_predicted == Imputed_Fashion_data$Recommendation)
Accuracy_Naive
#[1] 0.712
misclassification_error_Naive = 1-Accuracy_Naive
misclassification_error_Naive
#[1] 0.288

#-----------------------------------------------------------------------------------------------------------

#KNN

library(ISLR)
library(class)

Imputed_Fashion_data$Style = as.numeric(Imputed_Fashion_data$Style)
Imputed_Fashion_data$Price = as.numeric(Imputed_Fashion_data$Price)
Imputed_Fashion_data$Rating = as.numeric(Imputed_Fashion_data$Rating)
Imputed_Fashion_data$Size = as.numeric(Imputed_Fashion_data$Size)
Imputed_Fashion_data$Season = as.numeric(Imputed_Fashion_data$Season)
Imputed_Fashion_data$NeckLine = as.numeric(Imputed_Fashion_data$NeckLine)
Imputed_Fashion_data$SleeveLength = as.numeric(Imputed_Fashion_data$SleeveLength)
Imputed_Fashion_data$waiseline = as.numeric(Imputed_Fashion_data$waiseline)
Imputed_Fashion_data$Material = as.numeric(Imputed_Fashion_data$Material)
Imputed_Fashion_data$FabricType = as.numeric(Imputed_Fashion_data$FabricType)
Imputed_Fashion_data$Decoration = as.numeric(Imputed_Fashion_data$Decoration)
Imputed_Fashion_data$Pattern.Type = as.numeric(Imputed_Fashion_data$Pattern.Type)


Standardized.data = scale(Imputed_Fashion_data[,-13])
Standardized.data                                               

train = sample(500, 125)
train_set = Standardized.data[train,]
test_set = Standardized.data[-train,]

Recommendation_train = Imputed_Fashion_data$Recommendation[train]
Recommendation_train
length(Recommendation_train)
Recommendation_test = Imputed_Fashion_data$Recommendation[-train]

set.seed(1)
knn_predict = knn(train_set, test_set, Recommendation_train, k=3)
table(knn_predict, Recommendation_test)
#         Recommendation_test
#     knn_predict   0   1
#               0 135  97
#               1  79  64 
Accuracy_knn = mean(knn_predict == Recommendation_test)
Accuracy_knn
# [1] 0.5306667
misclassification_error_knn = 1-Accuracy_knn
misclassification_error_knn
#[1] 0.4693333






