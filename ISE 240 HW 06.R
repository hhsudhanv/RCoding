#Question 1 
set.seed(1)
#install.packages("readxl")
library("readxl")
my_data <- read_excel("FashionDataSet.xlsx")

head(my_data)

# Spell check for FabricType
table(my_data$FabricType)
#my_data$FabricType <- as.character(my_data$FabricType)
my_data$FabricType[my_data$FabricType == "sattin"] <- "satin"
my_data$FabricType[my_data$FabricType == "knitting"] <- "knitted"
my_data$FabricType[my_data$FabricType == "flannael"] <- "flannel"
my_data$FabricType[my_data$FabricType == "shiffon"] <- "chiffon"
my_data$FabricType[my_data$FabricType == "wollen"] <- "woolen"
table(my_data$FabricType)

# Spell check for Style
table(my_data$Style)
#my_data$Style <- as.character(my_data$Style)
my_data$Style[my_data$Style == "sexy"] <- "Sexy"
table(my_data$Style)

#Spell check for Price
table(my_data$Price)
#my_data$Price <- as.character(my_data$Price)
my_data$Price[my_data$Price == "low"] <- "Low"
my_data$Price[my_data$Price == "high"] <- "High"
table(my_data$Price)

#Spell check for Size
table(my_data$Size)
#my_data$Size <- as.character(my_data$Size)
my_data$Size[my_data$Size == "s"] <- "S"
my_data$Size[my_data$Size == "small"] <- "S"
table(my_data$Size)

#Spell check for Season
table(my_data$Season)
#my_data$Season <- as.character(my_data$Season)
my_data$Season[my_data$Season == "Automn"] <- "Autumn"
my_data$Season[my_data$Season == "spring"] <- "Spring"
my_data$Season[my_data$Season == "summer"] <- "Summer"
my_data$Season[my_data$Season == "winter"] <- "Winter"
table(my_data$Season)

#Spell check for Neckline
table(my_data$NeckLine)
#my_data$NeckLine<- as.character(my_data$NeckLine)
my_data$NeckLine[my_data$NeckLine == "sqare-collor"] <- "Square-collor"
my_data$NeckLine[my_data$NeckLine == "sweetheart"] <- "Sweetheart"
my_data$NeckLine[my_data$NeckLine == "mandarin-collor"] <- "Mandarin-collar"
my_data$NeckLine[my_data$NeckLine == "Scoop"] <- "Scoop Shape"
table(my_data$NeckLine)

#Spell check for Sleeve Length 
table(my_data$SleeveLength)
#my_data$SleeveLength<- as.character(my_data$SleeveLength)
my_data$SleeveLength[my_data$SleeveLength == "cap-sleeves"] <- "cap sleeve"
my_data$SleeveLength[my_data$SleeveLength == "capsleeves"] <- "cap sleeve"
my_data$SleeveLength[my_data$SleeveLength == "sleeevless"] <- "sleeveless"
my_data$SleeveLength[my_data$SleeveLength == "sleevless"] <- "sleeveless"
my_data$SleeveLength[my_data$SleeveLength == "sleveless"] <- "sleeveless"
my_data$SleeveLength[my_data$SleeveLength == "threequater"] <- "threequarter"
my_data$SleeveLength[my_data$SleeveLength == "thressqatar"] <- "threequarter"
my_data$SleeveLength[my_data$SleeveLength == "turndowncollor"] <- "turndowncollar"
my_data$SleeveLength[my_data$SleeveLength == "urndowncollor"] <- "turndowncollar"
table(my_data$SleeveLength)

#Spell check for Waiseline 
# No Corrections

#Spell check for Material 
table(my_data$Material)
#my_data$Material<- as.character(my_data$Material)
my_data$Material[my_data$Material == "model"] <- "modal"
my_data$Material[my_data$Material == "shiffon"] <- "chiffon"
my_data$Material[my_data$Material == "sill"] <- "silk"
my_data$Material[my_data$Material == "viscos"] <- "viscose"
table(my_data$Material)

#Spell check for Decoration
table(my_data$Decoration)
#my_data$Decoration<- as.character(my_data$Decoration)
my_data$Decoration[my_data$Decoration == "draped"] <- "drape"
my_data$Decoration[my_data$Decoration == "embroidary"] <- "embroidery"
my_data$Decoration[my_data$Decoration == "pleat"] <- "plait"
my_data$Decoration[my_data$Decoration == "sequined"] <- "sequin"
table(my_data$Decoration)

#Spell check for Pattern Type
table(my_data$`Pattern Type`)
my_data$`Pattern Type`<- as.character(my_data$`Pattern Type`)
my_data$`Pattern Type`[my_data$`Pattern Type` == "leapord"] <- "leopard"
table(my_data$`Pattern Type`)

my_data$`Pattern Type`
#Spell check for Pattern Type
table(my_data$Recommendation)
#No Correction


#______________________________________________________________________#
#QUESTION_1(b)

install.packages("missForest")
library(missForest)

fashiondata <- my_data

head(fashiondata)

fashiondata$Style <- as.factor(fashiondata$Style)
fashiondata$Style 
fashiondata$Price <- as.factor(fashiondata$Price)
fashiondata$Price
fashiondata$Size <- as.factor(fashiondata$Size)
fashiondata$Size
fashiondata$Season <- as.factor(fashiondata$Season)
fashiondata$Season
fashiondata$NeckLine <- as.factor(fashiondata$NeckLine)
fashiondata$NeckLine
fashiondata$SleeveLength <- as.factor(fashiondata$SleeveLength)
fashiondata$SleeveLength
fashiondata$waiseline <- as.factor(fashiondata$waiseline)
fashiondata$waiseline
fashiondata$Material <- as.factor(fashiondata$Material)
fashiondata$Material
fashiondata$FabricType <- as.factor(fashiondata$FabricType)
fashiondata$FabricType 
fashiondata$Decoration <- as.factor(fashiondata$Decoration)
fashiondata$Decoration
fashiondata$`Pattern Type`<- as.factor(fashiondata$`Pattern Type`)
factor(fashiondata$`Pattern Type`)

fashiondata[fashiondata == "Null"] <- NA
fashiondata[fashiondata == "NULL"] <- NA
fashiondata[fashiondata == "null"] <- NA
fashiondata[fashiondata == ""] <- NA
fashiondata$Dress_ID <- NULL


fashiondata <- as.data.frame(fashiondata)

fashion.imputed <- missForest(fashiondata)

fashion.imp <- fashion.imputed$ximp

is.na(fashion.imp$Material)

fashion.imp$Recommendation <- as.factor(fashion.imp$Recommendation)

write.csv(fashion.imp,"FashionData_Cleaned.csv")

#Question 2 

#METHOD 1 - NAIVE BAYES CLASSIFICATION

install.packages("e1071")
library(e1071)

n=dim(fashion.imp)[1]
n
n1=floor(n*(0.6))
n1
n2=n-n1
n2
train=sample(1:n,n1)
fashion.train=fashion.imp[train,]
fashion.test=fashion.imp[-train,]

Naive_Bayes_Model=naiveBayes(fashion.train$Recommendation~.,data=fashion.train)
summary(Naive_Bayes_Model)
NB_Predictions=predict(Naive_Bayes_Model,fashion.test, type="class")
NB_Predictions

table(fashion.test$Recommendation,NB_Predictions)

(87+36)/200
#Accuracy - 61.5%

#METHOD 2 - KNN

library("ISLR")
library("class")

#recommendation=my_data$Recommendation[!train]
#train.recommendation=my_data$Recommendation[train]

train.recommendation <- fashion.train$Recommendation

fashion.knn <- fashion.imp

fashion.knn$Style <- as.numeric(fashion.knn$Style)
fashion.knn$Price <- as.numeric(fashion.knn$Price)
fashion.knn$Size <- as.numeric(fashion.knn$Size)
fashion.knn$Season <- as.numeric(fashion.knn$Season)
fashion.knn$NeckLine<- as.numeric(fashion.knn$NeckLine)
fashion.knn$SleeveLength <- as.numeric(fashion.knn$SleeveLength)
fashion.knn$waiseline <- as.numeric(fashion.knn$waiseline)
fashion.knn$Material <- as.numeric(fashion.knn$Material)
fashion.knn$FabricType <- as.numeric(fashion.knn$FabricType)
fashion.knn$Decoration <- as.numeric(fashion.knn$Decoration)
fashion.knn$`Pattern Type` <- as.numeric(fashion.knn$`Pattern Type`)

fashion.knn$Recommendation



Recommendation <- fashion.knn$Recommendation[train]

fashion.knn.std <- scale(fashion.knn[,-13])

knn.train <- fashion.knn.std[train,]
knn.test <- fashion.knn.std[-train,]

knn.pred <- knn(knn.train,knn.test,Recommendation,k = 3)

table(fashion.knn$Recommendation[-train],knn.pred)
(76+28)/200
#Accuracy - 52%


#METHOD 3 - Logistic Regression

glm.fashion <- glm(Recommendation~.,data = fashion.imp, family = "binomial")
glm.fashion
summary(glm.fashion)
glm.predict.prob <- predict(glm.fashion, type = "response")
glm.predict <- ifelse(glm.predict.prob > 0.5,1,0)


table(fashion.imp$Recommendation, glm.predict)

(238+139)/500
#Accuracy - 75.4%

#Result - Based on the accuracy values of the classification methods, we recommend the use of logisctic regression as it has the highest accuracy value of 75.4%
