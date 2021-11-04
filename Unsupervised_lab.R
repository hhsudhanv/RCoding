######################################
# K-Means Clustering
######################################
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)  #a simulated example

x
dist(x)
plot(x,xlim=c(-2.5,6),ylim=c(-6,2.5))
x[1:25,1]=x[1:25,1]+3  # first 25 rows are shifted by 3 in the first col, shifted by 4 in the second column
x[1:25,2]=x[1:25,2]-4  #so that we have distinct two sets
plot(x,xlim=c(-2.5,6),ylim=c(-6,2.5))
km.out=kmeans(x,centers=2,nstart=20)    #kmeans() from stats package (default package)
                                #nstart=20->start with 20 random initial assignment
                                #report the best
km.out$cluster
table(km.out$cluster) # First 25 are clustered in cluster 2, second 25 in  cluster 1-exactly what we expected
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

#now with k=3 clusters (assuming that we didn't know our data had 2 distinct classes)
set.seed(4)
km.out=kmeans(x,3,nstart=20)  
km.out
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

km.out$cluster+1
km.out$cluster

set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out
par(mfrow=c(1,2))
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3, nstart=1", xlab="", ylab="", pch=20, cex=2)
km.out$tot.withinss  #total within cluster sum of squares (what K-means algorithm is minimizing)
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3, nstart=20", xlab="", ylab="", pch=20, cex=2)

par(mfrow=c(1,1))

# Hierarchical Clustering
#hclust() function of default stats package
#dist() calculates 50*50 euclidean distance of 50 observations that we created before


y <- read.csv("Euclidean Distance.csv")

dist(x)

table(dist(x))
write.csv("Euclidean Distance.csv") <- dist(x)
hc.complete=hclust(dist(x), method="complete")

hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
cutree(hc.complete, 2)  #cut to obtain 2 clusters
cutree(hc.average, 2)
cutree(hc.single, 2)  #compared to previous two linkages, what is wrong with this one?
                      #all but 16th obs are in one cluster, 16th is in the 2nd cluster
cutree(hc.single, 4)  #cut to obtain 4 clusters

par(mfrow=c(1,1))
xsc=scale(x)  #if ew want to scale the data before clustering
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")
cutree(hclust(dist(xsc)), 2)

#if we want to use correlation based idstanceinstead of euclidean distance 
# it would only make sense if we have >=3 variables,
# because correlation of two obs. with 2 variables will always be -1 or 1 (linear relationship)
y=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(y)))

dd
#correlation matrix is square 30*30, as.dist() converts it 
                        #into a distance vector to have output like dist() produces
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")

######################################
#PRINCIPAL COMPONENT ANALYSIS
######################################
library(ISLR)
data("USArrests")


attach(USArrests)
head(USArrests)

data_new<-scale(USArrests) #scale variables to mean=0, stdev=1
head(data_new)
summary(data_new)

data_new["Florida",]
pca.out<-prcomp(data_new)  #use PCA on standardized data
#or alternatively, you can scale it in the pca function
pca.out<-prcomp(USArrests, scale=TRUE)
names(pca.out)

pca.out$rotation  #principal component loadings (for this data, 4 distinct principal component)
pca.out$x   #principal component scores for 50 states (multiplication of variables with PC loadings)

biplot(pca.out,cex=0.5)

cor(USArrests)
cor(pca.out$x)

#variance explained by each PC
pca.var=pca.out$sdev^2

var.explained=pca.var/sum(pca.var)
var.explained  #62%of the variance is explained by pc1, 24.7% by pc2....

