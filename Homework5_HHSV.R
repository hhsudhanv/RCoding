# Homework - 5 - Unsupervised Learning
# Hari hara sudhan Venkateswaran 

#1 (d) Hierarchial Clustering

y.data <- matrix(nrow = 5, ncol = 5)
y.data
lower.tri(y.data,diag = 0)
y.data[lower.tri(y.data)] <- c(10,4,5,12,8,6,11,7,3,9)
y.data
y <- as.dist(y.data)
y
hc.single <- hclust(y,method = "single")
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

hc.complete <- hclust(y,method = "complete")
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)

cutree(hc.complete, h = 12)
cutree(hc.complete, h = 10)

cutree(hc.complete, h = 3)
cutree(hc.single, h = 3)


# 2 (b) K means clustering

x1=c(1,1,0,5,6,4)
x2=c(4,3,4,1,2,0)
km.data = matrix(nrow=6, ncol=2)
km.data = cbind(x1,x2)
km.data
km.out=kmeans(km.data,2,nstart=20)
plot(km.data, col=(km.out$cluster+1), main="K-Means Clustering with K=2", xlab="X1", ylab="X2", pch=18, cex=3)