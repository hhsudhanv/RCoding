##HW5 - Nandhini Varadharajan ##

#d.	Now, using R, apply hierarchical clustering with Single Linkage now. 
obs = matrix(nrow=5,ncol=5)
lower.tri(obs,diag=0)
diag(obs) = 0
obs[lower.tri(obs)] = c(10,4,5,12,8,6,11,7,3,9)
obs
ED = as.dist(obs)
ED
hc.single = hclust(ED, method="single")
hc.complete = hclust(ED, method ="complete")
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
cutree(hc.single, h = 3)
cutree(hc.complete,2)
cutree(hc.complete,h = 3)

# 2.b) Perform K-Means clustering with R using 20 initial assignment
x1=c(1,1,0,5,6,4)
x2=c(4,3,4,1,2,0)
obs.Km= matrix(nrow=6, ncol=2)
obs.km= cbind(x1,x2)
obs.km
km.out=kmeans(obs.km,2,nstart=20)
km.out

plot(obs.km, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)
