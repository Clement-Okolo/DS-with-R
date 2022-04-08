data(USArrests) # load USArrest data set

hc.complete = hclust(dist(USArrests), method="complete") # cluster the states
plot(hc.complete) # show dendrogram

#cut dendrogram @ k=3
hc.cut = cutree(hc.complete, 3) # produce a vector of cluster assignment
hc.cut

# scale
USArress_scaled = as.data.frame(scale(USArrests, scale=TRUE)) # Scale data set
sqrt(apply(USArress_scaled,2, var)) # variables with standard deviation of one

hc.complete = hclust(dist(USArress_scaled), method="complete") # cluster the states
plot(hc.complete) # show dendrogram



# Ques 2


set.seed(10)                                # set random seed

data = matrix(rnorm(60*50), 60, 50)         # stimulate data set
data_mean = matrix(rnorm(150, sd=3), 3, 50) # create mean shift

c1 = rep(1, 20)            # generate class 1
c2 = rep(2, 20)            # generate class 2
c3 = rep(3, 20)            # generate class 3

which= as.integer(rbind(c1,c2,c3))
table(which)

data = data + data_mean[which,]  # add mean shit and the three class to the data
dim(data)                        # show dimension of data
plot(data, col=which, pch=19, main="Stimulated Dataset")    # plot data with three classes




# 2b
pca.out = prcomp(data) # find PCA
summary(pca.out)
plot(pca.out$x[, 1:2], col=which, main="First Two Principal Components", pch=19)

# 2c

set.seed(15)

km.out = kmeans(data, 3, nstart=15) # perform k = 3
k3.labels = km.out$cluster
true.labels = which
table(k3.labels, true.labels) # compare to true class labels

# 2 d

set.seed(10)

km.out = kmeans(data, 2, nstart=15) # perform k = 2
k2.labels = km.out$cluster
true.labels = which
table(k2.labels, true.labels) # compare to true class labels

# 2 e

set.seed(1)

km.out = kmeans(data, 4, nstart=15) # perform k = 4
k4.labels = km.out$cluster
true.labels = which
table(k4.labels, true.labels) # compare to true class labels

# 2 f

set.seed(15)

pca = pca.out$x[, 1:2]       # first two principal component score vectors
dim(pca)                     # show dimension

km.out = kmeans(pca, 3, nstart=15) # perform k = 3
k3.labels = km.out$cluster
true.labels = which
table(k3.labels, true.labels) # compare to true class labels


# 2 g

# scale

data_scaled = as.data.frame(scale(data, scale=TRUE)) # scale data set
sqrt(apply(data_scaled,2, var))   # each variable with standard deviation of one


set.seed(15)


km.out = kmeans(data_scaled, 3, nstart=15) # perform k = 3 on scaled data
k3.labels = km.out$cluster
true.labels = which
table(k3.labels, true.labels) # compare to true class labels

plot(pca.out$x[, 1:2], col=which, main="First Two Principal Components", pch=19)
plot(km.out$cluster[, 1], col=which, main="kmeans", pch=19)



#plot(data, col=km.out$cluster, cex=2, pch=1, lwd=2) # plot clusters IDs found by k-means algorithm
#points(data, col=which, pch=19) # plot known clusters IDs

#biplot(pca.out, scale=0) # Plot the first two PCA -- PCA1 & PCA2; the biplot has both the PCA and loading scores in one plot
#biplot(pca.out, scale=0, cex=0.6)
