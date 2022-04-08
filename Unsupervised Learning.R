# PRINCIPAL COMPONENT ANALYSIS (PCA)
# ==============================


dimnames(USArrests) # unlike 'rownames' & 'colnames', 'dimnames' operates on both the rows and columns
apply(USArrests,2, mean) # find mean of the variables; recall 1 - means apply on rows, 2 - means apply on columns
apply(USArrests,2, var) # find variance of the variables
dim(USArrests)

# PCA is about variance so the mean of the variables shouldn't play a role, but the variances will."
# PCA looks for linear combination that maximizes the variance
# The variance of the individual variable is different because the variables are measured in different units.
# In this case, the next step is to standardize the variables before we do PCA. The result will be that the variables will have unit variance

#as.data.frame(scale(USArrests, scale=TRUE)) # This is an alternative away to scale the data before finding the PCA
pca.out = prcomp(USArrests, scale=TRUE) # standardize the variables and find PCA
pca.out # prints the Standard Deviations and PCA loadings

biplot(pca.out, scale=0) # Plot the first two PCA -- PCA1 & PCA2; the biplot has both the PCA and loading scores in one plot
biplot(pca.out, scale=0, cex=0.6) # reduce the font



# K-Means Clustering
# =================

# Recall K-Means Clustering works in any dimension, but it better to work with 2-D so we can plot pictures

set.seed(101)
x = matrix(rnorm(100*2), 100, 2) # use normal distribution to create a random normal matrix with 100 rows an 2 columns; 100*2 means 100 observations for each of the 2 columns

# cloud of Gaussian; create 4 clusters by generating some means and displace some of the gaussians by shifting their means around differently for each of the 4 clusters
xmean = matrix(rnorm(8, sd=4), 4, 2) # create another random normal matrix (4 rows, 2 columns) with 8 observations (because we'll have 4 clusters, each with two means) & SD of 4 because we want the means to shift around more than the actual data did
which=sample(1:4, 100, replace=TRUE) # decide which row gets which mean by picking 100 random sample from the numbers 1-4 with replacement
x = x + xmean[which,] # add appropriate mean to appropriate rows; this will produce a 100 row matrix with 2 columns

plot(x, col=which, pch=19) # plot

## We know the 'true'  cluster IDs, but we won't tell that to the Kmeans algorithm
km.out = kmeans(x, 4, nstart=15) # 4 clusters, random start 15 (recall Kmeans clustering starts with a random start; bad random start can result in bad solution)
km.out
plot(x, col=km.out$cluster, cex=2, pch=1, lwd=2) # plot clusters IDs found by k-means algorithm
points(x, col=which, pch=19) # plot known clusters IDs
#points(x, col=c(4,3,2,1)[which], pch=19) #reassign colors


# Hierarchical Clustering
# ======================

# We'll use the same data from kmeans clustering example above

hc.complete = hclust(dist(x), method="complete") # find hierarchical clustering using largest pairwise distance of x
plot(hc.complete) # plot dendrogram

# or

hc.single = hclust(dist(x), method="single") # find hierarchical clustering using smallest pairwise distance of x; aka. single linkage clustering
plot(hc.single)  # plot dendrogram

# or

hc.average = hclust(dist(x), method="average") # find hierarchical clustering using smallest pairwise distance of x; aka. single linkage clustering
plot(hc.average)  # plot dendrogram

# Lets compare our results with the actual clusters in the data

hc.cut = cutree(hc.complete, 4) # produce a vector of cluster assignment
table(hc.cut, which)

table(hc.cut, km.out$cluster) # compare with kmeans clustering

plot(hc.complete, labels=which) # label according to original cluster assignment

