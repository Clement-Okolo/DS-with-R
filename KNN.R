library(class)
?knn
attach(Smarket) # attach a data frame to make its variables available in the workspace by name

Xlag = cbind(Lag1,Lag2) # make a matrix of Lag1 & Lag2
class(Xlog)

train = Year<2005 # define train set

knn.pred = knn(Xlag[train,], Xlag[!train,], Direction[train], k=1)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train]) # measures the performance of the Knn classifier

