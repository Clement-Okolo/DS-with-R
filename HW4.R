library(ISLR)     # load library
attach(College)   # makes College variables available by name

set.seed(10)


train = sample(dim(College)[1], size = 0.80*dim(College)[1], replace=FALSE)	# set 80% of data set as training set
test = -train 		# set remaining 20% of data set as tesr set

training.set = College[train, ]	  # split training set
test.set= College[test, ]		    # split test set

dim(College)          # show dimension of entire data set
dim(training.set)     # show dimension of training set
dim(test.set)         # show dimension of test set


## Fit Linear model on training set

fit.lm = lm(Apps~., data=training.set) # fit linear model on training set using least squares 
pred.lm = predict(fit.lm, test.set)    # make prediction on test set
error.lm = mean((test.set$Apps - pred.lm)^2) # calculate test error using Mean Squared Error
error.lm
rmse.lm = sqrt(error.lm)               # find RMSE of linear model
rmse.lm


### Fit a ridge regression model on the training set


library(glmnet)             # load package

x.train = model.matrix(Apps~., data=training.set) # set up x.train (predictors)
y.train = training.set$Apps                     # set up y.train (response)

x.test = model.matrix(Apps~., data=test.set)    # set up x (predictors)
y.test = test.set$Apps
  
# model selection using ridge regression
fit.ridge = glmnet(x.train, y.train, alpha=0)   # fit ridge regression

cv.ridge = cv.glmnet(x.train, y.train, alpha=0) # generate  λ by cross-validation, with k=10 by default
cv.ridge
bestlambda = cv.ridge$lambda.min                # select minimum lambda model

pred.ridge = predict(fit.ridge, s=bestlambda, newx=x.test)

error.ridge = mean((y.test - pred.ridge)^2)
error.ridge
rmse.ridge = sqrt(error.ridge)                 # find RMSE of ridge regression model
rmse.ridge


####Fit a lasso model on the training set, with λ chosen by cross-validation
# model selection using lasso model

fit.lasso = glmnet(x.train, y.train, alpha=1)   # fit lasso model

cv.lasso = cv.glmnet(x.train, y.train, alpha=1) # generate  λ by cross-validation, with k=10 by default
cv.lasso
bestlambda = cv.lasso$lambda.min                # choose minimum lambda model

pred.lasso = predict(fit.lasso, s=bestlambda, newx=x.test) # make prediction on the test set 

error.lasso = mean((y.test - pred.lasso)^2)
error.lasso
rmse.lasso = sqrt(error.lasso)                  # find RMSE of lasso model
rmse.lasso

coef(fit.lasso, s=bestlambda)                   # show non-zero coefficient estimates



#### predict per capital crime rate in the Boston data set. 


library(MASS)      # load library
attach(Boston)     # makes College variables available by name

set.seed(15)

train = sample(dim(Boston)[1], size = 0.80*dim(Boston)[1], replace=FALSE)	# set 80% of data set as training set
test = -train 				                           # set remaining 20% of data set as test set

training.set = Boston[train, ]	               	 # split training set
test.set= Boston[test, ]		                     # split test set

dim(Boston)                                      # entire data set
dim(training.set)                                # training set
dim(test.set)                                    # test set




#### LASSO



library(glmnet) # load package
x.train = model.matrix(crim~., data=training.set) # set up predictors from the training set
y.train = training.set$crim                       # set up response from the training set

x.test = model.matrix(crim~., data=test.set)      # set up predictors from the test set
y.test = test.set$crim                            # set up response from the test set

fit.lasso = glmnet(x.train, y.train, alpha=1)     # fit lasso model

cv.lasso = cv.glmnet(x.train, y.train, alpha=1)   # generate  λ by cross-validation, with k=10 by default
cv.lasso
bestlambda = cv.lasso$lambda.min                  # choose minimum lambda model

pred.lasso = predict(fit.lasso, s=bestlambda, newx=x.test)  # make prediction on the test set 
error.lasso = mean((y.test - pred.lasso)^2)       # find test error using Mean Squared Error
error.lasso
rmse.lasso = sqrt(error.lasso)                    # find RMSE of lasso model
rmse.lasso

coef(fit.lasso, s=bestlambda)                     # show the non-zero coefficient estimates


#### RIGDE
# model selection using ridge regression



fit.ridge = glmnet(x.train, y.train, alpha=0)     # fit ridge regression

cv.ridge = cv.glmnet(x.train, y.train, alpha=0)   # generate  λ by cross-validation, with k=10 by default
cv.ridge
bestlambda = cv.ridge$lambda.min                  # choose minimum lambda model

pred.ridge = predict(fit.ridge, s=bestlambda, newx=x.test) # make prediction on the test set
error.ridge = mean((y.test - pred.ridge)^2)       # find test error using Mean Squared Error
error.ridge
rmse.ridge = sqrt(error.ridge)                    # find RMSE of ridge regression model
rmse.ridge

coef(fit.ridge, s=bestlambda)                  	  # show the non-zero coefficient estimates

#### BEST SUBSET
#pred.sub = predict(regfit.full, s=bestcp, newx=test.set)
#Estimating Test Error -- using CV

library(leaps)                                     # load library
set.seed(15)                                       # set random seed
folds=sample(rep(1:10, length=nrow(Boston)))       # create 10 folds
table(folds)                                       # show fold
cv.errors = matrix(NA, 10, 13) # create a matrix store the cross validation errors for the 13 subsets


predict.regsubsets=function(object, newdata, id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  mat[,names(coefi)] %*% coefi
}                                                 # write method to predict for regsubset


for(k in 1:10){0
  best.fit=regsubsets(crim~., data=Boston[folds!=k, ], nvmax=13, method="forward")
  for(i in 1:13){
    pred=predict(best.fit, Boston[folds==k, ], id=i)
    cv.errors[k,i]=mean( (Boston$crim[folds==k]-pred)^2) # find MSE of each fold
  }
} # write a double for loop to fit, predict, and calculate cross-validation errors

mse.cv = apply(cv.errors, 2, mean)                # find test error using Mean Squared Error
mse.cv
min(mse.cv)                                       # find minimum test error using Mean Squared Error


rmse.cv = sqrt(apply(cv.errors,2,mean))           # find RMSE
rmse.cv
min(rmse.cv)                                      # find minimum RMSE
plot(rmse.cv, pch=19, xlab = "Number of Features", ylab="RMSE", type="b") # plot


#Using Validation Method instead of Cross-validation
------------------------------------------------

library(glmnet) # load package
x.train = model.matrix(crim~., data=training.set) # set up predictors from the training set
y.train = training.set$crim                       # set up response from the training set

x.test = model.matrix(crim~., data=test.set)      # set up predictors from the test set
y.test = test.set$crim                            # set up response from the test set


regfit.fwd = regsubsets(crim~., data=training.set, nvmax=13, method="forward") # fit Forward Step-wise Selection
val.errors=rep(NA,13) # create vector for the 19 predictors/subsets


for(i in 1:13){
  coefi = coef(regfit.fwd, id=i)   # extract coefficients for the model of size 1:19)
  pred = x.test[,names(coefi)] %*% coefi # create the prediction formula
  val.errors[i] = mean((y.test -pred)^2) # create the MSE formula; evaluate prediction using MSE
}

val.errors
min(val.errors)

rmse.val = sqrt(val.errors)
min(rmse.val)

