library(ISLR)
summary(Hitters) # 'describe' baseball database

## There are some missing values. Eg. The salary variable has 59 missing values denoted as 'NA'
## Predict salaries of players

Hitters = na.omit(Hitters) # delete any row that contains a missing value
with(Hitters, sum(is.na(Salary))) # confirm there are no missing values in the salary variables
sum(is.na(Hitters)) # confirm there are no missing values in the entire dataset


#BEST SUBSET REGRESSION
#----------------------

library(leaps)
regfit.full = regsubsets(Salary~., data=Hitters) # fit best subsets
summary(regfit.full) # by default, shows subset upto size 8

regfit.full = regsubsets(Salary~., data=Hitters, nvmax=19) # fit best subset upto size 19 (the size of the predictors)
summary(regfit.full)

reg.summary = summary(regfit.full) 

# evaluate the best subset model (by estimating prediction error from training error -- Cp, BIC, AIC, ADr2)
names(reg.summary) # contains r-squared, residual sum of squares, adjusted r square, Cp, BIC to help us select the best model
plot(reg.summary$cp, xlab="Number of Variables", ylab ="Cp") # plot cp component of reg.summary; pick a model with the smallest Cp

which.min(reg.summary$cp) # identify the index of the smallest element of the Cp component; aka. the subset model with the lowest Cp
points(10, reg.summary$cp[10], pch=20, col='red') # highlight the model


## Alternative plot for Best Subset Regression

plot(regfit.full, scale='Cp') # plots are pattern picture; black square='variable are in', white='variables are out'
coef(regfit.full, 10) # print coefficients of the 10 variables in the model



#FOWARD STEPWISE SELECTION
#------------------------

regfit.fwd=regsubsets(Salary~., data=Hitters, nvmax=19, method="forward") # fit Forward Step-wise Selection
reg.summary = summary(regfit.fwd)
reg.summary

plot(reg.summary$cp, xlab="Number of Variables", ylab ="Cp")
which.min(reg.summary$cp) # identify the index of the smallest element of the Cp component; aka. the subset model with the lowest Cp
points(10, reg.summary$cp[10], pch=20, col='red') # highlight the model

## Alternatively,
plot(regfit.fwd, scale="Cp")



# Estimating Test Error: VALIDATION SET METHOD (Directly)
#------------------------------------------------------

dim(Hitters)
set.seed(1)
train=sample(seq(263), 180, replace=FALSE) # create use 2/3 (180) observations for training set
length(train) # shows length of training set

##Use forward selection on the training set
regfit.fwd = regsubsets(Salary~., data=Hitters[train,], nvmax=19, method="forward") # fit Forward Step-wise Selection


##set-up some vectors to record the test error because there is no 'predict' method for 'regsubset'
val.errors=rep(NA,19) # create vector for the 19 predictors/subsets
x.test = model.matrix(Salary~., data=Hitters[-train,]) # create matrix that correspond to the test set predictors


for(i in 1:19){
  coefi = coef(regfit.fwd, id=i)   # extract coefficients for the model of size 1:19)
  pred = x.test[,names(coefi)] %*% coefi # create the prediction formula
  val.errors[i] = mean((Hitters$Salary[-train]-pred)^2) # create the MSE formula; evaluate prediction using MSE
}

plot(sqrt(val.errors), ylab="RMSE", ylim=c(300,500), pch=19, type="b") # plot RMSE
points(sqrt(regfit.fwd$rss[-1]/180), col="red", pch=19, type="b") # plot RSS on the training set on the same plot
legend("topright", legend=c("Training", "Validation"), col=c("red", "black"), pch=19)



# EXTRA
## WRITE "predict" METHOD FOR "regsubset" 

predict.regsubsets=function(object, newdata, id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  mat[,names(coefi)] %*% coefi
}



# Estimating Test Error: CROSS-VALIDATION SET METHOD (Directly)
#-------------------------------------------------------------
set.seed(11)
folds=sample(rep(1:10, length=nrow(Hitters))) # k=10 fold; create a vector 1-10 of length of the nrows; observations are chosen at random, while each fold is of equal size
table(folds) # show fold

cv.errors = matrix(NA, 10, 19) # create a matrix for the 10 fold and 19 subsets for cv errors

## create double loop
for(k in 1:10){
  best.fit=regsubsets(Salary~., data=Hitters[folds!=k, ], nvmax=19, method="forward") # fit "regfit.fwd" (Forward Step-wise Selection)
  for(i in 1:19){
    pred=predict(best.fit, Hitters[folds==k, ], id=i) # predict
    cv.errors[k,i]=mean( (Hitters$Salary[folds==k]-pred)^2) # find MSE of each fold
  }
}

##plot
rmse.cv = sqrt(apply(cv.errors,2,mean)) # find RMSE of predictions; first find average down the column and use square root to find the RMSE
plot(rmse.cv, pch=19, type="b")



# RIDGE REGRESSION & LASSO
#------------------------

library(glmnet) # package does not use model formula language

x = model.matrix(Salary~.-1, data=Hitters) # set up x (predictors)
y = Hitters$Salary # set up y (response)


### fit ridge regression
fit.ridge = glmnet(x, y, alpha=0) 
plot(fit.ridge, xvar="lambda", label=TRUE) # plot coefficients of the predictors; recall that the ridge regressions model is penalized by the sum of squares of the coefficients multiplied by lambda

cv.ridge = cv.glmnet(x, y, alpha=0) # "cv.glmnet" is a built-in function used to perform cross-validation, with k=10 as default
plot(cv.ridge) # plot MSE
coef(cv.ridge) # Show coefficient from cv.ridge

### fit lasso
fit.lasso = glmnet(x,y) # fit lasso using alpha=1 by default
plot(fit.lasso, xvar="lambda", label=TRUE) # plot coefficients of the predictors; recall that the ridge regressions model is penalized by the absolute value of the coefficients multiplied by lambda
plot(fit.lasso, xvar="dev", label=TRUE) # deviance is same as r^2 in this context

cv.lasso = cv.glmnet(x,y) # perform cross-validation with k=10 and alpha=1 by default
plot(cv.lasso) # plot MSE
coef(cv.lasso) # Show coefficient from cv.lasso (only shows coefficient corresponding to the best model). Recall that fit.lasso contains the whole path of coefficient (roughly 100 coefficient vectors, depending on index by different values of lambda)


## EXTRA
## Selecting LAMBDA for LASSO using Validation method

lasso.tr=glmnet(x[train, ], y[train]) # fit lasso model using the training/validation divisions stated earlier; also, this tries to fit 100 values of lambda, but stops it nothing changes
lasso.tr

pred=predict(lasso.tr, x[-train,]) # predict test/validation data
dim(pred) # gives number of observations in the validation set Vs total values of lambda

rmse = sqrt(apply((y[-train]-pred)^2, 2, mean)) # compute RSME of predictions
plot(log(lasso.tr$lambda), rmse, type="b",xlab="Log(lambda)")

lam.best = lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr, s=lam.best) # Show coefficient from lasso or "predict(lasso.tr, s = lam.best, type = "coefficients")" prints the same results
