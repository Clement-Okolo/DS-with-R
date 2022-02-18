library(MASS)
library(ISLR)

### Multiple Linear Regression
names(Boston)

fit2=lm(medv~lstat+age, Boston) # two predictors -- lstat and age; diff from lstat x age in line 24
summary(fit2)

fit3 =lm(medv~., Boston) #all variables in data set as predictors EXCEPT medv which is the response variable
summary(fit3)

par(mar=c(1,1,1,1)) # adjust graph margin size
par(mfrow=c(2,2)) # plot multiple graphs using 2 by 2 layout

plot(fit3)

# update a linear model
fit4=update(fit3, ~.-age-indus) # remove predictor that are not significant based on the p-value and train the model again
summary(fit4)


### NON LINEAR TERMS AND INTERACTIONS

# INTERACTIONS

fit5=lm(medv~lstat*age, Boston) # different from lstat + age described in line 7
summary(fit5)


# NONLINEAR -- fitting polynomials 

fit6=lm(medv~lstat + I(lstat^2), Boston) # second degree polynomial a.k.a quadratic fit
summary(fit6)



#plot
attach(Boston) # named variables in Boston are available in our data space
par(mfrow=c(1,1))

plot(medv~lstat) # plane plot
points(lstat, fitted(fit6), col="red", pch=20) #include fitted values (model) from quadratic fit into the plane plot


# NONLINEAR -- easier way of fitting polynomials using poly()

fit7 = lm(medv~poly(lstat, 4)) # fourth degree polynomial
points(lstat, fitted(fit7), col="blue", pch=20)

# pch -- plotting characters
plot(1:20, 1:20, pch=1:20, cex=2) # show available pch in R


### QUALITATIVE PREDICTORS
fix(Carseats)
names(Carseats)

fit1= lm(Sales~. + Income:Advertising + Age:Price, Carseats) # multiple linear regression with two interactions
summary(fit1)

contrasts(Carseats$ShelveLoc) # how R will code the qualitative variable when put in a linear model

### ---- recall one dummy variables for TWO LEVEL factors like gender is 0 & 1
### ---- THREE LEVEL FACTORS like good, bad, and medium is therefore 00, 10, 01 (two dummy variables) 


### Write R Functions to fit a regression model and make a plot

regplot = function(x, y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit, col="red")
}                         # write function

attach(Carseats)
regplot(Price, Sales)     # call function


### Add more features to the function "..." are unnamed arguments and it gives room to add new arguments without editing/writing a new function

regplot = function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
} #


regplot(Price, Sales, xlab="Price", ylab="Sales", col="blue", pch=20) # call function with additional arguments for labels and color for points
