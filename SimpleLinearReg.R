library(MASS)
library(ISLR)

### Simple Linear Regression

names(Boston) # names variables in Boston data set
?Boston # provides more details

plot(medv~lstat, Boston) # plot two variables

fit1=lm(medv~lstat, Boston) # train linear model
fit1 # print call and coefficients
summary(fit1)

abline(fit1, col="red")  #show model graphically

names(fit1) # internal components

confint(fit1) # confidence interval

# predict medv using lstat =c(5,10,15). Print confidence interval.
predict(fit1, data.frame(lstat=c(5,10,15)), interval="confidence")
