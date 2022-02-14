library(MASS)
library(ISLR)

### Simple Linear Regression

names(Boston) # names variables in Boston data set
?Boston # provides more details

plot(medv~lstat, Boston) # plot two variables

fit1=lm(medv~lstat, Boston) # train linear model
fit1 # print call and coefficients
