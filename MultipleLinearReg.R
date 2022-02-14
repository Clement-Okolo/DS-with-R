library(MASS)
library(ISLR)

### Multiple Linear Regression
names(Boston)

fit2=lm(medv~lstat+age, Boston) # two predictors -- lstat and age
summary(fit2)

fit3 =lm(medv~., Boston) #all variables in data set as predictors EXCEPT medv which is the response variable
