require(ISLR) # "require" imports library and also returns true or false if library does not exist
require(boot) # "require" imports library and also returns true or false if library does not exist

?cv.glm # cross validation package for general linear models (glm)
plot(mpg~horsepower, Auto)


## LOOCV
glm.fit = glm(mpg~horsepower, data=Auto) # trains a linear model; recall that we would be training a logistic regression model instead if the argument family=binomial
cv.glm(Auto,glm.fit)$delta # runs LOOCV & uses brute force; running this could be slow
#"delta" is the cross validation prediction error; gives two numbers "raw result" and the "bias corrected version of the result"; the bias correction result deals with that fact that the training data is smaller that the whole dataset we intend to get prediction error for


## Write a function to use formula (5.2) on page 180 in the textbook
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
} #lm.influence is a post processor for lm fit and it would extract the element "h" from it to give the diagonal element h(ii) as seen in the formula

loocv(glm.fit) # call function


## Write function to compute error
## fit polynomials of different degrees to our data in line 5 bcos the data is non-linear

cv.error = rep(0,5) # create a vector to collect the errors
degree=1:5 # create variable which takes polynomial degrees from 1-5

for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit) 
}

plot(degree, cv.error, type="b") # plot error against degree for LOOCV 


## 10-fold CV
cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}

lines(degree, cv.error10, type="b", col="red") # plot error against degree for K=10 CV



