library(ISLR)
attach(Default) # makes Auto variables available by name


# fit logistic regression

glm.fit <- glm(default~income+balance, Default, family="binomial") # fit logistic regression
summary(glm.fit)


#split data

train = sample(dim(Default)[1], size = 0.50*dim(Default)[1], replace=FALSE)	# set 50% of data set as training set
val_set = -train 				# set remaining 50% of data set as validation set
training_data = Default[train, ]		# split training set

val_data= Default[val_set, ]		# split validation set

#dim(Default) # dimension of entire data set
#dim(training_data) # dimension of training set
#dim(val_data) # dimension of validation data


# Fit a multiple logistic regression model 

glm.fit <- glm(default~income+balance, Default, family="binomial", subset = train)
#summary(glm.fit)


# Predict default status for each individual in the validation set

glm.probs = predict(glm.fit, val_data, type="response") # compute posterior prob
glm.pred = ifelse(glm.probs>0.5, "Yes", "No") #  classifying the individual to the default category if the posterior probability is greater than 0.5


#Compute the validation set error

table(glm.pred, val_data$default) #accuracy
mean(glm.pred != val_data$default) # validation error


# logistic regression model
# predicts the probability of default using income, balance, and a dummy variable for student

dummy_student = ifelse(student=="Yes", 1, 0) # create dummy variable for student
head(Default)
Default = cbind(Default, dummy_student)
head(Default)

train = sample(dim(Default)[1], size = 0.50*dim(Default)[1], replace=FALSE)	# set 50% of data set as training set
val_set = -train 				# set remaining 50% of data set as validation set
training_data = Default[train, ]		# split training set
val_data= Default[val_set, ]		# split validation set

glm.fit <- glm(default~income+balance+dummy_student, Default, family="binomial", subset = train)
glm.probs = predict(glm.fit, val_data, type="response") # compute posterior prob
glm.pred = ifelse(glm.probs>0.5, "Yes", "No") #  classifying the individual to the default category if the posterior probability is greater than 0.5
table(glm.pred, val_data$default) 
mean(glm.pred != val_data$default) # validation error

## We will now perform cross-validation on a simulated data set.
## Generate a simulated data set as follows:

require(boot)
set.seed(10)
x=rnorm(100)
y=x-2*x^2+rnorm (100) 

#plot(x,y)
#par(new = TRUE)
#curve(x - 2*x^2, xlab = "", ylab = "", col = "red")


xy_data = data.frame(x,y) # create single data containing x and y
head(xy_data)

# compute the LOOCV errors from Y = β0 + β1X + Ɛ
set.seed(10)
glm.fit = glm(y~x, data=xy_data) # trains a linear model
cv.glm(xy_data,glm.fit)$delta # compute the LOOCV errors from Y = β0 + β1X + Ɛ
#gives two umbers "raw result" and the "bias corrected version of the result"; the bias correction result deals with that fact that the training data is smaller that the whole dataset we intend to get prediction error for
summary(glm.fit)$coef


# compute the LOOCV errors from Y = β0 + β1X + β2X2 + Ɛ
set.seed(10)
glm.fit2 = glm(y~poly(x, 2), data=xy_data) # second degree polynomial
cv.glm(xy_data,glm.fit2)$delta
summary(glm.fit2)$coef


# compute the LOOCV errors from  Y = β0 + β1X + β2X2  + β3X3 + Ɛ
set.seed(10)
glm.fit3 = glm(y~poly(x, 3), data=xy_data) # second degree polynomial
cv.glm(xy_data,glm.fit3)$delta

summary(glm.fit3)$coef




# compute the LOOCV errors from Y = β0 + β1X + β2X2  + β3X3 + β4X4 + Ɛ
set.seed(10)
glm.fit4 = glm(y~poly(x, 4)) # second degree polynomial
cv.glm(xy_data,glm.fit4)$delta

summary(glm.fit4)$coef



###################
fit6=lm(medv~lstat + I(lstat^2), Boston) # second degree polynomial a.k.a quadratic fit
fit7 = lm(medv~poly(lstat, 4)) # fourth degree polynomial

