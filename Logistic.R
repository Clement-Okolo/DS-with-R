require(ISLR)
names(Smarket) # prints the names of the variables in the stock market data set
summary(Smarket)
?Smarket
pairs(Smarket, col=Smarket$Direction) #plot all variables and use Direction as color

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, Smarket, family=binomial) # General Linear Model (glm) - can fit different kinds of linear models. The binomila family allows glm to fit logistic regression model
summary(glm.fit)

glm.probs = predict(glm.fit, type="response") # prediction on the training data used to fit the model; recall that we used the entire dataset as training dataset
glm.probs[1:5] # first 5 vectors of probabilities

glm.probs=ifelse(glm.probs>0.5, "Up", "Down") # this turns the probabilities into classifications by threshold in at 0.5. glm.probs>0.5 call it Up if not call it "Down"

attach(Smarket) # makes variables of Smarket available by name

table(glm.probs, Direction)
mean(glm.probs==Direction) # measures classification performance
# the mean is ~0.5; since we predicted on the training data, we may have overfitted the training data

## Divide data set into train and test sets
train = Year<2005
glm.fit1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, Smarket, family=binomial, subset=train) # train 

glm.probs = predict(glm.fit1, Smarket[!train,], type="response") # predict
  
glm.pred=ifelse(glm.probs>0.5, "Up", "Down")
Direction.2005 = Smarket$Direction[!train] # test set 
table(glm.pred, Direction.2005)

mean(glm.pred==Direction.2005) # result shows an accuracy of 48%; this means it lower than 50% previously recorded and obviously over fitting the training data

## Using a smaller model to counter Over fitting

glm.fit3 = glm(Direction~Lag1+Lag2, Smarket, family=binomial, subset=train)

glm.probs = predict(glm.fit3, Smarket[!train,], type="response")
glm.pred = ifelse(glm.probs>0.5, "Up", "Down")

table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005) # this shows an improvement from 0.48 to 0.56

summary(glm.fit3) # non of the predictors showed any significance


####################
####################


