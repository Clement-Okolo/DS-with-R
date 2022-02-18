library(ISLR)

fit = lm(mpg~horsepower, Auto)
summary(fit)

plot(mpg~horsepower, Auto, main= "mpg vs horsepower") # plot response and predictor

abline(fit, col="red") # display least square regression line


par(mfrow = c(1, 1)) #  using 2 by 2 layout
plot(fit) # diagnostic plots of the least squares regression fit


plot(Auto)

str(Auto)

names(Auto[-9])


fit2 <- lm(mpg ~ . - name, data = Auto) # all variables except name
summary(fit2) # print results


par(mfrow = c(2, 2))
plot(fit2)



fit3 <- lm(mpg~displacement*weight + year*origin, Auto[-9])
summary(fit3)



par(mfrow = c(2, 2))  #  using 2 by 2 layout
plot(log(Auto$year), Auto$mpg, xlab="log(year)", ylab="mpg")
plot(sqrt(Auto$year), Auto$mpg)
plot((Auto$year)^2, Auto$mpg)

data(Carseats)
fit3 <- lm(Sales~Price + Urban + US, Carseats)
summary(fit3)

str(Carseats$US)

fit4 <- lm(Sales~Price + US, Carseats)
summary(fit4)