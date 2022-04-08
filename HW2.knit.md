
<!-- rnb-text-begin -->

---
title: "Classification"
output: html_notebook
---

1. In this problem, you will develop a model to predict whether a given car gets high or low gas mileage based on the Auto data set.

(a) Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its median, and a 0 if mpg contains a value below its median. You can compute the median using the median() function. Note you may find it helpful to use the data.frame() function to create a single data set containing both mpg01 and the other Auto variables.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubGlicmFyeShJU0xSKVxuYXR0YWNoKEF1dG8pICMgbWFrZXMgQXV0byB2YXJpYWJsZXMgYXZhaWxhYmxlIGJ5IG5hbWVcblxuYGBgIn0= -->

```r
library(ISLR)
attach(Auto) # makes Auto variables available by name

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubXBnLm1lZGlhbiA9IG1lZGlhbihBdXRvJG1wZykgIyBjb21wdXRlIG1lZGlhblxuXG5tcGcwMSA9IGlmZWxzZShtcGc+bXBnLm1lZGlhbiwgMSwgMCkgIyBjcmVhdGUgYmluYXJ5IHZhcmlhYmxlXG5cbkF1dG8gPSBkYXRhLmZyYW1lKEF1dG8sIG1wZzAxKSAjIGNyZWF0ZSBhIHNpbmdsZSBkYXRhIHNldCBjb250YWluaW5nIGJvdGggbXBnMDEgYW5kIHRoZSBvdGhlciBBdXRvIHZhcmlhYmxlc1xuXG5oZWFkKEF1dG8pXG5cbmBgYCJ9 -->

```r
mpg.median = median(Auto$mpg) # compute median

mpg01 = ifelse(mpg>mpg.median, 1, 0) # create binary variable

Auto = data.frame(Auto, mpg01) # create a single data set containing both mpg01 and the other Auto variables

head(Auto)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


(b) Explore the data graphically in order to investigate the association between mpg01 and the other features. Which of the other features seem most likely to be useful in predicting mpg01? Scatter plots and box plots may be useful tools to answer this question. Describe your findings.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdChBdXRvKVx0ICMgY3JlYXRlIGEgbWF0cml4IG9mIHNjYXR0ZXJwbG90c1xuYGBgIn0= -->

```r
plot(Auto)	 # create a matrix of scatterplots
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGFyKG1mcm93PWMoMiwzKSlcbmJveHBsb3QoY3lsaW5kZXJzIH4gbXBnMDEsIEF1dG8sIG1haW4gPSBcIkN5bGluZGVycyB2cyBtcGcwMVwiKVxuYm94cGxvdChkaXNwbGFjZW1lbnQgfiBtcGcwMSwgQXV0bywgbWFpbiA9IFwiRGlzcGxhY2VtZW50IHZzIG1wZzAxXCIpXG5ib3hwbG90KGhvcnNlcG93ZXIgfiBtcGcwMSwgQXV0bywgbWFpbiA9IFwiSG9yc2Vwb3dlciB2cyBtcGcwMVwiKVxuYm94cGxvdCh3ZWlnaHQgfiBtcGcwMSwgQXV0bywgbWFpbiA9IFwiV2VpZ2h0IHZzIG1wZzAxXCIpXG5ib3hwbG90KGFjY2VsZXJhdGlvbiB+IG1wZzAxLCBBdXRvLCBtYWluID0gXCJBY2NlbGVyYXRpb24gdnMgbXBnMDFcIilcbmJveHBsb3QoeWVhciB+IG1wZzAxLCBBdXRvLCBtYWluID0gXCJZZWFyIHZzIG1wZzAxXCIpXG5ib3hwbG90KG9yaWdpbiB+IG1wZzAxLCBBdXRvLCBtYWluID0gXCJPcmlnaW4gdnMgbXBnMDFcIilcbmBgYCJ9 -->

```r
par(mfrow=c(2,3))
boxplot(cylinders ~ mpg01, Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, Auto, main = "Year vs mpg01")
boxplot(origin ~ mpg01, Auto, main = "Origin vs mpg01")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


(c) Split the data into a training set and a test set.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2V0LnNlZWQoMSlcbnRyYWluID0gc2FtcGxlKGRpbShBdXRvKVsxXSwgc2l6ZSA9IDAuODAqZGltKEF1dG8pWzFdLCByZXBsYWNlPUZBTFNFKVx0IyBzZXQgODAlIG9mIGRhdGEgc2V0IGFzIHRyYWluaW5nIHNldFxudGVzdCA9IC10cmFpbiBcdFx0XHRcdCMgc2V0IHJlbWFpbmluZyAyMCUgb2YgZGF0YSBzZXQgYXMgdGVzdCBzZXRcbnRyYWluaW5nX2RhdGEgPSBBdXRvW3RyYWluLCBdXHRcdCMgc3BsaXQgdHJhaW5pbmcgc2V0XG5cbnRlc3RpbmdfZGF0YT0gQXV0b1t0ZXN0LCBdXHRcdCMgc3BsaXQgdGVzdCBzZXRcbm1wZzAxLnRlc3QgPC0gbXBnMDFbdGVzdF1cdFx0IyBzcGxpdCByZXNwb25zZSB0ZXN0IHNldFxuZGltKEF1dG8pXHRcdFx0XHQjIHNob3cgZGltZW5zaW9uIG9mIEF1dG8gZGF0YSBzZXRcbmRpbSh0cmFpbmluZ19kYXRhKVx0XHRcdCMgc2hvdyBkaW1lbnNpb24gb2YgdHJhaW5pbmcgc2V0XG5kaW0odGVzdGluZ19kYXRhKVx0XHRcdCMgc2hvdyB0ZXN0IG9mIHRyYWluaW5nIHNldFxuYGBgIn0= -->

```r
set.seed(1)
train = sample(dim(Auto)[1], size = 0.80*dim(Auto)[1], replace=FALSE)	# set 80% of data set as training set
test = -train 				# set remaining 20% of data set as test set
training_data = Auto[train, ]		# split training set

testing_data= Auto[test, ]		# split test set
mpg01.test <- mpg01[test]		# split response test set
dim(Auto)				# show dimension of Auto data set
dim(training_data)			# show dimension of training set
dim(testing_data)			# show test of training set
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



(d) Perform LDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5sZGEuZml0IDwtIGxkYShtcGcwMX5jeWxpbmRlcnMrd2VpZ2h0K2Rpc3BsYWNlbWVudCtob3JzZXBvd2VyLCBBdXRvLCBzdWJzZXQgPSB0cmFpbilcbmxkYS5maXRcblxubGRhLnByZWQgPSBwcmVkaWN0KGxkYS5maXQsIHRlc3RpbmdfZGF0YSlcbmRhdGEuZnJhbWUobGRhLnByZWQpWzE6NSxdXG5cbnRhYmxlKGxkYS5wcmVkJGNsYXNzLCBtcGcwMS50ZXN0LCBkbm4gPSBjKFwiUHJlZGljdGVkXCIsIFwiQWN0dWFsXCIpKVxuXG5tZWFuKGxkYS5wcmVkJGNsYXNzIT1tcGcwMS50ZXN0KSAjIHRlc3QgZXJyb3Igb2YgTERBIG1vZGVsXG5gYGAifQ== -->

```r

lda.fit <- lda(mpg01~cylinders+weight+displacement+horsepower, Auto, subset = train)
lda.fit

lda.pred = predict(lda.fit, testing_data)
data.frame(lda.pred)[1:5,]

table(lda.pred$class, mpg01.test, dnn = c("Predicted", "Actual"))

mean(lda.pred$class!=mpg01.test) # test error of LDA model
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


(e) Perform QDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucWRhLmZpdCA8LSBxZGEobXBnMDF+Y3lsaW5kZXJzK3dlaWdodCtkaXNwbGFjZW1lbnQraG9yc2Vwb3dlciwgQXV0bywgc3Vic2V0ID0gdHJhaW4pXG5xZGEuZml0XG5cbnFkYS5wcmVkID0gcHJlZGljdChxZGEuZml0LCB0ZXN0aW5nX2RhdGEpXG5kYXRhLmZyYW1lKHFkYS5wcmVkKVsxOjUsXVxuXG50YWJsZShxZGEucHJlZCRjbGFzcywgbXBnMDEudGVzdCwgZG5uID0gYyhcIlByZWRpY3RlZFwiLCBcIkFjdHVhbFwiKSlcblxubWVhbihxZGEucHJlZCRjbGFzcz09bXBnMDEudGVzdClcbm1lYW4ocWRhLnByZWQkY2xhc3MhPW1wZzAxLnRlc3QpICMgdGVzdCBlcnJvciBvZiBMREEgbW9kZWxcbmBgYCJ9 -->

```r
qda.fit <- qda(mpg01~cylinders+weight+displacement+horsepower, Auto, subset = train)
qda.fit

qda.pred = predict(qda.fit, testing_data)
data.frame(qda.pred)[1:5,]

table(qda.pred$class, mpg01.test, dnn = c("Predicted", "Actual"))

mean(qda.pred$class==mpg01.test)
mean(qda.pred$class!=mpg01.test) # test error of LDA model
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




(f) Perform logistic regression on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZ2xtLmZpdCA9IGdsbShtcGcwMX5jeWxpbmRlcnMrd2VpZ2h0K2Rpc3BsYWNlbWVudCtob3JzZXBvd2VyLCBBdXRvLCBmYW1pbHkgPSBiaW5vbWlhbCwgc3Vic2V0ID0gdHJhaW4pXG5zdW1tYXJ5KGdsbS5maXQpXG5cbmdsbS5wcm9icyA9IHByZWRpY3QoZ2xtLmZpdCwgdGVzdGluZ19kYXRhLCB0eXBlPVwicmVzcG9uc2VcIikgXG5cbmdsbS5wcmVkID0gaWZlbHNlKGdsbS5wcm9icz4wLjUsIDEsIDApXG5cbnRhYmxlKGdsbS5wcmVkLCBtcGcwMS50ZXN0KVxubWVhbihnbG0ucHJlZCAhPSBtcGcwMS50ZXN0KSMgdGVzdCBlcnJvciBmb3IgbG9naXN0aWMgcmVncmVzc2lvbiBtb2RlbFxuYGBgIn0= -->

```r
glm.fit = glm(mpg01~cylinders+weight+displacement+horsepower, Auto, family = binomial, subset = train)
summary(glm.fit)

glm.probs = predict(glm.fit, testing_data, type="response") 

glm.pred = ifelse(glm.probs>0.5, 1, 0)

table(glm.pred, mpg01.test)
mean(glm.pred != mpg01.test)# test error for logistic regression model
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



(g) Perform KNN on the training data, with several values of K, in order to predict mpg01. Use only the variables that seemed most associated with mpg01 in (b). What test errors do you obtain? Which value of K seems to perform the best on this data set?


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxua25uLnByZWQgPSBrbm4odHJhaW4uWCwgdGVzdC5YLCBtcGcwMS50cmFpbiwgaz0xKVxudGFibGUoa25uLnByZWQsIG1wZzAxLnRlc3QpXG5tZWFuKGtubi5wcmVkIT1tcGcwMS50ZXN0KSAjIG1lYXN1cmVzIHRlc3QgZXJyb3Igb2Ygaz0xXG5cbmtubi5wcmVkID0ga25uKHRyYWluLlgsIHRlc3QuWCwgbXBnMDEudHJhaW4sIGs9NSlcbnRhYmxlKGtubi5wcmVkLCBtcGcwMS50ZXN0KVxubWVhbihrbm4ucHJlZCE9bXBnMDEudGVzdCkgIyBtZWFzdXJlcyB0ZXN0IGVycm9yIG9mIGs9NVxuXG5rbm4ucHJlZCA9IGtubih0cmFpbi5YLCB0ZXN0LlgsIG1wZzAxLnRyYWluLCBrPTEwKVxudGFibGUoa25uLnByZWQsIG1wZzAxLnRlc3QpXG5tZWFuKGtubi5wcmVkIT1tcGcwMS50ZXN0KSAjIG1lYXN1cmVzIHRlc3QgZXJyb3Igb2Ygaz0xMFxuXG5rbm4ucHJlZCA9IGtubih0cmFpbi5YLCB0ZXN0LlgsIG1wZzAxLnRyYWluLCBrPTYpXG50YWJsZShrbm4ucHJlZCwgbXBnMDEudGVzdClcbm1lYW4oa25uLnByZWQhPW1wZzAxLnRlc3QpICMgbWVhc3VyZXMgdGVzdCBlcnJvciBvZiBrPTZcblxua25uLnByZWQgPSBrbm4odHJhaW4uWCwgdGVzdC5YLCBtcGcwMS50cmFpbiwgaz0yMClcbnRhYmxlKGtubi5wcmVkLCBtcGcwMS50ZXN0KVxubWVhbihrbm4ucHJlZCE9bXBnMDEudGVzdCkgIyBtZWFzdXJlcyB0ZXN0IGVycm9yIG9mIGs9MjBcblxua25uLnByZWQgPSBrbm4odHJhaW4uWCwgdGVzdC5YLCBtcGcwMS50cmFpbiwgaz0xMDApXG50YWJsZShrbm4ucHJlZCwgbXBnMDEudGVzdClcbm1lYW4oa25uLnByZWQhPW1wZzAxLnRlc3QpICMgbWVhc3VyZXMgdGVzdCBlcnJvciBvZiBrPTIwXG5cbmBgYCJ9 -->

```r
knn.pred = knn(train.X, test.X, mpg01.train, k=1)
table(knn.pred, mpg01.test)
mean(knn.pred!=mpg01.test) # measures test error of k=1

knn.pred = knn(train.X, test.X, mpg01.train, k=5)
table(knn.pred, mpg01.test)
mean(knn.pred!=mpg01.test) # measures test error of k=5

knn.pred = knn(train.X, test.X, mpg01.train, k=10)
table(knn.pred, mpg01.test)
mean(knn.pred!=mpg01.test) # measures test error of k=10

knn.pred = knn(train.X, test.X, mpg01.train, k=6)
table(knn.pred, mpg01.test)
mean(knn.pred!=mpg01.test) # measures test error of k=6

knn.pred = knn(train.X, test.X, mpg01.train, k=20)
table(knn.pred, mpg01.test)
mean(knn.pred!=mpg01.test) # measures test error of k=20

knn.pred = knn(train.X, test.X, mpg01.train, k=100)
table(knn.pred, mpg01.test)
mean(knn.pred!=mpg01.test) # measures test error of k=20

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->

