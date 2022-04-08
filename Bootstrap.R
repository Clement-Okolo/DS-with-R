require(ISLR) # "require" imports library and also returns true or false if library does not exist
require(boot) # "require" imports library and also returns true or false if library does not exist

## Bootstrap; Minimum risk investment - Section 5.2
## Define alpha

alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

alpha(Portfolio$X, Portfolio$Y) # call function to find alpha


## Find the Standard error of alpha

alpha.fn=function(data, index){
  with(data[index,], alpha(X,Y))
}

alpha.fn(Portfolio, 1:100) # call function to get Standard error

set.seed(1)
alpha.fn(Portfolio, sample(1:100,100, replace=TRUE)) #run random sample


boot.out=boot(Portfolio, alpha.fn, R=1000) # do 1000 bootstrap
boot.out
plot(boot.out)
