# Group Programming assignment 4

rm(list=ls())
setwd("C:/R folder/tmp/myrepo/Graded Group Assignment 4")

#1 
Data = read.csv("cons.csv")

#2
ols <- function(y, x) {
  reg <- lm(y ~ x)
  coef0 <- reg$coefficients[[1]]
  coef1 <- reg$coefficients[[2]]
  coef <- as.matrix(c(coef0, coef1), nrow = 2)
  return(coef)
}

#3
b0 = 0.42
b1 = 0.95
n = 198
x = log(Data$di[-199])
xT = log(Data$di[199])


#4 

# Computing 1000 realisations of OLS coefficients
beta_mat1 <- matrix(0, nrow = 2, ncol = 1000)
y = matrix(0, nrow = n, ncol = 1)
residuals = matrix(0, nrow = n, ncol = 1)
rss <- matrix(0, ncol = 1, nrow = 1000)
var1 <- matrix(0, ncol = 1, nrow = 1000)
w0 <- matrix(0, ncol = 1, nrow = 1000)
w1 <- matrix(0, ncol = 1, nrow = 1000)
w2 <- matrix(0, ncol = 1, nrow = 1000)

for (i in 1:1000) {
  set.seed(i) #a
  u = rnorm(n, 0, 1) #b
  y = b0 + b1*x + u #c
  ols_result = ols(y, x)
  beta_mat1[, i] <- ols_result #d
  residuals = y - (b0 + b1*x)  # Calculate residuals
  rss[i] = sum(residuals^2) #e
  var1[i] = var(residuals)
  w0[i] = exp(b0 + b1*xT + 0.5) #true forecast 
  b0_ha = beta_mat1[1, i]
  b1_ha = beta_mat1[2, i]
  w1[i] = exp(b0_ha + b1_ha * xT) #naive forecast
  w2[i] = exp(b0_ha + b1_ha * xT + 0.5 * var1[i]) #unbiased forecast 
  }

#5 - admittedly not very elegant
biast = matrix(0, nrow = 2, ncol = 1000)

for (j in 1:1000) {
  biast[1, j] = sum(w1[j]-w0[j])
  biast[2, j] = sum(w2[j]-w1[j])
}

bias = matrix(0, nrow = 2)
bias[1,1] = 1/1000*sum(biast[1, j])
bias[2,1] = 1/1000*sum(biast[2, j])
bias

#6
y = Data$c
ld = log(Data$di)
ldT = log(388804.1)

#7
mod1 = lm(y ~ ld)
summary(mod1)
alpha_0 <- coef(mod1)[1] 
alpha_1 <- coef(mod1)[2]
yT <- alpha_0 + alpha_1 * ldT
yT
#q8
sig <- sigma(mod1)^2
sig
#q9
yN = exp(alpha_0 + alpha_1 * ldT) #naive
yN
#q10
yUB = exp(b0_ha + b1_ha*ldT + 0.5 * var1[i])
yUB

#q11
