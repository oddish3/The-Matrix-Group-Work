### Group Matrix ###

# Remove all variables and plots
rm(list = ls())

# Set working directory
setwd("C:/R folder/tmp/myrepo/Graded Group Assignment 4")

#### Q1. ####
Data <-  read.csv("cons.csv")

#### Q2. ####
ols<-function(y,x){
  ones <- rep(1,length(x))
  X = matrix(c(ones, x), ncol = 2)
  beta = solve(t(X) %*% X) %*% t(X) %*% as.matrix(y)
  return(beta)
}


#### Q3. ####
b0 = 0.42
b1 = 0.95
n = 198

x =log(Data$di[-199])
xT = log(Data$di[199])


#### Q4. #### unsure if this is correct

w0 <- matrix(0,1000,1)
w1 <- matrix(0,1000,1)
w2 <- matrix(0,1000,1)

for(i in 1:1000){
  set.seed(i)
  u = rnorm(n, mean=0, sd = 1)
  y = b0+b1*x+u
  OLS = ols(y,x)
  uhat = y- OLS[1] - OLS[2]*x
  varu = sum(uhat^2)/(198-2)
  
  
  w0[i] = exp(b0+b1*xT+0.5)
  w1[i] = exp(OLS[1] + OLS[2]*xT)
  w2[i] = exp(OLS[1] + OLS[2]*xT +0.5*varu)
}


#### Q5. #### 
bias <- matrix(0,2,1)
for(j in 1:1000){
  bias[1] = 1/1000*sum(w1[j] - w0[j])
  bias[2] = 1/1000*sum(w2[j] - w0[j]) #Sol's using w1 not w0 like in
  # the PSet
}

#### Q6. ####
y = Data$c
ld = log(Data$di)
ldT = log(388804.1)



#### Q7. ####
mod1 <- lm(y ~ ld)

yT = sum(mod1$coefficients*c(1, ldT))




#### Q8. ####
mod2 <-  lm(log(y) ~ ld)

sig = summary(mod2)$sigma^2 


#### Q9. ####
yN <- exp(sum(mod2$coefficients*c(1, ldT)))



#### Q10. ####
yUB <-  exp(sum(mod2$coefficients*c(1,ldT) + 0.5*sig))



#### Q11. #### Unsure if this is correct
mc = 10000

#(a)
OLS0 <-  matrix(0,mc,1)
OLS1 <-  matrix(0,mc,1)
theoretical_var_int = matrix(0,mc,1)
theoretical_var_slope = matrix(0,mc,1)

for(i in 1:mc){
  set.seed(i)
  U = rnorm(n = 100, mean=0, sd=sqrt(10))
  X = rnorm(n = 100, mean = 5, sd=sqrt(16))
  Y = 1 - 3*X+U
  OLS_ = ols(Y,X)
  OLS0[i] = OLS_[1,]
  OLS1[i] = OLS_[2,]
  theoretical_var_int[i] <- 10 * (1/100 + mean(X)^2 / sum((X - mean(X))^2))
  theoretical_var_slope[i] <- 10 / sum((X - mean(X))^2)
}
y1 <- (OLS0 - mean(OLS0)) / sqrt(theoretical_var_int)
y2<- (OLS1 - mean(OLS1)) / sqrt(theoretical_var_slope)
mean(y1) #tried using 
mean(y2)

##### Q12. ####

z1 <-  matrix(0,mc,1)
z2 <-  matrix(0,mc,1)
for(i in 1:mc){
  set.seed(i)
  U = rnorm(n = 100, mean=0, sd=sqrt(10))
  X = rnorm(n = 100, mean = 5, sd=sqrt(16))
  Y = 1 - 3*X+U
  OLS1 = ols(Y,X)
  uhat = Y - 1 +3*X
  sigmaNsqu = sum(uhat^2)/100-2
  varOLS = sigmaNsqu*(t(X)*X)^-1 #unsure if this is correct
  
  z1[i,] = sqrt(varOLS[1])
  z2[i,] = sqrt(varOLS[2])
}
