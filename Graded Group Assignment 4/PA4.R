### Group Matrix ###

# Remove all variables and plots
rm(list = ls())

# Set working directory
# setwd("C:/R folder/tmp/myrepo/Graded Group Assignment 4")

#### Q1. ####
Data <-  read.csv("cons.csv")

#### Q2. ####

ols <- function(y, x) {
  reg <- lm(y ~ x)
  coef0 <- reg$coefficients[[1]]
  coef1 <- reg$coefficients[[2]]
  beta <- as.matrix(c(coef0, coef1), nrow = 2)
  return(beta)
}


#### Q3. ####
b0 = 0.42
b1 = 0.95
n = 198

x = log(Data$di[-199])
xT = log(Data$di[199])

#### Q4. ####

w0 <- matrix(0, 1000, 1)
w1 <- matrix(0, 1000, 1)
w2 <- matrix(0, 1000, 1)

beta_mat <- matrix(0, 1000, nrow = 2)

for(i in 1:1000) {
  set.seed(i)
  u = rnorm(n)
  y = b0 + b1*x + u
  beta_mat[, i] = ols(y, x)
  uhat = y - beta_mat[1, i] - beta_mat[2, i]*x
  varu = sum(uhat^2)/(n - 2)
  
  w0[i] = exp(b0 + b1*xT + 0.5)
  w1[i] = exp(beta_mat[1, i] + beta_mat[2, i]*xT)
  w2[i] = exp(beta_mat[1, i] + beta_mat[2, i]*xT + 0.5*varu)
}

#### Q5. #### 
bias <- matrix(0, 2, 1)

for(j in 1:1000) {
  bias[1] = 1/1000*sum(w1[j] - w0[j])
  bias[2] = 1/1000*sum(w2[j] - w0[j])
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
yUB <- exp(sum(mod2$coefficients*c(1, ldT)) + 0.5*sig)

#### Q11. ####
mc = 10000

beta_mat2 <- matrix(0, mc, nrow = 2)
# list_theo_var <- list()
y1 <- matrix(0, mc, ncol = 1)
y2 <- matrix(0, mc, ncol = 1)

for(i in 1:mc) {
  set.seed(i)
  U = rnorm(n = 100, mean = 0, sd = sqrt(10))
  X = rnorm(n = 100, mean = 5, sd = sqrt(16))
  X1 = matrix(c(matrix(rep(1, 100)), matrix(X)), ncol = 2)
  Y = 1 - 3*X + U
  beta_mat2[, i] = ols(Y, X)
  theo_var <- 10*solve(t(X1) %*% X1)
  # list_theo_var[[i]] <- theo_var
  y1[i] <- (beta_mat2[1, i] - mean(beta_mat2[1, ]))/sqrt(theo_var[1, 1])
  y2[i] <- (beta_mat2[2, i] - mean(beta_mat2[2, ]))/sqrt(theo_var[2, 2])
}

##### Q12. ####

beta_mat2_est <- matrix(0, mc, nrow = 2)
z1 <- matrix(0, mc, ncol = 1)
z2 <- matrix(0, mc, ncol = 1)

for(i in 1:mc) {
  set.seed(i)
  U = rnorm(n = 100, mean = 0, sd = sqrt(10))
  X = rnorm(n = 100, mean = 5, sd = sqrt(16))
  X1 = matrix(c(matrix(rep(1, 100)), matrix(X)), ncol = 2)
  Y = 1 - 3*X + U
  beta_mat2_est[, i] = ols(Y, X)
  uhat_sim = Y - beta_mat2_est[1, i] - beta_mat2_est[2, i]*X
  varu_sim = sum(uhat_sim^2)/(100 - 2)
  theo_var_sim <- varu_sim*solve(t(X1) %*% X1)
  z1[i] <- (beta_mat2_est[1, i] - mean(beta_mat2_est[1, ]))/sqrt(theo_var_sim[1, 1])
  z2[i] <- (beta_mat2_est[2, i] - mean(beta_mat2_est[2, ]))/sqrt(theo_var_sim[2, 2])
}
