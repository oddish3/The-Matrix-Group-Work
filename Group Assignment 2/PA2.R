#### Group Matrix ####

# Remove all variables in environment
rm(list = ls())
#Set working directory
#setwd("/Users/hoikwan/Documents/University /MSc/Modules/ECON61001 Econometric Methods/Assessment/Problem Assignments/PA2")

#### Q1. ####

Data <- read.csv('wage.csv')

#### Q2. ####

educbar <- mean(Data$educ) 

#### Q3. ####
w12 <- mean(Data$wage[Data$educ == 12])

w13 <- mean(Data$wage[Data$educ == 13])

#### Q4. ####
alpha1 <- (w13-w12)/w12*100 

#### Q5. ####
reg1 <- lm(log(wage) ~ educ, data = Data)

beta0 <- reg1$coefficients[[1]] #intercept

beta1 <- reg1$coefficients[[2]] #slope parameter

#### Q6. ####
Data_New = Data[-row(Data)[Data$educ == 0], ]

#### Q7. ####
reg2 <- lm(log(wage) ~ log(educ), data = Data_New)

delta0 <-  reg2$coefficients[[1]]

delta1 <- reg2$coefficients[[2]]

#### Q8.####

# Let the regressor be: y = 15 + 2*x + e

set.seed(123)
n = 500
x <- rnorm(n, 5, 4)
sim_b0 = 15 # Population intercept
sim_b1 = 2 # Population slope

ols <- function(y, x) {
  reg <- lm(y ~ x)
  coef0 <- reg$coefficients[[1]]
  coef1 <- reg$coefficients[[2]]
  coef <- as.matrix(c(coef0, coef1), ncol = 2)
  return(coef)
}

# Computing 1000 realisations of OLS coefficients
beta_mat1 <- matrix(0, ncol = 2, 1000)

for (i in 1:1000) {
  set.seed(i)
  u = rnorm(n, 0, 0.5)
  y = sim_b0 + sim_b1*x + u
  beta_mat1[i, ] <- ols(y, x)
}

#### Q9. ####
x <-  Data$educ

#### Q10. ####
b0 <- beta0 #intercept

b1 <- beta1 #slope parameter

#### Q11. ####
# simulate 1000 new realisations and save the estimated coefficients for all of them
beta_mat2 <- matrix(0, ncol = 2, 1000)

for (i in 1:1000) {
  set.seed(i)
  u = rnorm(526, 0, 10)
  y = b0 + b1*x + u
  beta_mat2[i, ] <- ols(y, x)
}


#### Q12. ####
c <- c(mean(beta_mat2[, 1]), mean(beta_mat2[, 2]))

#### Q13. ####

beta_mat3 <- matrix(0, ncol = 2, 1000)

for (i in 1:1000) {
  set.seed(i)
  sim_u = rnorm(1000, 0, 10)
  sim_x = rnorm(1000, 5, 10)
  sim_y = sim_x + sim_u
  beta_hat <- (t(as.matrix(sim_x)) %*% as.matrix(sim_y))/(t(as.matrix(sim_x)) %*% as.matrix(sim_x))
  beta_tilde <- mean(sim_y)/mean(sim_x)
  beta_mat3[i, ] <- matrix(c(beta_hat, beta_tilde), ncol = 2)
}

#### Q14. ####
var1 <-  var(beta_mat3[, 1])
var2 <-  var(beta_mat3[, 2])

#### Q15. ####

# since var1 (OLS) us smaller than var2 (avg.), OLS is the more efficient estimator
eff <- "OLS"
