rm(list=ls())
# interested in conducting a simulation study to examine properties
# of 2SLS 

# with equation y_i = x_i B_0 + u_i
# where x_i is a scalar variable generated via first stage regression
# equation 



# 1

library(MASS)

# 2

### Globals -----

q = 4
b = 0 # (Beta_0)
n = 500


# 3 simulation study --- scenario 1 ------
reps = 5000
beta_ols_results=numeric(reps)
beta_2sls_results=numeric(reps)
mu_results=numeric(reps)

Rsr = 0.2
p = 0
sigma =  matrix(c(1,p, p, 1), nrow = 2, ncol = 2, byrow = TRUE)


for (i in 1:reps){
  set.seed(i)
  v = mvrnorm(n, rep(0, 2), sigma)
  u = matrix(v[, 1])
  w = matrix(v[, 2])
  z = matrix(rnorm(n * q), n, q)
  pi = matrix(sqrt(Rsr / (q - q * Rsr)) * rep(1, q))
  x = z %*% pi + w
  y = x %*% b + u
  Pz = z %*% solve(t(z) %*% z) %*% t(z)
  beta_ols = solve(t(x) %*% x) %*% t(x) %*% y
  beta_2sls = solve(t(x) %*% Pz %*% x) %*% t(x) %*% Pz %*% y
  beta_ols_results[i] = beta_ols
  beta_2sls_results[i] = beta_2sls
  
  mu = (t(pi) %*% t(z) %*% z %*% pi)
  mu_results[i] = mu
  
}

ols1 = mean(beta_ols_results - b)
sls1= mean(beta_2sls_results - b)
m1 = mean(mu_results)


# 4 Scenario 2 -----
reps = 5000
beta_ols_results=numeric(reps)
beta_2sls_results=numeric(reps)
mu_results=numeric(reps)

Rsr = 0.00001
p = 0
sigma =  matrix(c(1,p, p, 1), nrow = 2, ncol = 2, byrow = TRUE)

for (i in 1:reps){
  set.seed(i)
  v = mvrnorm(n, rep(0, 2), sigma)
  u = matrix(v[, 1])
  w = matrix(v[, 2])
  z = matrix(rnorm(n * q), n, q)
  pi = matrix(sqrt(Rsr / (q - q * Rsr)) * rep(1, q))
  x = z %*% pi + w
  y = x %*% b + u
  Pz = z %*% solve(t(z) %*% z) %*% t(z)
  beta_ols = solve(t(x) %*% x) %*% t(x) %*% y
  beta_2sls = solve(t(x) %*% Pz %*% x) %*% t(x) %*% Pz %*% y
  beta_ols_results[i] = beta_ols
  beta_2sls_results[i] = beta_2sls
  
  mu = (t(pi) %*% t(z) %*% z %*% pi)
  mu_results[i] = mu
  
}

ols2 = mean(beta_ols_results - b)
sls2 = mean(beta_2sls_results - b)
m2 = mean(mu_results)


# 5 Scenario 3 -----
reps = 5000
beta_ols_results=numeric(reps)
beta_2sls_results=numeric(reps)
mu_results=numeric(reps)

Rsr = 0.2
p = 0.9
sigma =  matrix(c(1,p, p, 1), nrow = 2, ncol = 2, byrow = TRUE)

for (i in 1:reps){
  set.seed(i)
  v = mvrnorm(n, rep(0, 2), sigma)
  u = matrix(v[, 1])
  w = matrix(v[, 2])
  z = matrix(rnorm(n * q), n, q)
  pi = matrix(sqrt(Rsr / (q - q * Rsr)) * rep(1, q))
  x = z %*% pi + w
  y = x %*% b + u
  Pz = z %*% solve(t(z) %*% z) %*% t(z)
  beta_ols = solve(t(x) %*% x) %*% t(x) %*% y
  beta_2sls = solve(t(x) %*% Pz %*% x) %*% t(x) %*% Pz %*% y
  beta_ols_results[i] = beta_ols
  beta_2sls_results[i] = beta_2sls
  
  mu = (t(pi) %*% t(z) %*% z %*% pi)
  mu_results[i] = mu
  
}

ols3 = mean(beta_ols_results - b)
sls3 = mean(beta_2sls_results - b)
m3 = mean(mu_results)


# 6 Scenario 4 -----
reps = 5000
beta_ols_results=numeric(reps)
beta_2sls_results=numeric(reps)
mu_results=numeric(reps)

Rsr = 0.00001
p = 0.9
sigma =  matrix(c(1,p, p, 1), nrow = 2, ncol = 2, byrow = TRUE)

for (i in 1:reps){
  set.seed(i)
  v = mvrnorm(n, rep(0, 2), sigma)
  u = matrix(v[, 1])
  w = matrix(v[, 2])
  z = matrix(rnorm(n * q), n, q)
  pi = matrix(sqrt(Rsr / (q - q * Rsr)) * rep(1, q))
  x = z %*% pi + w
  y = x %*% b + u
  Pz = z %*% solve(t(z) %*% z) %*% t(z)
  beta_ols = solve(t(x) %*% x) %*% t(x) %*% y
  beta_2sls = solve(t(x) %*% Pz %*% x) %*% t(x) %*% Pz %*% y
  beta_ols_results[i] = beta_ols
  beta_2sls_results[i] = beta_2sls
  
  mu = (t(pi) %*% t(z) %*% z %*% pi)
  mu_results[i] = mu
  
}

ols4 = mean(beta_ols_results - b)
sls4 = mean(beta_2sls_results - b)
m4 = mean(mu_results)
