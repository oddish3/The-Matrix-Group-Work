#### Group Matrix #####

rm(list=ls())

#### Q1 ####
library(MASS)


#### Q2 ####
q = 4

b = 0

n = 500


#### Q3 ####
R2 <-  0.2
rho <-  0

# Define variance-covariance matrix
Omega <-  matrix(c(1, rho, rho, 1), ncol = 2)

# Define OLS, 2SLS coefficient and concentration parameter matrix from simulation
beta_OLS <- matrix(0, 5000, 1)

beta_2SLS <- matrix(0, 5000, 1)

mu <- matrix(0, 5000, 1)


# Run simulation
for (i in 1:5000) {
  # Set seed
  set.seed(i)
  
  # Define v vector (n x 2)
  v = mvrnorm(n = n, mu = c(0,0), Sigma = Omega) #c(0,0) is mean of error terms
  
  # Define u and w
  u = matrix(v[, 1]) #first column of v
  w = matrix(v[, 2]) #second column of v
  
  # Define z (500 x 4)
  z = matrix(rnorm(n*q, 0, 1), ncol = q) #n*q = 2000, standard normal (0,1)
  
  # Define pi and Pi matrix
  pi = sqrt(R2 / (q - q*R2))
  
  Pi = matrix(pi, nrow = q, ncol = 1)
  
  x = z %*% Pi + w

  # Define y
  y = x %*% matrix(b) + u
  
  # Projection matrix Pz
  Pz = z %*% solve(t(z) %*% z) %*% t(z)
  
  # OLS beta0
  beta_OLS[i, ] = (t(x) %*% y) / (t(x) %*% x)

  # 2SLS beta0
  beta_2SLS[i, ] = (t(x) %*% Pz %*% y) / (t(x) %*% Pz %*% x)
  
  # concentration parameter
  mu[i, ] = t(Pi) %*% t(z) %*% z %*% Pi
  
}


# Bias of OLS
ols1 <- mean(beta_OLS - b)


# Bias of 2SLS
sls1 <- mean(beta_2SLS - b)


# mean of concentration parameter
m1 <-  mean(mu)





#### Q4 ####
R22 <-  0.00001
rho2 <-  0

# Define variance-covariance matrix
Omega2 <-  matrix(c(1, rho2, rho2, 1), ncol = 2)

# Define OLS, 2SLS coefficient and concentration parameter matrix from simulation
beta_OLS2 <- matrix(0, 5000, 1)

beta_2SLS2 <- matrix(0, 5000, 1)

mu2 <- matrix(0, 5000, 1)


# Run simulation
for (i in 1:5000) {
  # Set seed
  set.seed(i)
  
  # Define v vector (n x 2)
  v2 = mvrnorm(n = n, mu = c(0,0), Sigma = Omega2) #c(0,0) is mean of error terms
  
  # Define u and w
  u2 = matrix(v2[, 1]) #first column of v
  w2 = matrix(v2[, 2]) #second column of v
  
  # Define z (500 x 4)
  z2 = matrix(rnorm(n*q, 0, 1), ncol = q) #n*q = 2000, standard normal (0,1)
  
  # Define pi and Pi matrix
  pi2 = sqrt(R22 / (q - q*R22))
  
  Pi2 = matrix(pi2, nrow = q, ncol = 1)
  
  x2 = z2 %*% Pi2 + w2
  
  # Define y
  # y2 = b*x2 + u2
  y2 = x2 %*% matrix(b) + u2
  
  # Projection matrix Pz
  Pz2 = z2 %*% solve(t(z2) %*% z2) %*% t(z2)
  
  # OLS beta0
  beta_OLS2[i, ] = (t(x2) %*% y2) / (t(x2) %*% x2)
  
  # 2SLS beta0
  beta_2SLS2[i, ] = (t(x2) %*% Pz2 %*% y2) / (t(x2) %*% Pz2 %*% x2)
  
  # concentration parameter
  mu2[i, ] = t(Pi2) %*% t(z2) %*% z2 %*% Pi2
  
}


# Bias of OLS
ols2 <- mean(beta_OLS2 - b)


# Bias of 2SLS
sls2 <- mean(beta_2SLS2 - b)


# mean of concentration parameter
m2 <-  mean(mu2)




#### Q5 ####
R23 <-  0.2
rho3 <-  0.9

# Define variance-covariance matrix
Omega3 <-  matrix(c(1, rho3, rho3, 1), ncol = 2)

# Define OLS, 2SLS coefficient and concentration parameter matrix from simulation
beta_OLS3 <- matrix(0, 5000, 1)

beta_2SLS3 <- matrix(0, 5000, 1)

mu3 <- matrix(0, 5000, 1)


# Run simulation
for (i in 1:5000) {
  # Set seed
  set.seed(i)
  
  # Define v vector (n x 2)
  v3 = mvrnorm(n = n, mu = c(0,0), Sigma = Omega3) #c(0,0) is mean of error terms
  
  # Define u and w
  u3 = matrix(v3[ ,1]) #first column of v
  w3 = matrix(v3[ ,2]) #second column of v
  
  # Define z (500 x 4)
  z3 = matrix(rnorm(n*q, 0, 1), ncol = q) #n*q = 2000, standard normal (0,1)
  
  # Define pi and Pi matrix
  pi3 = sqrt(R23 / (q - q*R23))
  
  Pi3 = matrix(pi3, nrow = q, ncol = 1)
  
  x3 = z3 %*% Pi3 + w3
  
  # Define y
  y3 = x3 %*% matrix(b) + u3
  
  # Projection matrix Pz
  Pz3 = z3 %*% solve(t(z3) %*% z3) %*% t(z3)
  
  # OLS beta0
  beta_OLS3[i, ] = (t(x3) %*% y3) / (t(x3) %*% x3)
  
  # 2SLS beta0
  beta_2SLS3[i, ] = (t(x3) %*% Pz3 %*% y3) / (t(x3) %*% Pz3 %*% x3)
  
  # concentration parameter
  mu3[i, ] = t(Pi3) %*% t(z3) %*% z3 %*% Pi3
  
}


# Bias of OLS
ols3 <- mean(beta_OLS3 - b)


# Bias of 2SLS
sls3 <- mean(beta_2SLS3 - b)


# mean of concentration parameter
m3 <-  mean(mu3)



#### Q6 ####
R24 <-  0.00001
rho4 <-  0.9

# Define variance-covariance matrix
Omega4 <-  matrix(c(1, rho4, rho4, 1), ncol = 2)

# Define OLS, 2SLS coefficient and concentration parameter matrix from simulation
beta_OLS4 <- matrix(0, 5000, 1)

beta_2SLS4 <- matrix(0, 5000, 1)

mu4 <- matrix(0, 5000, 1)


# Run simulation
for (i in 1:5000) {
  # Set seed
  set.seed(i)
  
  # Define v vector (n x 2)
  v4 = mvrnorm(n = n, mu = c(0,0), Sigma = Omega3) #c(0,0) is mean of error terms
  
  # Define u and w
  u4 = matrix(v4[, 1]) #first column of v
  w4 = matrix(v4[, 2]) #second column of v
  
  # Define z (500 x 4)
  z4 = matrix(rnorm(n*q, 0, 1), ncol = q) #n*q = 2000, standard normal (0,1)
  
  # Define pi and Pi matrix
  pi4 = sqrt(R24 / (q - q*R24))
  
  Pi4 = matrix(pi4, nrow = q, ncol = 1)
  
  x4 = z4 %*% Pi4 + w4
  
  # Define y
  y4 = x4 %*% matrix(b) + u4
  
  # Projection matrix Pz
  Pz4 = z4 %*% solve(t(z4) %*% z4) %*% t(z4)
  
  # OLS beta0
  beta_OLS4[i, ] = (t(x4) %*% y4) / (t(x4) %*% x4)
  
  # 2SLS beta0
  beta_2SLS4[i, ] = (t(x4) %*% Pz4 %*% y4) / (t(x4) %*% Pz4 %*% x4)
  
  # concentration parameter
  mu4[i, ] = t(Pi4) %*% t(z4) %*% z4 %*% Pi4
  
}


# Bias of OLS
ols4 <- mean(beta_OLS4 - b)


# Bias of 2SLS
sls4 <- mean(beta_2SLS4 - b)


# mean of concentration parameter
m4 <-  mean(mu4)




