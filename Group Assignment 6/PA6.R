#### Group Matrix #####

rm(list = ls())

### Q1 ####
df <- read.csv("mroz_subset.csv")


### Q2 ####
Y <- as.matrix(df$lwage)


#### Q3 ####
n0 <- nrow(Y)


#### Q4 ####
intercept <-  rep(1,428)
x <- as.matrix(df[, -1])

X <- cbind(intercept, x)

#### Q5 ####
#define OLS formula
ols <- function(y,x){
  beta = solve(t(x) %*% x) %*% t(x) %*% as.matrix(y)
  return(beta)
}

# OLS Coefficients
b0 <- ols(Y,X) 

### Q6 ####
nz0 <- sum(abs(b0) > 0.01)

### Q7 ####
# Define lambda sequence
lam <- seq(10, 500)

# Define beta_tilde matrix
beta_tilde <- matrix(0, length(b0), length(lam) )

# Define # of elements greater than 0.01 matrix
nz1 <- matrix(0, 1 , length(lam))

# Define Ridge residuals
e_hat <-  matrix(0, n0, length(lam))

# Define error term variance
Var_ridge <- matrix(0, 1, length(lam))


# Define Ridge formula
Ridge <-  function(y,x){
  beta_tilde = solve( t(x) %*% x + lam[i] * diag(ncol(x)) ) %*% t(x) %*% as.matrix(y)
  return(beta_tilde)
}


## Run Ridge loop
for (i in 1:length(lam)){
  x <-  X
  y <-  Y
  beta_tilde[,i] <- Ridge(y,x)
  
  nz1[,i] <- sum(abs(beta_tilde[,i]) > 0.01)
  
  e_hat[,i] = Y - X%*%beta_tilde[,i] 
  
  Var_ridge[,i] = (sum(e_hat[,i]^2))/ (nrow(X)-11)
  
}
 # Variance - Plot using 'plot(t(Var_ridge))' - Variance should be increasing
 # # of variables - Plot using 'plot(t(nz1))' - # of variables should be decreasing - higher lambda means smaller coefficients
 # Coefficients - Plot using 'plot(beta_tilde[i,])' where i is one of the betas - Should be converge to zero has lambda increases




### Q8 #### 
# the first element of 'lam' is 10 and increases in increments of 1
 # so to find lam = 30 (30 - 9 = 21 check using lam[21] )
 # lam = 460 (460-9 = 451) check using lam[451]

a1 <-  c(nz1[,21] , nz1[,451])
 # Interpretation  of non-zero coefficients is 'nz' variable


### Q9 ####
b1 <-  c( sum((beta_tilde[,21] - b0)^2) , sum((beta_tilde[,451] - b0)^2))



### Q10 ####
c1 <-  c( Var_ridge[,21],  Var_ridge[,451])

# Subset n = 100, n=15
Y100 <-  as.matrix(Y[1:100,])

X100 <- as.matrix(X[1:100, ])

Y15 <-  as.matrix(Y[1:15,])

X15 <- as.matrix(X[1:15, ])

# Re-estimation of values
# Define beta_tilde matrix
beta_tilde100 <- matrix(0, length(b0), length(lam) )

beta_tilde15 <- matrix(0, length(b0), length(lam) )

# Define # of elements greater than 0.01 matrix
nz100 <- matrix(0, 1 , length(lam))

nz15 <- matrix(0, 1 , length(lam))


# Define Ridge residuals
e_hat100 <-  matrix(0, 100, length(lam))

e_hat15 <-  matrix(0, 15, length(lam))


# Define error term variance
Var_ridge100 <- matrix(0, 1, length(lam))

Var_ridge15 <- matrix(0, 1, length(lam))


# Loop for n= 100
for (i in 1:length(lam)){
  x <-  X100
  y <-  Y100
  beta_tilde100[,i] <- Ridge(y,x)
  
  nz100[,i] <- sum(abs(beta_tilde100[,i]) > 0.01)
  
  e_hat100[,i] = Y100 - X100%*%beta_tilde100[,i] 
  
  Var_ridge100[,i] = (sum(e_hat100[,i]^2))/ (nrow(X100)-11)
  
}

# Loop for n = 15

for (i in 1:length(lam)){
  x <-  X15
  y <-  Y15
  beta_tilde15[,i] <- Ridge(y,x)
  
  nz15[,i] <- sum(abs(beta_tilde15[,i]) > 0.01)
  
  e_hat15[,i] = Y15 - X15%*%beta_tilde15[,i] 
  
  Var_ridge15[,i] = (sum(e_hat15[,i]^2))/ (nrow(X15)-11)
  
}



### Q11 ####
a2 <- c(nz100[,21] , nz100[,451])


### Q12 ####
b2 <- c( sum((beta_tilde100[,21] - b0)^2) , sum((beta_tilde100[,451] - b0)^2))


### Q13 ####
c2 <- c( Var_ridge100[,21],  Var_ridge100[,451])


### Q14 ####
a3 <- c(nz15[,21] , nz15[,451])


### Q15 ####
b3 <- c( sum((beta_tilde15[,21] - b0)^2) , sum((beta_tilde15[,451] - b0)^2))


### Q16 ####
c3 <- c( Var_ridge15[,21],  Var_ridge15[,451])

