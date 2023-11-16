## PA5
rm(list=ls())

#### Q1. ####

# Number of realizations
T <- 500

# Initialize the vector to store the final u values
u <- numeric(T)

# Loop over each realization
for (i in 1:T) {
  # Set the seed
  set.seed(i)
  
  # Initialize the first value of u and e
  e <- rnorm(T)  # Generate T realizations of e
  u[1] <- e[1]   # Initialize u1
  
  # Compute u for t > 1
  for (t in 2:T) {
    u[t] <- 0.8 * u[t - 1] + e[t]
  }
  
  # Store the last value of each realization
  u[i] <- u[T]
}

## 2
library(tseries) 
# Number of realizations
T <- 500

# Initialize the vector to store the final v values
v <- numeric(T)

# Loop over each realization
for (i in 1:T) {
  # Set the seed
  set.seed(i)
  
  # Initialize the first value of v and e
  e <- rnorm(T)  # Generate T realizations of e
  v[1] <- e[1]   # Initialize v1
  
  # Compute v for t > 1
  for (t in 2:T) {
    v[t] <- v[t - 1] + e[t]
  }
  
  # Store the last value of each realization
  v[i] <- v[T]
}

## 3

# Perform the Dickey-Fuller test on ut
adf_ut <- adf.test(u)

# Extract the p-value for ut
pu <- adf_ut$p.value

# Perform the Dickey-Fuller test on vt
adf_vt <- adf.test(v)

# Extract the p-value for vt
pv <- adf_vt$p.value


## 4

# Load the USeconomic data
data("USeconomic")

# Save the length of the GNP series
TGNP <- length(GNP)

## 5 

# Perform the Dickey-Fuller test on the GNP series
adf_gnp <- adf.test(GNP)

# Extract the p-value
p1 <- adf_gnp$p.value


## 6 
# Compute the first difference of the GNP
dGNP <- diff(GNP)

## 7

# Perform the ADF test on the first difference of the GNP series
adf_dGNP <- adf.test(dGNP)

# Extract the p-value
p2 <- adf_dGNP$p.value


