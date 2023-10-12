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
w12 <-  mean(Data$wage[Data$educ==12])

w13 <- mean(Data$wage[Data$educ==13])

#### Q4. ####
alpha1 <-(w13-w12)/w12 *100 

#### Q5. ####
reg1 <- lm(log(wage) ~ educ, data=Data)

beta0 <- reg1$coefficients[[1]] #intercept

beta1 <- reg1$coefficients[[2]] #slope parameter


#### Q6. ####
# Remove individuals with no education i.e. obs = 2
Data_New = Data[-row(Data) [Data$educ == 0],]



#### Q7. ####
reg2 <- lm(log(wage) ~ log(educ), data = Data_New)

delta0 <-  reg2$coefficients[[1]]

delta1 <- reg2$coefficients[[2]]


#### Q8.####
set.seed(123)
b0 = 1 # Population intercept
b1 = -3 # Population slope
x <- rnorm(526, 5, 4)

#OLS function
ols<-function(y,x){
  ones <- rep(1,length(x))
  X = matrix(c(ones, x), ncol = 2)
  beta = solve(t(X) %*% X) %*% t(X) %*% as.matrix(y)
  return(beta)
}

# Computing 1000 realisations of OLS coefficients
beta<-matrix(0,2,1000)
for (i in 1:1000){
  set.seed(i)
  # x <-rnorm(526,5,4)
  u = rnorm(526,0,0.5)
  y = b0 + b1*x + u
  beta[,i]<-ols(y,x)
}


ols1 <- function(y, x) {
  reg <- lm(y ~ x)
  coef0 <- reg$coefficients[[1]]
  coef1 <- reg$coefficients[[2]]
  coef <- as.matrix(c(coef0, coef1), nrow = 2)
  return(coef)
}

# Computing 1000 realisations of OLS coefficients
beta_mat1 <- matrix(0, nrow = 2, 1000)

for (i in 1:1000) {
  set.seed(i)
  u = rnorm(526, 0, 0.5)
  y = b0 + b1*x + u
  beta_mat1[, i] <- ols1(y, x)
}

mean(beta[2, ])
mean(beta_mat1[2, ])
var(beta[2, ])
var(beta_mat1[2, ])

#### Q9. ####
x <-  Data$educ

#### Q10. ####
b0 <- beta0 #intercept

b1 <- beta1 #slope parameter


#### Q11. ####
# simulate 1000 new realisations and save the estimated coefficients for all of them
beta<-matrix(0,2,1000)
for (i in 1:1000){
  set.seed(i)
  u = rnorm(526,0,10)
  y = b0 + b1*x + u
  beta[,i]<-ols(y,x)
}


#### Q12. ####   
c <- c(mean(beta[1,]), mean(beta[2,]))


#### Q13. ####
betahat <-matrix(0,2,1000)
for (i in 1:1000){
  set.seed(i)
  u = rnorm(1000,0,10)
  x = rnorm(1000,5,10)
  y = x + u
  betahat[,i]<- ols(y,x)
}

betatilde <-matrix(0,2,1000)
for (i in 1:1000){
  set.seed(i)
  u = rnorm(1000,0,10)
  x = rnorm(1000,5,10 )
  y = x + u
  betatilde[,i]<- mean(y)/mean(x)
}


#### Q14. ####
var1 <-  var(betahat[2,])
var2 <-  var(betatilde[2,])

#### Q15. ####
eff <-"OLS"

