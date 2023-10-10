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
reg1 <- lm(wage ~ educ, data=Data)

beta0 <- reg1$coefficients[1] #intercept

beta1 <- reg1$coefficients[2] #slope parameter


#### Q6. ####
Data_New = Data[-row(Data) [Data$educ == 0],]



#### Q7. ####
reg2 <- lm(log(wage) ~ log(educ), data = Data_New)

delta0 <-  reg2$coefficients[1]

delta1 <- reg2$coefficients[2]


#### Q8.####
set.seed(42)
n=526
x<-rnorm(n,5,4)
u = rnorm(n,0,0.5)
b0 = 1 # Population intercept
b1 = -3 # Population slope


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
  x<-rnorm(n,5,4)
  u = rnorm(n,0,0.5)
  y = b0 + b1*x + u
  beta[,i]<-ols(y,x)
}

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
c <- mean(beta)


#### Q13. ####
b1pop =1 
betahat <-matrix(0,2,1000)
for (i in 1:1000){
  set.seed(i)
  u = rnorm(1000,0,10)
  x = rnorm(1000,5,10 )
  y = b1pop*x + u
  betahat[,i]<-ols(y,x)
}

betatilde <-matrix(0,2,1000)
for (i in 1:1000){
  set.seed(i)
  u = rnorm(1000,0,10)
  x = rnorm(1000,5,10 )
  y = b1pop*x + u
  betatilde[,i]<- mean(y)/mean(x)
}

#### Q14. ####
var1 <-  var(betahat)
var2 <-  var(betatilde)


#### Q15. ####
eff <-  function(x){
  d1 = dim(x)[1]
  d2 = dim(x)[2]
  if (d1==d2){
    disp = "square"
  }
  else disp = "not square"
  
  return(disp)
}