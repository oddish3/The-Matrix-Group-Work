#Programming Assignment 2
rm(list=ls())
#setwd("C:/R folder/tmp/myrepo/Group Assignment 2") ran in console

#library to read csv file
library(readr)

#1. Importing the data
Data <- read_csv("wage.csv")

#2. Sample Average of Educ
educbar <- mean(Data$educ)

#3.sample average of educ conditional on educ = 12/13
w12 = mean(Data$wage[Data$educ==12])
w13 = mean(Data$wage[Data$educ==13])

#4. rel diff
alpha1 = ((w13-w12)/w12)*100

#5. linear regression model 
Data$wage1<-log(Data$wage) #log of wages

model1 <- lm(wage1 ~ educ, data = Data)

beta0 <- model1[["coefficients"]][["(Intercept)"]]
beta0
beta1 <- model1[["coefficients"]][["educ"]]
beta1

#6. remove individuals with no educ

Data_New = Data[-row(Data)[Data$educ == 0],] #only 2 obs removed?
unique(Data$educ) #no missing

#7. using data without 0 educ

model1 <- lm(wage1 ~ educ, data = Data_New)

delta0 <- model1[["coefficients"]][["(Intercept)"]]
delta1 <- model1[["coefficients"]][["educ"]]

#8. Defining a function to compute OLS solution to model (tut 2)

set.seed(1) # try different numbers here and you will see different results
n=500
x<-rnorm(n,5,4)
u = rnorm(n,0,0.5)
b0 = 1
b1 = -3

y = b0 + b1*x + u

ols<-function(y,x){
  ones <- rep(1,length(x))
  X = matrix(c(ones, x), ncol = 2)
  beta = solve(t(X) %*% X) %*% t(X) %*% as.matrix(y)
  return(beta)
}

ols(y,x) #CLT - close to values

# simulate 1000 new datasets and save the estimated coefficients for all of them
beta<-matrix(0,2,1000)
for (i in 1:1000){
  set.seed(i)
  x<-rnorm(n,5,4)
  u = rnorm(n,0,0.5)
  y = b0 + b1*x + u
  beta[,i]<-ols(y,x)
}

#9. define x using the original dataset to fix regressor x = educ

x= Data$educ

#10. define b0 and b1 to be beta0 and beta1
b0 <- beta0
b1 <- beta1

#11. 1000 realisations 
beta<-matrix(0,2,526)
for (i in 1:526){
  set.seed(i)
  #x<-rnorm(n,5,4)
  u = rnorm(526,0,10) #changed to SD = 10
  y = b0 + b1*x + u
  beta[,i]<-ols(y,x)
}

#12. saved into row vector c (not sure)

C1 = mean(beta[1,])
C2 = mean(beta[2,])
C = matrix(c(C1,C2), ncol =2)
C
