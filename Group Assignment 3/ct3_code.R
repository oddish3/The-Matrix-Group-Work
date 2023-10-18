
########################################################################
###                 Power properties of tests                        ###
########################################################################
rm(list = ls()) # clean the global environment
dev.off() # erase all the previous plots
# press ctrl+L to erase text in console

# simulate a linear regression model
set.seed(42) # for replicability
n=500 # sample size
x<-rnorm(n,5,4) # note the second argument in simulating a random normal variable is std
u = rnorm(n,0,0.5)
b0 = 1
b1 = -3
y = b0 + b1*x + u #simulated linear regression model

ols = lm(y~x) # estimated linear regression model
bhat = ols$coefficients[2] # estimated slope coefficient
t = (bhat +3)/sqrt(vcov(ols)[2,2]) # Student t-test
rj = abs(t)>qt(0.975, n - 2) # rejection rule

# in order to check whether the test is correctly sized we need to repeat the simulation "many" 
# times and see whether the proportion of rejections corresponds to the nominal significance level

# simulate 1000 new datasets and save the test for all of them
rj<-matrix(0,1,1000)
for (i in 1:1000){
  set.seed(i)
  x<-rnorm(n,5,4)
  u = rnorm(n,0,0.5)
  y = b0 + b1*x + u
  ols = lm(y~x)
  bhat = ols$coefficients[2]
  t = (bhat +3)/sqrt(vcov(ols)[2,2])
  rj[i] = abs(t)>qt(0.975, n - 2)
}
# note for a binary rejection variable, the mean corresponds to the share of ones
mean(rj)
# thus empirical size is very close to the nominal 5% level (Type I error)

# we now can examine the relationship between the size of the test and the sample size
# it is convenient to define a FUNCTION which returns the size of the test depending on parameters
rejH0 <- function(n,theta,mc,su){ 
  rj<-matrix(0,1,mc)
  for (i in 1:mc){
    set.seed(i)
    x<-rnorm(n,5,4)
    u = rnorm(n,0,sqrt(su))
    y = b0 + b1*x + u
    ols = lm(y~x)
    bhat = ols$coefficients[2]
    t = (bhat - theta)/sqrt(vcov(ols)[2,2])
    rj[i] = abs(t)>qt(0.975, n - 2)
  }
  return(rj)
}
#check with the old parameter constellations whether the function is correct:
mean(rejH0(500,-3,1000,0.25))

#now examine the null rejection probability at the true parameter value 
#as a function of sample size
ngrid = seq(100,5000,length=10)
rj_n<-matrix(0,1,10)
for (j in 1:10){
  rj_n[j] <-mean(rejH0(ngrid[j],-3,1000,0.25))
}
plot(ngrid, rj_n, main = 'Empirical Size', 
     xlab = 'Sample Size [n]', ylab = 'empirical H0 rejection',col = 'blue', lwd = 3)
abline(h=0.05, col="black", lwd=3)

#now examine the null rejection probability at the true parameter value 
#as a function of Monte Carlo iterations

#compute the 5000 null rejection probabilites
rj<- rejH0(500,-3,10000,0.25)
# with the cumsum function we can compute the moving average null rejection probability

x<-cumsum(rj)/seq(1,length(rj))

dev.off()

#plot the moving average
plot(seq(1,length(rj)), x, main = 'Empirical Size', 
     xlab = 'Simulation Draws [MC]', ylab = 'empirical H0 rejection',col = 'blue', lwd = 2)
abline(h=0.05, col="black", lwd=3)

#now examine the null rejection probability at the true parameter value 
#as a function of the error term variance

sugrid = seq(0.1,10,length=15)
rj_u<-matrix(0,1,15)
for (j in 1:15){
  rj_u[j] <-mean(rejH0(1000,-3,1000,sugrid[j]))
}

dev.off() 
#plot the simulation result
plot(sugrid, rj_u, main = 'Empirical Size', 
     xlab = 'Error Term Variance', ylab = 'empirical H0 rejection',col = 'blue', lwd = 3)
abline(h=0.05, col="black", lwd=3)

dev.off()

# b) ########################################################################
# we now can examine the power of the of the test 
# we can use the same function to compute the null rejection probability
pw<- mean(rejH0(500,-3,1000,0.25))
# the Type II error might be checked by the rejection probabiliteies over the frid of b* values

bgrid = seq(-3.2,-2.8,length=21)
pw1<-matrix(0,1,length(bgrid))
pw2<-pw1
for (j in 1:length(bgrid)){
  pw1[j] <-mean(rejH0(500,bgrid[j],1000,10))
  pw2[j] <-mean(rejH0(1000,bgrid[j],1000,10))
}

par(mfrow=c(1,1))
plot(bgrid, pw1, type = "l", lty = 1, main = 'Power Curves', 
     xlab = 'beta', ylab = 'empirical H0 rejection',col = 'blue', lwd = 3)
lines(bgrid, pw2, type = "l", lty = 2, col = 'red', lwd = 3)
legend(x = -3.2, y = 0.5, 
       legend = c("n = 500", "n = 1000"), 
       lty = c(1,2), col = c('blue','red'), lwd = c(2,2))

########################################################################
###                 Restricted least squares                        ###
########################################################################
rm(list = ls()) # clean the global environment
dev.off() # erase all the previous plots
# press ctrl+L to erase text in console

# a) ########################################################################

# simulate a linear regression model
set.seed(42) # for replicability
n=500 # sample size
x<-rnorm(n,5,4) # note the second argument in simulating a random normal variable is std
u = rnorm(n,0,0.5)
b0 = 1
b1 = -3
y = b0 + b1*x + u #simulated linear regression model

# first define a funciton for the residual sum of squares (use ct2 code)
rss <- function(b0,b1){
  uhat = y-b0-b1*x
  ss = sum(uhat^2)
  return(ss)
}

# create a grid of parameter values for 3D plotting
a0 <- seq(-6, 6, by= 0.1)
a1 <- a0
z <- outer(a0, a1, Vectorize(rss))

# b) ########################################################################

# select those who satisfy the restirction
#install.packages("pracma", repos="http://R-Forge.R-project.org")
library(pracma)
A = meshgrid(a0,a1)
idx = A$X+A$Y==0

# Library upload (not, you might need to install the plotly package first)
# install.packages("plotly")
library(plotly)

# Plot the RSS surface
axx <- list(title = "alpha 0")
axy <- list(title = "alpha 1")
axz <- list(title = "RSS")
p <- plot_ly(x = a0, y = a1, z = z, type = "surface")
p %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis = axz)) %>%
  add_trace(x = A$X[idx], y = A$Y[idx], z = z[idx], mode = "markers", type = "scatter3d", 
          marker = list(size = 5, color = "red", symbol = 104))

# c) ########################################################################
#  from previous tutorial copy a function which solves a bivariate OLS problem
ols<-function(y,x){
  ones <- rep(1,length(x))
  X = matrix(c(ones, x), ncol = 2)
  beta = solve(t(X) %*% X) %*% t(X) %*% as.matrix(y)
  return(beta)
}

# rewirte the restriction in the matrix form
R=matrix(c(1,1),1,2)
r=0

# try it out
ones <- rep(1,length(x))
X = matrix(c(ones, x), ncol = 2)
xx = solve(t(X) %*% X)
brls = ols(y,x) - xx%*%t(R)%*%solve(R%*%xx%*%t(R))%*%(R%*%ols(x,y)-r)

# we can use it in the new RLS funciton
rls<- function(y,x,R,r){
  ones <- rep(1,length(x))
  X = matrix(c(ones, x), ncol = 2)
  xx = solve(t(X) %*% X)
  bhat = ols(y,x)
  brls = bhat - xx%*%t(R)%*%solve(R%*%xx%*%t(R))%*%(R%*%bhat-r)
  return(brls)
}

# check with different restrictions
rls(y,x,matrix(c(1,-1),1,2),0) # imposes b0=-b1
rls(y,x,matrix(c(1,1),1,2),-2) # is the restiction from the DGP
rls(y,x,matrix(c(1,1),1,2),9)









