# Q1
Data = read.csv('cons.csv')
# Q2
ols<-function(y,x){
  ones <- rep(1,length(x))
  X = matrix(c(ones, x), ncol = 2)
  beta = solve(t(X) %*% X) %*% t(X) %*% as.matrix(y)
  return(beta)
}
# Q3
b0 = 0.42
b1 = 0.95
n = 198
x = log(Data$di[-199])
xT = log(Data$di[199])
# Q4
beta = matrix(0,2,1000)
e = matrix(0,n,1000)
sigma_hat2 = matrix(0,1,1000)
w0 = matrix(0,1000,1)
w1 = matrix(0,1000,1)
w2 = matrix(0,1000,1)
for (i in 1:1000){
  set.seed(i)
  u = matrix(rnorm(n,0,1),n,1)
  y = b0 + b1*x + u
  beta[,i] = ols(y,x)
  e[,i] = y - beta[1,i] - beta[2,i]*x
  sigma_hat2[i] = sum(e[,i]^2)/(n-2)
  w0[i] = exp(b0+b1*xT+0.5)
  w1[i] = exp(beta[1,i]+beta[2,i]*xT)
  w2[i] = exp(beta[1,i]+beta[2,i]*xT+0.5*sigma_hat2[,i])
}
# Q5
bias = matrix(0,2,1)
bias[1] = mean(w1 - w0)
bias[2] = mean(w2 - w0)
# Q6
y = Data$c
ld = log(Data$di)
ldT = log(388804.1)
# Q7
beta_test = ols(y,ld)
yT = beta_test[1] + beta_test[2]*ldT
# Q8
ly = log(Data$c)
beta_test2 = ols(ly,ld)
#error = matrix(0,length(ly),1)
error = ly - beta_test2[1] - beta_test2[2]*ld
sig = sum(error^2)/(length(ly)-2)
# Q9
yN = exp(beta_test2[1] + beta_test2[2]*ldT)
# Q10
yUB = exp(beta_test2[1] + beta_test2[2]*ldT + 0.5*sig)
# Q11
mc = 10000
N = 100
Beta = matrix(0,2,mc)
var_delta0 = matrix(0,1,mc)
var_delta1 = matrix(0,1,mc)
y1 = matrix(0,mc,1)
y2 = matrix(0,mc,1)
for (i in 1:mc){
  set.seed(i)
  U = matrix(rnorm(N,0,sqrt(10)),N,1)
  X = matrix(rnorm(N,5,4),N,1)
  Y = 1 - 3*X + U
  Beta[,i] = ols(Y,X)
  XX = matrix(c(rep(1,N), X), ncol = 2)
  var_Beta = 10 * solve(t(XX) %*% XX)
  var_delta0[i] = var_Beta[1,1]
  var_delta1[i] = var_Beta[2,2]
  y1[i] = Beta[1,i] - 1 / sqrt(var_delta0[i])
  y2[i] = Beta[2,i] + 3 / sqrt(var_delta1[i])
}
# Q12
mc = 10000
N = 100
Beta = matrix(0,2,mc)
varhat_delta0 = matrix(0,1,mc)
varhat_delta1 = matrix(0,1,mc)
res = matrix(0,N,mc)
SigmaN_hat2 = matrix(0,1,mc)
z1 = matrix(0,mc,1)
z2 = matrix(0,mc,1)
for (i in 1:mc){
  set.seed(i)
  U = matrix(rnorm(N,0,sqrt(10)),N,1)
  X = matrix(rnorm(N,5,4),N,1)
  Y = 1 - 3*X + U
  Beta[,i] = ols(Y,X)
  XX = matrix(c(rep(1,N), X), ncol = 2)
  res[,i] = Y - Beta[1,i] - Beta[2,i]*X
  SigmaN_hat2[i] = sum(res[,i]^2)/(N-2)
  varhat_Beta = SigmaN_hat2[i] * solve(t(XX) %*% XX)
  varhat_delta0[i] = varhat_Beta[1,1]
  varhat_delta1[i] = varhat_Beta[2,2]
  z1[i] = Beta[1,i] - 1 / sqrt(varhat_delta0[i])
  z2[i] = Beta[2,i] + 3 / sqrt(varhat_delta1[i])
}
