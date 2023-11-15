######## Group matrix ########

rm(list = ls())
### Q1. ####
## Simulating the AR1 process with function:
ar1 <- function(c,rho, T){
  for (i in 1:T) {
  set.seed(i)
  u = matrix(0,T,1)
  e = rnorm(i)
  # specify a starting value
  u[1] = e[1]
}
  #Compute the rest of the process iteratively
  for (t in 2:T) {
    u[t] = c + rho*u[t-1] +e[t]
  }
  return(u)
}


u = ar1(0,0.8,500)



### Q2, ###
v = ar1(0,1,500)


library(tseries)


### Q3. ###
pu <-  adf.test(u)$p.value
pv <-  adf.test(v)$p.value


### Q4. ###
data(USeconomic)
TGNP <-  length(GNP)


### Q5. ###
p1 <-  adf.test(GNP)$p.value


### Q6. ###
dGNP <-  diff(GNP)

### Q7. ###
p2 <-  adf.test(dGNP)$p.value

