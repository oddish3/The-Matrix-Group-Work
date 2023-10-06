###############################

# GROUP: MATRIX
# Member:
# - Chinh Hoang Duc: 11335127 - hoang.chinh@postgrad.manchester.ac.uk
# - Sol Yates: 10710007 - solomon.yates@student.manchester.ac.uk
# - Sicong Liu: 10850471 - email: sicong.liu@postgrad.manchester.ac.uk
# - Hoi Kwan Cheung: 10704589 - hoikwan.cheung@postgrad.manchester.ac.uk
# - Orejesu Ojutiku: 11465531 - orejesu.ojutiku@postgrad.manchester.ac.uk

############################### 

rm(list = ls())

# setwd("C:\\Users\\kingd\\OneDrive\\D\\DOCUMENTS\\Uni of Man\\Year 0\\ECON 61001 Metrics Method\\Assignments\\R-assignments")

# 1. Make a matrix called A ####

a1 <- c(1, 2, 3)
a2 <- c(3, 2, 1)
a3 <- c(-3, -6, -9)

A <- matrix(c(a1, a2, a3), nrow = 3, ncol = 3)

# 2. Make a matrix called C ####

b1 <- c(1, 2, 3)
b2 <- c(3, 2, 1)
B <- matrix(c(b1, b2), nrow = 3, ncol = 2)

C <- t(B) %*% B
C 

# 3. Calculate the determinant of matrix C, save as 'dt' ####

dt <- det(C)
dt

# 4. Calculate the inverse of C, save as D ####

D <- solve(C)
D

# 5. Calculate the trace of D, save as 'tr' ####

tr <- sum(diag(D))
tr

# 6. Define 'issquare' ####

issquare <- function(M) {
  if (nrow(M) == ncol(M)) {
    result = "square"
  } else {
    result = "not square"
  }
  return(result)
}

# is_square <- function(M) {
#   if (nrow(M) == ncol(M)) {
#     result = "The matrix M is square!"
#   } else {
#     result = paste("The function M is not square! M is a", nrow(M), "X", ncol(M), "matrix!")
#   }
#   return(result)
# }

issquare(A)
issquare(B)
#is_square(A)
#is_square(B)

# 7. Create vector 'x'. Create 'y'

x <- seq(from = -10, to = 10, by = 0.5)

y <- x^2

# 8. Create vector 'dx' and 'dy'

dx <- c()
i = 1

while(i <= length(x)) {
  dxi <- x[i] - x[i-1]
  dx <- c(dx, dxi)
  i <- i+1
}

dy <- c()
i = 1

while(i <= length(y)) {
  dyi <- y[i] - y[i-1]
  dy <- c(dy, dyi)
  i <- i+1
}

# 9. Compute the derivative, save as 'd'

d <- dy / dx
d

rm(list = ls())
#source("PA1.R")

