#programming Assignment 1 
rm(list = ls())
setwd("C:/R folder/tmp/myrepo")

#Question 1
A = matrix(c(1, 2, 3, 3, 2, 1, -3, -6, -9), nrow = 3, ncol = 3)
A

B = matrix(c(1, 2, 3, 3, 2, 1), nrow = 3)
B

Bt = t(B)
Bt
B

a11 = Bt[1,1] * B[1,1] + Bt[1,2] * B[2,1] + Bt[1,3] * B[3,1]
a12 = Bt[1,1] * B[1,2] + Bt[1,2] * B[2,2] + Bt[1,3] * B[3,2]
a21 = Bt[2,1] * B[1,1] + Bt[2,2] * B[2,1] + Bt[2,3] * B[3,1]
a22 = Bt[2,1] * B[1,2] + Bt[2,2] * B[2,2] + Bt[2,3] * B[3,2]

C = matrix(c(a11, a12, a21, a22),nrow = 2)
C
