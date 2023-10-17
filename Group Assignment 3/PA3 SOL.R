rm(list=ls())
#setwd("C:/R folder/tmp/myrepo/Group Assignment 3")

# 1
Data = read.csv("wage.csv")

#2
y = Data$wage
x2 = Data$educ
x3 = Data$exper
x4 = Data$female

#3
model1 = lm(y ~ x2 + x3)
R2 = summary(model1)$r.squared

#4 sample correlation
rh0_y2 = cor(y, x2)

#5
e2 = model1[["residuals"]]

model2 = lm(x3 ~ x2)
e32 = model2[["residuals"]]

rho_y32 = cor(e2, e32)

#6
ls = 1 - R2
rs = (1 - R2)*(1 - rho_y32)
setequal(ls, rs)

#7
model3 = lm(y ~ x2 + x3 + x4)
R21 = summary(model3)$r.squared
ls2 = 1 - R21

#8
rho_4 = R21

#9
e23 = model3[["residuals"]]

model4 = lm(x3 ~ x4)
e32 = model4[["residuals"]]

rho_34 = cor(e23, e32)

#10
model5 = lm(x2 ~ x3 + x4)
e234 = model5[["residuals"]]
rho_234 = cor(e23, e234)

#11 
rs2 = (1 - R21)*(1 - rho_34)*(1 - rho_234)
#setequal(ls2, rs2) #very similar but not equal hmmm

d3 = model3[["model"]][["x4"]]

test1 = t.test(d3, mu = 0, alternative = "less", conf.level = 0.95)
 
t1 = test1[["statistic"]][["t"]]

