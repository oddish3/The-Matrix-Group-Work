### Group Matrix ###

# Remove all variables 
rm(list = ls())

# Set working directory
 setwd("/Users/hoikwan/Documents/University /MSc/Modules/ECON61001 Econometric Methods/Assessment/Problem Assignments/PA3")

#### Q1. ####
Data <- read.csv("wage.csv") 


#### Q2. ####
y = Data$wage

x2 = Data$educ

x3 = Data$exper

x4 = Data$female


#### Q3. ####
reg1 <-  lm(y ~ x2 +x3)

R2 <-  summary(reg1)$r.squared

#### Q4. ####
rho_y2 <- cor(y, x2)

#### Q5. ####
reg2 <-  lm(y ~ x2)
reg3 <- lm(x3 ~ x2)

e2 <- reg2$residuals
e3 <- reg3$residuals

rho_y32 <-  cor(e2,e3)

#### Q6. ####
ls <- 1- R2

rs <-  (1-(rho_y2)^2) * (1-(rho_y32)^2)


#### Q7. ####
reg4 <-  lm(y ~ x2 +x3 +x4)

ls2 <- 1 - summary(reg4)$r.squared


#### Q8. ####
rho_4 <- cor(y,x4)


#### Q9. ####



#### Q10. ####



#### Q11. ####



##### Q12. ####
t1 <-  (tstat = (reg4$coefficients["x4"])/(summary(reg4)$coefficients["x4","Std. Error"]))

cv1 <- qt(0.05, 522, lower.tail = FALSE )

pv1 <- pt(t1, 522, lower.tail = TRUE )

##### Q13. ####
t2 <-  (tstat = (reg4$coefficients["x4"] - (-2) )/(summary(reg4)$coefficients["x4","Std. Error"]))

cv2 <- qt(0.01, 522, lower.tail = TRUE)

pv2 <- pt(t2, 522, lower.tail = TRUE)


#### Q14. ####
Ew_female = sum(reg4$coefficients*c(1,12,3,1))



#### Q15. ####
Ew_male = sum(reg4$coefficients*c(1,12,3,0))











