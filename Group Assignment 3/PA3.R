### Group Matrix ###

# Remove all variables 
rm(list = ls())

# Set working directory
#setwd("/Users/hoikwan/Documents/University /MSc/Modules/ECON61001 Econometric Methods/Assessment/Problem Assignments/PA3")

#### Q1. ####
Data <- read.csv("wage.csv") 

#### Q2. ####
y = Data$wage

x2 = Data$educ

x3 = Data$exper

x4 = Data$female

#### Q3. ####
mod1 <-  lm(y ~ x2 +x3)

R2 <-  summary(mod1)$r.squared

#### Q4. ####
rho_y2 <-  cor(y,x2)


#### Q5. ####
reg2 <-  lm(y ~ x2)
reg32 <- lm(x3 ~ x2)

e2 <- reg2[["residuals"]]
e32 <- reg32$residuals

rho_y32 <-  cor(e2,e32)

#### Q6. ####
ls <- 1- R2

rs <-  (1-(rho_y2)^2) * (1-(rho_y32)^2)

#### Q7. ####
mod2 <-  lm(y ~ x2 +x3 +x4)

ls2 <- 1 - summary(mod2)$r.squared

#### Q8. ####
rho_4 <- cor(y,x4)

#### Q9. ####
reg4 <- lm(y ~x4)
reg34 <-lm(x3 ~ x4)

e4 <- reg4$residuals
e34 <- reg34$residuals

rho_y34 <-  cor(e4,e34)


#### Q10. ####
regy34 <- lm(y ~ x3 + x4)
reg234 <-lm(x2 ~ x3 + x4)

ey34 <- regy34$residuals
e234 <- reg234$residuals

rho_234 <- cor(ey34, e234)

#### Q11. ####
rs2 <- (1 - rho_4*rho_4)*(1 - rho_y34*rho_y34)*(1- rho_234*rho_234)

##### Q12. ####
t1 <-  (tstat = (mod2[["coefficients"]][["x4"]])/(summary(mod2)$coefficients["x4","Std. Error"]))

cv1 <- qt(0.05, 522, lower.tail = TRUE)

pv1 <- pt(t1, 522, lower.tail = TRUE)

##### Q13. ####
t2 <-  (tstat = (mod2[["coefficients"]][["x4"]] - (-2) )/(summary(mod2)$coefficients["x4","Std. Error"]))

cv2 <- qt(1- 0.01, 522, lower.tail = FALSE)

pv2 <- 1- pt(t2, 522, lower.tail = TRUE)


#### Q14. ####
Ew_female = sum(mod2$coefficients*c(1,12,3,1))

#### Q15. ####
Ew_male = sum(mod2$coefficients*c(1,12,3,0))











