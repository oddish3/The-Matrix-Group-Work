#### Group matrix ####

rm(list =ls())

library(wooldridge)

### Q1 ####
df <- kielmc


#### Q2 ####
df_78 <- df[df$year == 1978, c('rprice', 'nearinc')]


#### Q3 ####
df_81 <- df[df$year == 1981, c('rprice', 'nearinc')]



#### Q4 ####
reg_78 <- lm(rprice ~ nearinc, data = df_78)

b_78 <-  reg_78$coefficients


#### Q5 ####
mun78 <- sum(reg_78$coefficients*c(1,1))
# mun <- mean(df_78$rprice[df_78$nearinc == 1])
 # Average real price is regression when 'nearinc' = 1 

#### Q6 ####
mu78 <- sum(reg_78$coefficients*c(1,0))
# mun <- mean(df_78$rprice[df_78$nearinc == 0])
 #Average real price is regression when 'nearinc' = 0 


#### Q7 ####
# The difference in the average real prices is coefficient of 'nearinc'
delta_78 <-  mun78 - mu78


#### Q8 ####
reg_81 <- lm(rprice ~ nearinc, data=df_81)

b_81 <-  reg_81$coefficients



#### Q9 ####
mun81 <- sum(reg_81$coefficients*c(1,1))
# mun <- mean(df_81$rprice[df_81$nearinc == 1])


#### Q10 ####
mu81 <- sum(reg_81$coefficients*c(1,0))
# mun <- mean(df_81$rprice[df_81$nearinc == 0])


#### Q11 ####
delta_81 <-  mun81 - mu81


#### Q12 ####
delta1 <-  delta_81 - delta_78


#### Q13 ####
reg1 <- lm(rprice ~ y81 + nearinc + y81nrinc, data=df)

beta <- reg1$coefficients

#### Q14 ####
tstat <-  (reg1$coefficients[4]) / (summary(reg1)$coefficients["y81nrinc" , "Std. Error" ])


#### Q15 ####
cv <-  qt(0.10, 317, lower.tail = TRUE) # Rejected at 10% s.f.but not at 5% (what is appropriate level?)

pvalue <- pt(tstat, 317, lower.tail = TRUE)


#### Q16 ####
reject <-  TRUE


