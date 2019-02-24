###############################################################
# Title:        ps2.r
# Author:       Priyanshi Khatri (pxk173330)
# Date:         2018-02-19
# Description:  Turn-in product for problem set 2
###############################################################

###############################################################
##Question 1
rm(list=ls(all=TRUE))

## Import packages
library(data.table)

## Data import and validation
context1    <- fread('attend.csv')
summary(context1)

## Generate new variable
context1$attendrt    <- context1$attend/32
context1$hwrt        <- context1$hw/8

## Run model
model1      <- lm(termGPA~priGPA+ACT+attendrt+hwrt, data=context1)

## Summarize
summary(model1)

# Call:
#   lm(formula = termGPA ~ priGPA + ACT + attendrt + hwrt, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.87210 -0.28100  0.00001  0.30164  1.49711 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.286983   0.164169  -7.839 1.77e-14 ***
#   priGPA       0.548962   0.042418  12.942  < 2e-16 ***
#   ACT          0.036099   0.006051   5.966 3.92e-09 ***
#   attendrt     1.052246   0.155436   6.770 2.81e-11 ***
#   hwrt         0.913031   0.116932   7.808 2.22e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4788 on 675 degrees of freedom
# Multiple R-squared:   0.58,	Adjusted R-squared:  0.5775 
# F-statistic:   233 on 4 and 675 DF,  p-value: < 2.2e-16


##Question 1
## a) Every 50% increase in attendrt is associated with 0.526 increase in GPA for 
#     the term controlling for priGPA, ACT score and homework completion rate
## b) Every 50% increase in hwrt is associated with 0.4565 increase in GPA for the term 
#     controlling for priGPA, ACT score and attendance rate
## c) To calculate predictions
pred1  <- c(1,2.2,32,(28/32),(8/8))
sum(pred1*coef(model1))
#Predicted Value of Term GPA is 2.909664
## d) To calculate predictions
pred2  <- c(1,3.9,20,(28/32),(8/8))
sum(pred2*coef(model1))
#Predicted Value of Term GPA is 3.409706
## e) PriGPA is more important to termGPA intuitively than ACT score as it has
#     higher coefficient(0.548962 > 0.036099)
## f) To calculate predictions
pred1  <- c(1,3,25,(32/32),(4/8))
sum(pred1*coef(model1))
#Predicted Value of Term GPA is 2.771152
## 8) To calculate predictions
pred1  <- c(1,3,25,(16/32),(8/8))
sum(pred1*coef(model1))
##Predicted Value of Term GPA is 2.701545
## h) Attendance rate is more important to termGPA than homework completion rate
#     as attendrt has higher coefficient than hwrt(1.052246 > 0.913031)
## i) It is easier to compare attendrt and hwrt as they are on same scale 0-1,
#     whereas priGPA and ACT score are on different scales.

###############################################################
##Question 2
rm(list=ls(all=TRUE))

## Data import and validation
context2    <- fread('CEOSAL2.csv')
summary(context2)

## Run model
model2      <- lm(log(salary)~log(mktval)+profits+ceoten, data=context2)
model3      <- lm(log(salary)~log(mktval)+profits+ceoten+log(sales), data=context2)

## Summarize
summary(model2)

# Call:
#   lm(formula = log(salary) ~ log(mktval) + profits + ceoten, data = context2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.63382 -0.34660  0.00627  0.35059  1.96220 
# 
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.7095052  0.3954502  11.909  < 2e-16 ***
# log(mktval) 0.2386220  0.0559166   4.267 3.25e-05 ***
# profits     0.0000793  0.0001566   0.506   0.6132    
# ceoten      0.0114646  0.0055816   2.054   0.0415 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5289 on 173 degrees of freedom
# Multiple R-squared:  0.2514,	Adjusted R-squared:  0.2384 
# F-statistic: 19.36 on 3 and 173 DF,  p-value: 7.141e-11

summary(model3)

# Call:
#   lm(formula = log(context1$salary) ~ log(context1$mktval) + profits + 
#        ceoten + log(sales), data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.48792 -0.29369  0.00827  0.29951  1.85524 
# 
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          4.558e+00  3.803e-01  11.986  < 2e-16 ***
# log(mktval)          1.018e-01  6.303e-02   1.614   0.1083    
# profits              2.905e-05  1.503e-04   0.193   0.8470    
# ceoten               1.168e-02  5.342e-03   2.187   0.0301 *  
#   log(sales)           1.622e-01  3.948e-02   4.109 6.14e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5062 on 172 degrees of freedom
# Multiple R-squared:  0.3183,	Adjusted R-squared:  0.3024 
# F-statistic: 20.08 on 4 and 172 DF,  p-value: 1.387e-13

##Question 2 Interpretations
## a) We did not take log of profits because there are some values of profit which
#     are negative and log is not valid for negative numbers.
## b) Every 1% increase in market value is associated with 0.24% increase in 
#     salary controlling for profits and ceoten(years as ceo with company)
## c) Every 1% increase in market value is associated with 0.1018%
#     increase in salary controlling for profits, ceoten and sales
## d) Since cor(log(context2$mktval),log(context2$sales)) = 0.7359232, as correlation
#     is high log(mktval) becomes insignificant in model 3 because log(sales) is 
#     included in that model as compared to model 2 where log(mktval) is significant
#     because log(sales) is absent in model 2. This is Omitted-variable bias(OVB)
## e) Since p value of profits in model 3(0.8470) is not less than 0.05, therefore it is
#     NOT significant
## f) Every 1% increase in sales of the firm is associated with 0.1622% increase
#     in salary controlling for log mktval, profits and ceoten

###############################################################
##Question 3
rm(list=ls(all=TRUE))

## Data import and validation
context3    <- fread('hprice1.csv')
summary(context3)

## Run model
model4      <- lm(price~bdrms+log(lotsize)+log(sqrft)+colonial, data=context3)
model5      <- lm(log(price)~bdrms+log(lotsize)+log(sqrft)+colonial, data=context3)

## Summarize
summary(model4)

# Call:
#   lm(formula = price ~ bdrms + log(lotsize) + log(sqrft) + colonial, 
#      data = context3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -109.603  -38.258   -4.325   22.984  220.766 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2030.455    210.967  -9.625 3.68e-15 ***
#   bdrms           18.572      9.308   1.995   0.0493 *  
#   log(lotsize)    61.446     12.372   4.966 3.60e-06 ***
#   log(sqrft)     225.508     30.072   7.499 6.41e-11 ***
#   colonial         4.134     14.509   0.285   0.7764    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 59.66 on 83 degrees of freedom
# Multiple R-squared:  0.6781,	Adjusted R-squared:  0.6626 
# F-statistic: 43.71 on 4 and 83 DF,  p-value: < 2.2e-16

summary(model5)

# Call:
#   lm(formula = log(price) ~ bdrms + log(lotsize) + log(sqrft) + 
#        colonial, data = context3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.69479 -0.09750 -0.01619  0.09151  0.70228 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.34959    0.65104  -2.073   0.0413 *  
#   bdrms         0.02683    0.02872   0.934   0.3530    
# log(lotsize)  0.16782    0.03818   4.395 3.25e-05 ***
#   log(sqrft)    0.70719    0.09280   7.620 3.69e-11 ***
#   colonial      0.05380    0.04477   1.202   0.2330    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1841 on 83 degrees of freedom
# Multiple R-squared:  0.6491,	Adjusted R-squared:  0.6322 
# F-statistic: 38.38 on 4 and 83 DF,  p-value: < 2.2e-16

##Question 3 Interpretations
## a) Every 1% sqrft increase in lot size is associated with $61.44k increase in 
#     price of the house controlling bedrooms, size of house and colonial
## b) Every 1% sqrft increase in lot size is associated with 0.168% increase in 
#     price of house controlling for bedrooms, size of house and colonial
## c) Every home belonging to colonial style is associated with $4.134k increase
#     in price of house controlling for bedrooms, size of house and size of lot
## d) Model 4 better fits the data for this data set as R squared for model 4 is
#     0.68 which is greater than R squared for model 5 which is 0.65, more closer 
#     the value of R squared to 1, better the model fits the data
## e) As per model 4, increase in price can be calculated as =
#     bdrms coeff*(no of bdrms) + log(sqrft) coeff*(increase in sqrft)  
#     = 18.572 * 1 + 225.508 * 0.10
#     = 41.1228
#     New price of house = 300 + 41.1228 = $341.1228k
#     Cost of expansion is $50k and additional space payment is $20k
#     we want a return of $350k -$20K = $330k
#     Our estimated value is $341k which is greater than $330k
#     So, we should pursue the expansion.

###############################################################
##Question 4
rm(list=ls(all=TRUE))

## Data import and validation
context4    <- fread('JTRAIN2.csv')
summary(context4)

## Run model
model6      <- lm(re78~re75+train+educ+black, data=context4)

##Summarize
summary(model6)

# Call:
# lm(formula = re78 ~ re75 + train + educ + black, data = context4)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -9.120 -4.377 -1.756  3.353 54.058 
# 
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  1.97686    1.89028   1.046   0.2962   
# re75         0.14697    0.09811   1.498   0.1349   
# train        1.68422    0.62700   2.686   0.0075 **
# educ         0.41026    0.17267   2.376   0.0179 * 
# black       -2.11277    0.82941  -2.547   0.0112 * 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 6.496 on 440 degrees of freedom
# Multiple R-squared:  0.04917,	Adjusted R-squared:  0.04053 
# F-statistic: 5.688 on 4 and 440 DF,  p-value: 0.00018

##Question 4 Interpretations
## a) Though re75 is not significant according to the model6 as p value is > 0.05 
#     Every 1 thousand dollar increase in real earnings in 1975 is associated with
#     $1469.7 increase in real earnings in 1978 controlling for
#     training, education and if person is black
## b) Every person who was assigned to job training is associated with $1684.22
#     increase in real earnings in 1978 controlling for education,black and re75, 
#     yes the coefficient is significant as p value is less than 0.05
## c) Every person who was black is associated with decrease in real earnings in 
#     1978 by $2112.7 controlling for re75, education and job training