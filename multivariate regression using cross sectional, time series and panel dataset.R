###############################################################
# Title:        ps3.r
# Author:       Priyanshi Khatri (pxk173330)
# Date:         2018-04-02
# Description:  Turn-in product for problem set 3
###############################################################

###############################################################
##Question 1
rm(list=ls(all=TRUE))

## Import packages
library(data.table)
library(sandwich) # For White correction
library(lmtest) # More advanced hypothesis testing tools
library(tseries)
library(plm)

## Data import and validation
context1    <- fread('hprice1.csv')
summary(context1)

## Run model
model1      <- lm(price~bdrms+lotsize+sqrft, data=context1)
model2      <- lm(log(price)~bdrms+log(lotsize)+log(sqrft), data=context1)

## Summarize
summary(model1)

# Call:
#   lm(formula = price ~ bdrms + lotsize + sqrft, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -120.026  -38.530   -6.555   32.323  209.376 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.177e+01  2.948e+01  -0.739  0.46221    
# bdrms        1.385e+01  9.010e+00   1.537  0.12795    
# lotsize      2.068e-03  6.421e-04   3.220  0.00182 ** 
# sqrft        1.228e-01  1.324e-02   9.275 1.66e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 59.83 on 84 degrees of freedom
# Multiple R-squared:  0.6724,	Adjusted R-squared:  0.6607 
# F-statistic: 57.46 on 3 and 84 DF,  p-value: < 2.2e-16

# Old school t test for significance (like summary)
coeftest(model1,vcov.=vcov) 

# t test of coefficients:
#   
#   Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept) -2.1770e+01  2.9475e+01 -0.7386  0.462208    
# bdrms        1.3853e+01  9.0101e+00  1.5374  0.127945    
# lotsize      2.0677e-03  6.4213e-04  3.2201  0.001823 ** 
# sqrft        1.2278e-01  1.3237e-02  9.2751 1.658e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# White-corrected or white robust t test for significance
coeftest(model1,vcov.=vcovHC)

# t test of coefficients:
#   
#   Estimate  Std. Error t value Pr(>|t|)   
# (Intercept) -21.7703086  41.0326944 -0.5306 0.597124   
# bdrms        13.8525219  11.5617901  1.1981 0.234236   
# lotsize       0.0020677   0.0071485  0.2893 0.773101   
# sqrft         0.1227782   0.0407325  3.0143 0.003406 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Summarize
summary(model2)

# Call:
#   lm(formula = log(price) ~ bdrms + log(lotsize) + log(sqrft), 
#      data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.68422 -0.09178 -0.01584  0.11213  0.66899 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.29704    0.65128  -1.992   0.0497 *  
# bdrms         0.03696    0.02753   1.342   0.1831    
# log(lotsize)  0.16797    0.03828   4.388 3.31e-05 ***
# log(sqrft)    0.70023    0.09287   7.540 5.01e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1846 on 84 degrees of freedom
# Multiple R-squared:  0.643,	Adjusted R-squared:  0.6302 
# F-statistic: 50.42 on 3 and 84 DF,  p-value: < 2.2e-16

# Old school t test for significance (like summary)
coeftest(model2,vcov.=vcov)

# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.651284 -1.9915   0.04967 *  
# bdrms         0.036958   0.027531  1.3424   0.18308    
# log(lotsize)  0.167967   0.038281  4.3877 3.307e-05 ***
# log(sqrft)    0.700232   0.092865  7.5403 5.006e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# White-corrected or white robust t test for significance
coeftest(model2,vcov.=vcovHC)

# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.850457 -1.5251  0.130988    
# bdrms         0.036958   0.035576  1.0389  0.301845    
# log(lotsize)  0.167967   0.053275  3.1528  0.002243 ** 
# log(sqrft)    0.700232   0.121392  5.7683 1.298e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##Question 1
## a) lotsize and sqrft are significant variables at 1% and 0.1% significance level 
#     respectively using the OLS test for model1.
## b) sqrft variable is significant at 1% level after using the white corrected
#     significance test for model1, interpreted this by checking the p values.
## c) log(lotsize) and log(sqrft) both are significant variables at 0.1% level using 
#     the OLS test for model2, interpreted this by checking the p values.
## d) log(lotsize) and log(sqrft) are significant variables at 1% and 0.1% respectively
#     after using the white corrected significance test for model2.
## e) Taking the log of the variables can decrease the variability of data. So, this
#     makes data conform more closely, bringing extreme values closer than before
#     and thus reducing heteroskedasticity to some extent.


###############################################################
##Question 2

## Data import and validation
context2    <- fread('beveridge.csv')
summary(context2)

## Run model
model3      <- lm(urate~vrate, data=context2)

## Summarize
summary(model3)

# Call:
#   lm(formula = urate ~ vrate, data = context2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1399 -0.9063 -0.1726  0.7893  2.9342 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  17.1194     0.5920   28.92   <2e-16 ***
#   vrate        -3.7414     0.2068  -18.09   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.202 on 133 degrees of freedom
# Multiple R-squared:  0.7111,	Adjusted R-squared:  0.7089 
# F-statistic: 327.3 on 1 and 133 DF,  p-value: < 2.2e-16

## Newey-West-corrected significance test
coeftest(model3,vcov=NeweyWest(model3,lag=5))

# t test of coefficients:
#   
#              Estimate Std. Error t value  Pr(>|t|)    
#  (Intercept) 17.11942    1.36561  12.536 < 2.2e-16 ***
#   vrate       -3.74145    0.39575  -9.454 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## KPSS test to check for level or tend stationary
kpss.test(context2$urate,null="Level")
kpss.test(context2$urate,null="Trend")
kpss.test(context2$vrate,null="Level")
kpss.test(context2$vrate,null="Trend")

kpss.test(diff(context2$urate),null="Level")#good
# KPSS Test for Level Stationarity
# 
# data:  diff(context2$urate)
# KPSS Level = 0.25265, Truncation lag parameter = 2, p-value = 0.1
kpss.test(diff(context2$urate),null="Trend")
kpss.test(diff(context2$vrate),null="Level")#good
# KPSS Test for Level Stationarity
# 
# data:  diff(context2$vrate)
# KPSS Level = 0.30923, Truncation lag parameter = 2, p-value = 0.1
kpss.test(diff(context2$vrate),null="Trend")

kpss.test(diff(diff(context2$urate)),null="Level")
kpss.test(diff(diff(context2$urate)),null="Trend")
kpss.test(diff(diff(context2$vrate)),null="Level")
kpss.test(diff(diff(context2$vrate)),null="Trend")

## Run model
model4      <- lm(diff(urate)~diff(vrate), data=context2)

## Summarize
summary(model4)
coeftest(model4, vcov. = vcov)

# Call:
#   lm(formula = diff(urate) ~ diff(vrate), data = context2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.54257 -0.13705 -0.03429  0.06847  0.66571 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.03705    0.01781   2.080   0.0394 *
# diff(vrate) -0.02760    0.10732  -0.257   0.7974  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2058 on 132 degrees of freedom
# Multiple R-squared:  0.0005008,	Adjusted R-squared:  -0.007071 
# F-statistic: 0.06614 on 1 and 132 DF,  p-value: 0.7974

## Newey-West-corrected significance test
coeftest(model4,vcov=NeweyWest(model4,lag=5))

# t test of coefficients:
#   
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.037046   0.030041  1.2332   0.2197
# diff(vrate) -0.027599   0.081122 -0.3402   0.7342


##Question 2
## f) Yes, the p values for OLS and NeweyWest significance tests show that the  
#     coefficient on the vanancy rate(vrate) is significant at 0.1% level before 
#     we correct for stationarity.
## g) Based on KPSS test, unemployment rate(urate) is level stationary after first 
#     differencing, so we should apply first differencing to urate before modeling.
## h) Based on KPSS test, Vacancy rate(urate) is level stationary after first 
#     differencing, so we should apply first differencing to vrate before modeling.
## i) From model 3 to model 4 as we have corrected for stationarity, the coefficient on 
#     variable vacancy rate(vrate) is not significant in model4 as opposed to model3 where
#     it is significant at 0.1% significance level.
## j) Model 4 better describes the data than model3 as it was created after we  
#     corrected for stationarity in time series data.


###############################################################
##Question 3

## Data import and validation
context3    <- fread('JTRAIN.csv')
#declare this after generating variables as after declaring it as plm we cannot perform
#data table operations
context3    <- plm.data(context3,indexes=c("fcode","year"))
summary(context3)

## Generate new variable
context3$d88  <- ifelse((context3$year==1988), 1,0)
context3$d89  <- ifelse((context3$year==1989), 1,0)

##context3$d88 <- as.numeric(contest3$year==1988)

for (i in 1:nrow(context3)) {
    if (context3$year[i]==1987)
        {context3$grant_t[i] <- 0}
    else if(context3$grant[i-1]==0)
        {context3$grant_t[i] <- 0}
    else
        {context3$grant_t[i] <- 1}
}

context3$grant_lag <- rep(0,471)
context3 <- contest3[order(fcode,year)]
for (j in 1:471)
  if (context3$year[j]==1987)
    context3$grant_lag[j] <- context3$grant[j-1]
#easier to code

## Run model
#Pooled effects linear model
model5      <- plm(log(scrap)~d88+d89+grant+grant_t,model="pooling", data=context3)
#Fixed effects model
model6      <- plm(log(scrap)~d88+d89+grant+grant_t,model="within", data=context3)

## Summarize
summary(model5)

# Pooling Model
# 
# Call:
#   plm(formula = log(scrap) ~ d88 + d89 + grant + grant_t, data = context3, 
#       model = "pooling")
# 
# Balanced Panel: n=54, T=3, N=162
# 
# Residuals :
#   Min.  1st Qu.   Median  3rd Qu.     Max. 
# -5.20260 -0.89599 -0.08461  1.02417  3.30029 
# 
# Coefficients :
#              Estimate Std. Error t-value Pr(>|t|)   
# (Intercept)  0.597434   0.203063  2.9421 0.003754 **
# d88         -0.239370   0.310864 -0.7700 0.442447   
# d89         -0.496524   0.337928 -1.4693 0.143748   
# grant        0.200020   0.338285  0.5913 0.555186   
# grant_t      0.048936   0.436066  0.1122 0.910792   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Total Sum of Squares:    355.75
# Residual Sum of Squares: 349.59
# R-Squared:      0.017311
# Adj. R-Squared: -0.0077257
# F-statistic: 0.691427 on 4 and 157 DF, p-value: 0.59893

coeftest(model6)

# t test of coefficients:
#   
#          Estimate Std. Error t value Pr(>|t|)  
# d88     -0.080216   0.109475 -0.7327  0.46537  
# d89     -0.247203   0.133218 -1.8556  0.06634 .
# grant   -0.252315   0.150629 -1.6751  0.09692 .
# grant_t -0.421590   0.210200 -2.0057  0.04749 *
#   ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


## HAC-corrected significance test (Arellano). Note: n must be >> T
summary(model6, vcov=vcovHC(model6, method = "arellano"))

# Oneway (individual) effect Within Model
# 
# Note: Coefficient variance-covariance matrix supplied: vcovHC(model6, method = "arellano")
# 
# Call:
#   plm(formula = log(scrap) ~ d88 + d89 + grant + grant_t, data = context3, 
#       model = "within")
# 
# Balanced Panel: n=54, T=3, N=162
# 
# Residuals :
#   Min.   1st Qu.    Median   3rd Qu.      Max. 
# -2.286936 -0.112387 -0.017841  0.144272  1.426674 
# 
# Coefficients :
#          Estimate Std. Error t-value Pr(>|t|)  
# d88     -0.080216   0.095719 -0.8380  0.40393  
# d89     -0.247203   0.192514 -1.2841  0.20197  
# grant   -0.252315   0.140329 -1.7980  0.07507 .
# grant_t -0.421590   0.276335 -1.5256  0.13013  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Total Sum of Squares:    32.25
# Residual Sum of Squares: 25.766
# R-Squared:      0.20105
# Adj. R-Squared: -0.23684
# F-statistic: 7.38691 on 4 and 53 DF, p-value: 8.3412e-05

##Question 3
## k) For every firm which received grant in current year the scrap rate is expected 
#     to increase by 20% controlling for d88,d89 and grant_t.
## l) For every firm which received grant last year the scrap rate is expected to 
#     increase by 4.89% controlling for d88,d89 and grant.
## m) The signs of b3 and b4 can be interpreted as decrease or increase in scrap rate,
#     as per model 5 both the coefficients are positive therefore we can interpret that
#     scrap rate would increase if the grant was provided to the firm
#     So if you have high scrap rate you will get grant.
## n) For every firm which received grant in current year the scrap rate is expected 
#     to decrease by 25.23% controlling for d88,d89 and grant_t.
## o) For every firm which received grant last year the scrap rate is expected to 
#     decrease by 42.15% controlling for d88,d89 and grant.
## p) The signs of b3 and b4 can be interpreted as decrease or increase in scrap rate,
#     as per model 6 both the coefficients are negative therefore we can interpret that
#     scrap rate would decrease if the grant was provided to the firm.
## q) d89 and grant(i,t-1) variables are NOT significant after using the HAC(Arellano)
#     significance test while they were significant in the OLS significance test
#     HAC(Arellano) significance test removes heteroskedasticity and autocorrelation.