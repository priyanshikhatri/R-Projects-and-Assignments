###############################################################
# Title:        ps4.r
# Author:       Priyanshi Khatri (pxk173330)
# Date:         2018-04-23
# Description:  Turn-in product for problem set 4
###############################################################

###############################################################
##Question 1
rm(list=ls(all=TRUE))

## Import packages
library(data.table)
library(glmx)
library(margins)
library(lmtest)
library(sandwich)#used for white test
library(party)
library(evtree)

## Data import and validation
context1    <- fread('htv.csv')
summary(context1)

## Run model
model1      <- lm(log(wage)~abil+educ+exper, data=context1)
c(AIC(model1),BIC(model1))
#[1] 1935.995 1961.569

model2      <- lm(log(wage)~abil+educ+exper+(educ:exper), data=context1)
c(AIC(model2),BIC(model2))
#[1] 1927.660 1958.349

## Summarize
summary(model1)

# Call:
#   lm(formula = log(wage) ~ abil + educ + exper, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.31146 -0.28802  0.03383  0.30273  1.81079 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.612140   0.176546   3.467 0.000544 ***
#   abil        0.056510   0.008627   6.551 8.41e-11 ***
#   educ        0.101724   0.009831  10.347  < 2e-16 ***
#   exper       0.034811   0.006682   5.209 2.22e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5303 on 1226 degrees of freedom
# Multiple R-squared:  0.2043,	Adjusted R-squared:  0.2023 
# F-statistic: 104.9 on 3 and 1226 DF,  p-value: < 2.2e-16

summary(model2)

# Call:
#   lm(formula = log(wage) ~ abil + educ + exper + (educ : exper), 
#      data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.28405 -0.29671  0.03965  0.30413  1.80108 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.331373   0.284573   4.678 3.21e-06 ***
#   abil         0.052907   0.008667   6.105 1.38e-09 ***
#   educ         0.048998   0.019102   2.565  0.01044 *  
#   exper       -0.037966   0.023596  -1.609  0.10787    
# educ:exper   0.005602   0.001742   3.215  0.00134 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5283 on 1225 degrees of freedom
# Multiple R-squared:  0.2109,	Adjusted R-squared:  0.2084 
# F-statistic: 81.87 on 4 and 1225 DF,  p-value: < 2.2e-16

##Question 1
## a) Model 2 has an additional term (educ*exper) as compare to model1, also the
#     BIC for model 2 (1958.349) is less than that of model 1 (1961.569)
## b) It is an interaction variable and its making the model better in terms of
#     understanding the relationship among the variables educ and exper that is
#     joint effect on the model

###############################################################
##Question 2

## Data import and validation
context2    <- fread('loanapp.csv')
summary(context2)

## Run model
model3      <- glm(approve~white,family=binomial(link="logit"), data=context2)

## Summarize
summary(model3)

# Call:
#   glm(formula = approve ~ white, family = binomial(link = "logit"), 
#       data = context2)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.1864   0.4384   0.4384   0.4384   0.8314  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   0.8847     0.1253   7.061 1.65e-12 ***
#   white       1.4094     0.1512   9.325  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1480.7  on 1988  degrees of freedom
# Residual deviance: 1401.8  on 1987  degrees of freedom
# AIC: 1405.8
# 
# Number of Fisher Scoring iterations: 5


# White-corrected or white robust t test for significance
coeftest(model3,vcov.=vcovHC)

# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  0.88469    0.12570   7.038  1.95e-12 ***
#   white     1.40942    0.15152   9.302 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Run model
model4      <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+
                   cosign+chist+pubrec+mortlat1+mortlat2+vr,family = binomial(link="logit"), data=context2)

## Summarize
summary(model4)

# Call:
#   glm(formula = approve ~ white + hrat + obrat + loanprc + unem + 
#         male + married + dep + sch + cosign + chist + pubrec + mortlat1 + 
#         mortlat2 + vr, family = binomial(link = "logit"), data = context2)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.9549   0.2545   0.3458   0.4768   2.0827  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  3.80171    0.59467   6.393 1.63e-10 ***
#   white        0.93776    0.17290   5.424 5.84e-08 ***
#   hrat         0.01326    0.01288   1.030  0.30313    
# obrat       -0.05303    0.01128  -4.702 2.58e-06 ***
#   loanprc     -1.90495    0.46041  -4.138 3.51e-05 ***
#   unem        -0.06658    0.03281  -2.029  0.04242 *  
#   male        -0.06639    0.20642  -0.322  0.74776    
# married      0.50328    0.17799   2.828  0.00469 ** 
#   dep         -0.09073    0.07333  -1.237  0.21598    
# sch          0.04123    0.17840   0.231  0.81723    
# cosign       0.13206    0.44608   0.296  0.76720    
# chist        1.06658    0.17121   6.230 4.67e-10 ***
#   pubrec      -1.34067    0.21736  -6.168 6.92e-10 ***
#   mortlat1    -0.30988    0.46351  -0.669  0.50378    
# mortlat2    -0.89468    0.56857  -1.574  0.11559    
# vr          -0.34983    0.15372  -2.276  0.02286 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1476  on 1970  degrees of freedom
# Residual deviance: 1201  on 1955  degrees of freedom
# (18 observations deleted due to missingness)
# AIC: 1233
# 
# Number of Fisher Scoring iterations: 5


# White-corrected or white robust t test for significance
coeftest(model4,vcov.=vcovHC)

# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  3.801710   0.653953  5.8134 6.120e-09 ***
#   white        0.937764   0.177764  5.2753 1.325e-07 ***
#   hrat         0.013263   0.013924  0.9525 0.3408381    
# obrat       -0.053034   0.012809 -4.1403 3.469e-05 ***
#   loanprc     -1.904951   0.535160 -3.5596 0.0003714 ***
#   unem        -0.066579   0.036124 -1.8430 0.0653225 .  
# male        -0.066385   0.210174 -0.3159 0.7521100    
# married      0.503282   0.186857  2.6934 0.0070728 ** 
#   dep         -0.090734   0.075412 -1.2032 0.2289086    
# sch          0.041229   0.179024  0.2303 0.8178605    
# cosign       0.132059   0.406794  0.3246 0.7454585    
# chist        1.066577   0.173265  6.1558 7.472e-10 ***
#   pubrec      -1.340665   0.233076 -5.7520 8.817e-09 ***
#   mortlat1    -0.309882   0.545600 -0.5680 0.5700580    
# mortlat2    -0.894675   0.608995 -1.4691 0.1418053    
# vr          -0.349828   0.156653 -2.2331 0.0255398 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Run model
model5      <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+
                  cosign+chist+pubrec+mortlat1+mortlat2+vr+(white*obrat),family = binomial(link="logit"), data=context2)

## Summarize
summary(model5)

# White-corrected or white robust t test for significance
coeftest(model5,vcov.=vcovHC)

# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  4.306527   0.901355  4.7778 1.772e-06 ***
#   white        0.296882   0.858772  0.3457 0.7295644    
# hrat         0.013405   0.014158  0.9468 0.3437269    
# obrat       -0.066604   0.020728 -3.2133 0.0013121 ** 
#   loanprc     -1.909701   0.533546 -3.5793 0.0003446 ***
#   unem        -0.067549   0.035988 -1.8770 0.0605206 .  
# male        -0.071904   0.210842 -0.3410 0.7330802    
# married      0.503536   0.187212  2.6897 0.0071526 ** 
#   dep         -0.095772   0.076030 -1.2597 0.2077873    
# sch          0.034893   0.180807  0.1930 0.8469704    
# cosign       0.152567   0.413003  0.3694 0.7118226    
# chist        1.061385   0.174154  6.0945 1.098e-09 ***
#   pubrec      -1.344267   0.235075 -5.7185 1.075e-08 ***
#   mortlat1    -0.333314   0.540401 -0.6168 0.5373739    
# mortlat2    -0.920857   0.610013 -1.5096 0.1311534    
# vr          -0.350862   0.157302 -2.2305 0.0257141 *  
#   white:obrat  0.018149   0.023603  0.7689 0.4419416    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##Question 2
## a) It can be concluded from model3 that if the applicant is white the changes
#     of loan getting approved is high
## b) Yes, b1 is still significant at 0.1% significance level, though b1 is slightly
#     reduced but still if the applicant is white the chances of loan getting
#     approved is high
## c) After adding the interaction variable, b1 is now completely insignificant
## d) white*obrat is the interaction variable and it has affected the model
#     so greatly because it incorporated the realtionship between the white and
#     obrat variables which made the model more accurate for analysis of dependent Variable

###############################################################
##Question 3

## Data import and validation
context3    <- fread('smoke.csv')
summary(context3)

## Create new variables
context3$agesq <-context3$age^2

## Run model
model6      <- glm(cigs~educ+age+agesq+log(income)+restaurn,family=poisson(link="log"), data=context3)

## Summarize
summary(model6)

# Call:
#   glm(formula = cigs ~ educ + age + (age^2) + log(income) + restaurn, 
#       family = poisson(link = "log"), data = context3)
# 
# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -5.320  -4.204  -3.626   2.356  14.806  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.6926554  0.1875448   3.693 0.000221 ***
#   educ        -0.0449613  0.0042746 -10.518  < 2e-16 ***
#   age         -0.0052270  0.0007324  -7.137 9.54e-13 ***
#   log(income)  0.2383297  0.0197209  12.085  < 2e-16 ***
#   restaurn    -0.3740674  0.0307253 -12.175  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 15821  on 806  degrees of freedom
# Residual deviance: 15437  on 802  degrees of freedom
# AIC: 16917
# 
# Number of Fisher Scoring iterations: 6

# White-corrected or white robust t test for significance
coeftest(model6,vcov.=vcovHC)

# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept) -0.0895322  0.7819305 -0.1145  0.908840    
# educ        -0.0595212  0.0194111 -3.0663  0.002167 ** 
# age          0.1139858  0.0215662  5.2854 1.254e-07 ***
# agesq       -0.0013679  0.0002485 -5.5045 3.701e-08 ***
# log(income)  0.1047168  0.0840807  1.2454  0.212973    
# restaurn    -0.3613089  0.1386491 -2.6059  0.009163 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##Question 3
## a)For every one year increase in education is associated with 5.95% decrease
#   in cigarettes smoked.
## b)dln(cigs)/dage =b2+2b3age, so when age is 20 dln(cigs)/dage =5.927%
#    so when age is 60 dln(cigs)/dage = -5.0162%


###############################################################
##Question 4

## Data import and validation
context4    <- fread('hdisease.csv')
summary(context4)

context4$exang <- ifelse(context4$exang=="yes",1,0)

## Run model
f           <- hdisease~age+cp+trestbps+thalach+exang
model7      <- ctree(f, data=context4)
plot(model7)
model7

# Model formula:
#   hdisease ~ age + cp + trestbps + thalach + exang

# Fitted party:
#   [1] root
# |   [2] cp < 4
# |   |   [3] age < 57: 0.192 (n = 255, err = 75.6)
# |   |   [4] age >= 57: 0.802 (n = 106, err = 112.8)
# |   [5] cp >= 4
# |   |   [6] age < 54: 1.078 (n = 180, err = 172.9)
# |   |   [7] age >= 54: 1.700 (n = 253, err = 343.2)
# 
# Number of inner nodes:    3
# Number of terminal nodes: 4

model8      <- evtree(f, data=context4)
plot(model8)
model8

# Model formula:
#   hdisease ~ age + cp + trestbps + thalach + exang
# 
# Fitted party:
#   [1] root
# |   [2] cp <= 3
# |   |   [3] thalach <= 112: 1.241 (n = 29, err = 45.3)
# |   |   [4] thalach > 112
# |   |   |   [5] exang <= 0
# |   |   |   |   [6] age <= 56: 0.110 (n = 219, err = 27.4)
# |   |   |   |   [7] age > 56: 0.625 (n = 72, err = 56.9)
# |   |   |   [8] exang > 0: 0.707 (n = 41, err = 40.5)
# |   [9] cp > 3
# |   |   [10] age <= 53
# |   |   |   [11] thalach <= 128: 1.333 (n = 78, err = 79.3)
# |   |   |   [12] thalach > 128: 0.882 (n = 102, err = 84.6)
# |   |   [13] age > 53
# |   |   |   [14] exang <= 0: 1.409 (n = 93, err = 138.5)
# |   |   |   [15] exang > 0
# |   |   |   |   [16] age <= 65: 1.768 (n = 138, err = 162.6)
# |   |   |   |   [17] age > 65: 2.500 (n = 22, err = 19.5)
# 
# Number of inner nodes:    8
# Number of terminal nodes: 9

## Data import and validation
context45    <- fread('hdisease.csv')
summary(context45)

#predicted classification
context45$hdisease_pred  <- predict(model8,context4)

##Question 4
## a)On comparing both the models, model8 underfits the data whereas model 7
#   overfits the data
## b)We didnt include dset in the model because it is a categorical variable
#   and we don't have identifying information on dset


###############################################################
##Question 5

## Data import and validation
context5    <- fread('WAGE1.csv')
summary(context5)

seed        <-	2	#random seed
maxClusters	<-	10 

## Use within-group variation to choose k, kmeans for 10 clusters
wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) { # i represents the k value
  set.seed(seed)
  model <- kmeans(context5,centers=i,nstart=10)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

## Run the model
set.seed(seed)
model9 <- kmeans(context5,centers=3,nstart=10)

## Means from model9
model9$centers

# wage     educ     exper    tenure   nonwhite    female   married    numdep
# 1 5.900926 10.96296 38.777778 11.657407 0.10185185 0.4629630 0.7500000 0.5740741
# 2 5.206958 13.12548  5.946768  1.699620 0.09885932 0.5133080 0.4486692 0.8897338
# 3 7.105806 12.72258 20.638710  6.316129 0.10967742 0.4322581 0.7806452 1.6322581
# smsa  northcen     south      west   construc    ndurman   trcommpu
# 1 0.6388889 0.2962963 0.3333333 0.1574074 0.01851852 0.14814815 0.03703704
# 2 0.7870722 0.2775665 0.3155894 0.1596958 0.06083650 0.07604563 0.05323194
# 3 0.6709677 0.1741935 0.4387097 0.1935484 0.03870968 0.15483871 0.03225806
# trade   services  profserv   profocc   clerocc    servocc
# 1 0.2222222 0.12037037 0.2500000 0.3240741 0.1203704 0.13888889
# 2 0.3193916 0.09505703 0.2699620 0.3498099 0.1863118 0.17110266
# 3 0.2774194 0.09677419 0.2451613 0.4258065 0.1677419 0.09032258

## segment the data into clusters and run on model 9
groups1 <- model9$cluster
context5$cluster <- groups1

model10 <- lm(wage~educ+exper+tenure,data=context5[cluster==1])
model11 <- lm(wage~educ+exper+tenure,data=context5[cluster==2])
model12 <- lm(wage~educ+exper+tenure,data=context5[cluster==3])

table(groups1)

# groups1
# 1   2   3 
# 108 263 155

## Summarize
summary(model10)

# Call:
#   lm(formula = wage ~ educ + exper + tenure, data = context5[cluster == 
#                                                                1])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.4894 -2.0198 -0.5852  1.2467 14.4291 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.78799    2.83502   1.336 0.184419    
# educ         0.40735    0.10276   3.964 0.000135 ***
# exper       -0.10172    0.05851  -1.739 0.085077 .  
# tenure       0.13653    0.03098   4.407 2.55e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.292 on 104 degrees of freedom
# Multiple R-squared:  0.3365,	Adjusted R-squared:  0.3174 
# F-statistic: 17.58 on 3 and 104 DF,  p-value: 2.628e-09

summary(model11)

# Call:
#   lm(formula = wage ~ educ + exper + tenure, data = context5[cluster == 
#                                                                2])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.7853 -1.2968 -0.3433  0.6621 12.0409 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -3.21053    0.81295  -3.949 0.000101 ***
#   educ         0.52524    0.06025   8.717 3.49e-16 ***
#   exper        0.17286    0.03806   4.542 8.55e-06 ***
#   tenure       0.29156    0.06613   4.409 1.52e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.278 on 259 degrees of freedom
# Multiple R-squared:  0.3765,	Adjusted R-squared:  0.3693 
# F-statistic: 52.14 on 3 and 259 DF,  p-value: < 2.2e-16

summary(model12)

# Call:
#   lm(formula = wage ~ educ + exper + tenure, data = context5[cluster == 
#                                                                3])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.6701 -2.2514 -0.5411  1.6947 13.8507 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -4.85740    2.03152  -2.391    0.018 *  
#   educ         0.73631    0.11888   6.194 5.30e-09 ***
#   exper        0.05905    0.05871   1.006    0.316    
# tenure       0.21796    0.04655   4.683 6.26e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.755 on 151 degrees of freedom
# Multiple R-squared:  0.2939,	Adjusted R-squared:  0.2798 
# F-statistic: 20.95 on 3 and 151 DF,  p-value: 2.118e-11


##Question 5
## a)Using the elbow test on the within-sum of squares plot, the optimal number
#   of clusters for this data set is 2.
## b)Cluster 1 workers have low education but high experience and tenure,
#    cluster 2 workers have high education years but low experience
#    and tenure whereas cluster 3 workers have high education and also relatively
#    high experience and tenure
## c)For model10 educ and tenure are significant at 0.1% level and exper is 
#    significant at 10% level, as their coefficients are relatively low so wage
#    of the workers for cluster1 is low. For model11 educ, exper and tenure are
#    significant at 0.1% level and educ and tenure have relatively high value 
#    whereas exper has low value. For model12 educ and tenure are significant 
#    whereas exper is insignificant and education is immensely valuable.

###############################################################
##Question 6

## Data import and validation
context6    <- fread('murder.csv')
summary(context6)

## Run PCA 
model13 <- prcomp(context6[,2:52])# exclude year
model13

## Generate screeplot
screeplot(model13,type="lines") # looks like there are 2 principal components
eigen <- model13$sdev^2 
varplot <- sum(eigen)-cumsum(eigen) 
plot(0:10,varplot[1:11])
lines(0:10,varplot[1:11]) # finally there are 1 principal components

# get the principal components
context6$factors <- model13$x[,1]
head(context6$factors)

#[1] -22.686544 -22.722453 -17.717671 -15.038591  -3.081939  -8.288013

summary(context6$factors)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -27.871  -9.946  -4.047   0.000   4.656  43.855 

# algebra to standardize the principal components to have variance 1
context6$factors <- context6$factors/sd(context6$factors)
var(context6$factors)

## time series plot of the factor
plot(context6$Year,context6$factors)
lines(context6$Year,context6$factors)

##Question 6
## a)Based on scree plot, It looks like there is one principal component on 
#   applying elbow test.
## b)The value of factor is high from 1990 to 1992 and then decreased a little
#   but was high again in 1995 and then crime continued to decrease and was very
#   low in 2014
