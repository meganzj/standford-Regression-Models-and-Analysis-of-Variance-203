[Previously saved workspace restored]

> invisible(options(echo = TRUE))
> baseurl <- 'http://www-stat.stanford.edu/~jtaylo/courses/stats203/'
> geturl <- function(tailend) {
+ return(paste(baseurl, tailend,sep=''))
+ }
> 
> # Expenditure example
> 
> expenditure.table <-read.table(geturl('data/expenditure.table'), header=T)
> attach(expenditure.table)
> 
> # A "neater" approach
> 
> exp.lm <- lm(Expenditure ~ Stock)
> 
> # find best fitting AR model
> 
> print(ar(resid(exp.lm), order.max=3))

Call:
ar(x = resid(exp.lm), order.max = 3)

Coefficients:
1 
0.7506 

Order selected 1 sigma^2 estimated as 6.925 
> 
> library(nlme)
> exp.gls <- gls(Expenditure ~ Stock, data=expenditure.table, corr=corAR1(0.8), method='ML')
> print(summary(exp.gls))
Generalized least squares fit by maximum likelihood
Model: Expenditure ~ Stock 
Data: expenditure.table 
AIC BIC logLik
96.18197 100.1649 -44.09099

Correlation Structure: AR(1)
Formula: ~1 
Parameter estimate(s):
Phi 
0.845362 

Coefficients:
Value Std.Error t-value p-value
(Intercept) -156.53705 38.23425 -4.094158 7e-04
Stock 2.32035 0.22095 10.501576 0e+00

Correlation: 
(Intr)
Stock -0.998

Standardized residuals:
Min Q1 Med Q3 Max 
-2.23034903 -1.25090986 -0.06133351 0.33448454 1.12981551 

Residual standard error: 3.979903 
Degrees of freedom: 20 total; 18 residual
> 
> exp.gls <- gls(Expenditure ~ Stock, data=expenditure.table, corr=corAR1(0.8), method='REML')
> print(summary(exp.gls))
Generalized least squares fit by REML
Model: Expenditure ~ Stock 
Data: expenditure.table 
AIC BIC logLik
92.39303 95.95452 -42.19651

Correlation Structure: AR(1)
Formula: ~1 
Parameter estimate(s):
Phi 
0.9979174 

Coefficients:
Value Std.Error t-value p-value
(Intercept) -137.03338 71.44341 -1.918069 0.0711
Stock 2.22375 0.36264 6.132168 0.0000

Correlation: 
(Intr)
Stock -0.872

Standardized residuals:
Min Q1 Med Q3 Max 
-0.32213937 -0.22791157 -0.08725919 -0.04668138 0.07920104 

Residual standard error: 35.29475 
Degrees of freedom: 20 total; 18 residual
> proc.time()
[1] 0.86 0.07 0.94 0.00 0.00
> 