> invisible(options(echo = TRUE))
> library(MPV)
> data(p9.10)
> 
> full.lm <- lm(y ~ ., data=p9.10)
> 
> ## DATA description
> # help(p9.10)
> 
> # The 'p9.10' data frame has 31 observations on the rut depth of
> # asphalt pavements prepared under different conditions.
> 
> # Usage:
> 
> # data(p9.10)
> 
> # Format:
> 
> # This data frame contains the following columns:
> 
> # y change in rut depth/million wheel passes (log scale)
> 
> # x1 viscosity (log scale)
> 
> # x2 percentage of asphalt in surface course
> 
> # x3 percentage of asphalt in base course
> 
> # x4 indicator
> 
> # x5 percentage of fines in surface course
> 
> # x6 percentage of voids in surface course
> 
> 
> # This is the library with the leaps function
> 
> # install.packages('leaps')
> 
> library(leaps)
> 
> # Leaps takes a design matrix as argument: throw away the intercept
> # column or leaps will complain
> 
> X <- model.matrix(full.lm)[,-1]
> 
> # Look at R^2
> 
> # R^2 -- all subsets
> 
> r2.leaps <- leaps(X, p9.10$y, nbest=3, method='r2')
> plot(r2.leaps$size, r2.leaps$r2, pch=23, bg='blue', cex=2)
> best.model.r2 <- r2.leaps$which[which((r2.leaps$r2 == max(r2.leaps$r2))),]
> print(best.model.r2)
1 2 3 4 5 6 
TRUE TRUE TRUE TRUE TRUE TRUE 
> 
> 
> # Adjusted R^2 -- all subsets
> 
> adjr2.leaps <- leaps(X, p9.10$y, nbest=3, method='adjr2')
> plot(adjr2.leaps$size, adjr2.leaps$adjr2, pch=23, bg='blue', cex=2)
> best.model.adjr2 <- adjr2.leaps$which[which((adjr2.leaps$adjr2 == max(adjr2.leaps$adjr2))),]
> print(best.model.adjr2)
1 2 3 4 5 6 
TRUE FALSE FALSE TRUE FALSE FALSE 
> 
> 
> # Cp -- all subsets
> 
> Cp.leaps <- leaps(X, p9.10$y, nbest=3, method='Cp')
> plot(Cp.leaps$size, Cp.leaps$Cp, pch=23, bg='blue', cex=2)
> best.model.Cp <- Cp.leaps$which[which((Cp.leaps$Cp == min(Cp.leaps$Cp))),]
> print(best.model.Cp)
1 2 3 4 5 6 
FALSE FALSE FALSE TRUE FALSE FALSE 
> 
> 
> # Use stepwise search, both direction, starting at full model
> 
> full.step.both <- step(full.lm, direction='both')
Start: AIC= -93.08 
y ~ x1 + x2 + x3 + x4 + x5 + x6 

Df Sum of Sq RSS AIC
- x6 1 0.001 0.981 -95.049
- x2 1 0.006 0.986 -94.901
- x5 1 0.015 0.995 -94.620
- x3 1 0.017 0.997 -94.551
- x1 1 0.053 1.033 -93.447
0.980 -93.078
- x4 1 0.262 1.242 -87.724

Step: AIC= -95.05 
y ~ x1 + x2 + x3 + x4 + x5 

Df Sum of Sq RSS AIC
- x2 1 0.008 0.989 -96.808
- x5 1 0.014 0.995 -96.620
- x3 1 0.016 0.997 -96.533
- x1 1 0.064 1.045 -95.081
0.981 -95.049
+ x6 1 0.001 0.980 -93.078
- x4 1 0.264 1.245 -89.670

Step: AIC= -96.81 
y ~ x1 + x3 + x4 + x5 

Df Sum of Sq RSS AIC
- x5 1 0.010 0.999 -98.496
- x3 1 0.020 1.009 -98.185
0.989 -96.808
- x1 1 0.073 1.062 -96.598
+ x2 1 0.008 0.981 -95.049
+ x6 1 0.003 0.986 -94.901
- x4 1 0.258 1.247 -91.618

Step: AIC= -98.5 
y ~ x1 + x3 + x4 

Df Sum of Sq RSS AIC
- x3 1 0.013 1.011 -100.107
0.999 -98.496
- x1 1 0.082 1.081 -98.046
+ x5 1 0.010 0.989 -96.808
+ x2 1 0.004 0.995 -96.620
+ x6 1 0.000441 0.998 -96.510
- x4 1 0.253 1.252 -93.497

Step: AIC= -100.11 
y ~ x1 + x4 

Df Sum of Sq RSS AIC
1.011 -100.107
- x1 1 0.070 1.081 -100.027
+ x3 1 0.013 0.999 -98.496
+ x2 1 0.008 1.004 -98.341
+ x5 1 0.003 1.009 -98.185
+ x6 1 0.001 1.010 -98.143
- x4 1 0.301 1.312 -94.035
> print(summary(full.step.both))

Call:
lm(formula = y ~ x1 + x4, data = p9.10)

Residuals:
Min 1Q Median 3Q Max 
-0.40519 -0.11629 0.01939 0.10351 0.44521 

Coefficients:
Estimate Std. Error t value Pr(>|t|) 
(Intercept) 0.7883 0.1338 5.892 2.45e-06 ***
x1 -0.1489 0.1068 -1.394 0.17424 
x4 -0.2866 0.0993 -2.886 0.00743 ** 
---
Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

Residual standard error: 0.19 on 28 degrees of freedom
Multiple R-Squared: 0.8434,	Adjusted R-squared: 0.8322 
F-statistic: 75.37 on 2 and 28 DF, p-value: 5.356e-12 

> 
> # Backward
> 
> full.step.backward <- step(full.lm, direction='backward')
Start: AIC= -93.08 
y ~ x1 + x2 + x3 + x4 + x5 + x6 

Df Sum of Sq RSS AIC
- x6 1 0.001 0.981 -95.049
- x2 1 0.006 0.986 -94.901
- x5 1 0.015 0.995 -94.620
- x3 1 0.017 0.997 -94.551
- x1 1 0.053 1.033 -93.447
0.980 -93.078
- x4 1 0.262 1.242 -87.724

Step: AIC= -95.05 
y ~ x1 + x2 + x3 + x4 + x5 

Df Sum of Sq RSS AIC
- x2 1 0.008 0.989 -96.808
- x5 1 0.014 0.995 -96.620
- x3 1 0.016 0.997 -96.533
- x1 1 0.064 1.045 -95.081
0.981 -95.049
- x4 1 0.264 1.245 -89.670

Step: AIC= -96.81 
y ~ x1 + x3 + x4 + x5 

Df Sum of Sq RSS AIC
- x5 1 0.010 0.999 -98.496
- x3 1 0.020 1.009 -98.185
0.989 -96.808
- x1 1 0.073 1.062 -96.598
- x4 1 0.258 1.247 -91.618

Step: AIC= -98.5 
y ~ x1 + x3 + x4 

Df Sum of Sq RSS AIC
- x3 1 0.013 1.011 -100.107
0.999 -98.496
- x1 1 0.082 1.081 -98.046
- x4 1 0.253 1.252 -93.497

Step: AIC= -100.11 
y ~ x1 + x4 

Df Sum of Sq RSS AIC
1.011 -100.107
- x1 1 0.070 1.081 -100.027
- x4 1 0.301 1.312 -94.035
> print(summary(full.step.backward))

Call:
lm(formula = y ~ x1 + x4, data = p9.10)

Residuals:
Min 1Q Median 3Q Max 
-0.40519 -0.11629 0.01939 0.10351 0.44521 

Coefficients:
Estimate Std. Error t value Pr(>|t|) 
(Intercept) 0.7883 0.1338 5.892 2.45e-06 ***
x1 -0.1489 0.1068 -1.394 0.17424 
x4 -0.2866 0.0993 -2.886 0.00743 ** 
---
Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

Residual standard error: 0.19 on 28 degrees of freedom
Multiple R-Squared: 0.8434,	Adjusted R-squared: 0.8322 
F-statistic: 75.37 on 2 and 28 DF, p-value: 5.356e-12 

> 
> # Forward
> # Need an "upper" model
> 
> lowest.step.forward <- step(lm(y ~ 1, data=p9.10), list(upper=full.lm), direction='forward')
Start: AIC= -46.64 
y ~ 1 

Df Sum of Sq RSS AIC
+ x4 1 5.374 1.081 -100.027
+ x1 1 5.144 1.312 -94.035
+ x6 1 1.078 5.378 -50.304
+ x5 1 0.427 6.028 -46.763
6.456 -46.640
+ x2 1 0.136 6.320 -45.299
+ x3 1 0.063 6.392 -44.947

Step: AIC= -100.03 
y ~ x4 

Df Sum of Sq RSS AIC
+ x1 1 0.070 1.011 -100.107
1.081 -100.027
+ x5 1 0.013 1.068 -98.403
+ x6 1 0.012 1.070 -98.359
+ x2 1 0.010 1.071 -98.319
+ x3 1 0.001 1.081 -98.046

Step: AIC= -100.11 
y ~ x4 + x1 

Df Sum of Sq RSS AIC
1.011 -100.107
+ x3 1 0.013 0.999 -98.496
+ x2 1 0.008 1.004 -98.341
+ x5 1 0.003 1.009 -98.185
+ x6 1 0.001 1.010 -98.143
> print(summary(lowest.step.forward))

Call:
lm(formula = y ~ x4 + x1, data = p9.10)

Residuals:
Min 1Q Median 3Q Max 
-0.40519 -0.11629 0.01939 0.10351 0.44521 

Coefficients:
Estimate Std. Error t value Pr(>|t|) 
(Intercept) 0.7883 0.1338 5.892 2.45e-06 ***
x4 -0.2866 0.0993 -2.886 0.00743 ** 
x1 -0.1489 0.1068 -1.394 0.17424 
---
Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

Residual standard error: 0.19 on 28 degrees of freedom
Multiple R-Squared: 0.8434,	Adjusted R-squared: 0.8322 
F-statistic: 75.37 on 2 and 28 DF, p-value: 5.356e-12 

> 
> proc.time()
[1] 1.06 0.06 1.18 0.00 0.00
> 