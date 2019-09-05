> invisible(options(echo = TRUE))
> ### Download the data and tell R where to find the variables by attaching it
> 
> heights.table <- read.table('http://www-stat.stanford.edu/~jtaylo/courses/stats203/data/heights.table', header=T, sep=',')
> attach(heights.table)
> 
> # wife's height vs. husband's height
> plot(heights.table, pch=23, bg='red', cex=2, lwd=2)
> 
> # Fit model
> 
> wife.lm <- lm(WIFE ~ HUSBAND)
> print(summary(wife.lm))

Call:
lm(formula = WIFE ~ HUSBAND)

Residuals:
Min 1Q Median 3Q Max 
-19.4235 -3.9438 0.8399 4.0123 11.1439 

Coefficients:
Estimate Std. Error t value Pr(>|t|) 
(Intercept) 42.54871 10.77592 3.949 0.000153 ***
HUSBAND 0.69593 0.06176 11.268 < 2e-16 ***
---
Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

Residual standard error: 5.952 on 93 degrees of freedom
Multiple R-Squared: 0.5772,	Adjusted R-squared: 0.5727 
F-statistic: 127 on 1 and 93 DF, p-value: < 2.2e-16 

> 
> # with fitted line
> plot(heights.table, pch=23, bg='red', cex=2, lwd=2)
> abline(wife.lm$coef, lwd=2, col='orange')
> 
> 
> ### Some other aspects of R
> 
> # Take a look at the variable names
> 
> names(heights.table)
[1] "HUSBAND" "WIFE" 
> 
> # Estimate beta.1 using S_xx and S_yx
> 
> num <- cov(HUSBAND, WIFE) # = S_xx / (n-1)
> den <- var(HUSBAND) # = S_yx / (n-1)
> print(num/den)
[1] 0.6959256
> 
> # Get predicted values (Y.hat)
> 
> wife.hat <- predict(wife.lm)
> 
> # Two different ways of getting residuals
> 
> wife.resid1 <- WIFE - predict(wife.lm)
> wife.resid2 <- resid(wife.lm)
> 
> # Computing sample variance by hand
> 
> husband.var <- sum((HUSBAND - mean(HUSBAND))^2) / (length(HUSBAND) - 1)
> print(c(var(HUSBAND), husband.var))
[1] 98.8 98.8
> 
> # Estimating sigma.sq
> 
> S2 <- sum(resid(wife.lm)^2) / wife.lm$df
> print(sqrt(S2))
[1] 5.951824
> print(sqrt(sum(resid(wife.lm)^2) / (length(WIFE) - 2)))
[1] 5.951824
> print(summary(wife.lm)$sigma)
[1] 5.951824
> 
> # What else is in summary(wife.lm)?
> 
> print(names(summary(wife.lm)))
[1] "call" "terms" "residuals" "coefficients" 
[5] "aliased" "sigma" "df" "r.squared" 
[9] "adj.r.squared" "fstatistic" "cov.unscaled" 
> proc.time()
[1] 0.50 0.03 0.57 0.00 0.00