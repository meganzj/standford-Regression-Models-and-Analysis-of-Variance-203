> # year vs. millions of telephone calls in Belgium
> 
> data(phones)
> attach(phones)
> plot(year, calls, pch=21, bg='red')
> 
> calls.lm <- lm(calls ~ year)
> print(summary(calls.lm))

Call:
lm(formula = calls ~ year)

Residuals:
Min 1Q Median 3Q Max 
-78.97 -33.52 -12.04 23.38 124.20 

Coefficients:
Estimate Std. Error t value Pr(>|t|) 
(Intercept) -260.059 102.607 -2.535 0.0189 * 
year 5.041 1.658 3.041 0.0060 **
---
Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

Residual standard error: 56.22 on 22 degrees of freedom
Multiple R-Squared: 0.2959,	Adjusted R-squared: 0.2639 
F-statistic: 9.247 on 1 and 22 DF, p-value: 0.005998 

> 
> calls.rlm <- rlm(calls ~ year, maxit=50, psi=psi.bisquare) # default is psi.huber
> print(summary(calls.rlm))

Call: rlm(formula = calls ~ year, maxit = 50, psi = psi.bisquare)
Residuals:
Min 1Q Median 3Q Max 
-1.6585 -0.4143 0.2837 39.0866 188.5376 

Coefficients:
Value Std. Error t value 
(Intercept) -52.3025 2.7530 -18.9985
year 1.0980 0.0445 24.6846

Residual standard error: 1.654 on 22 degrees of freedom

Correlation of Coefficients:

(Intercept)
year -0.9937 
> 
> calls.lms <- lmsreg(calls ~ year)
> 
> calls.lts <- ltsreg(calls ~ year, quantile=5)
> 
> lines(year, predict(calls.rlm), col='green', lwd=2)
> lines(year, predict(calls.lm), col=rgb(0.65,0,0.23), lwd=2)
> lines(year, predict(calls.lms), col='orange', lwd=2)
> lines(year, predict(calls.lts), col=rgb(0.4,0.4,0.4), lwd=2)
> 
> # legend(locator(1), c('Linear', 'Robust', 'Least Median', 'Least Trimmed'), lwd=rep(2,4), col=c(rgb(0.65,0,0.23), 'green', 'orange', rgb(0.4,0.4,0.4)))
> 
> legend(51,190, c('Linear', 'Robust', 'Least Median', 'Least Trimmed'), lwd=rep(2,4), col=c(rgb(0.65,0,0.23), 'green', 'orange', rgb(0.4,0.4,0.4)))
> 
> proc.time()
[1] 0.65 0.04 0.72 0.00 0.00