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
> # ,,,
> plot(wife.lm, pch=23, bg='red', cex=2, lwd=2)
> 
> 
> proc.time()
[1] 0.50 0.05 0.59 0.00 0.00
> 