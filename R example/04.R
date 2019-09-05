> invisible(options(echo = TRUE))
> ## Read in data
> 
> paper.table <- read.table('http://www-stat.stanford.edu/~jtaylo/courses/stats203/data/paper.table', header=T, sep=',')
> attach(paper.table)
> 
> # Pulp and paper data
> 
> plot(x,y, pch=23, bg='red', cex=2)
> 
> # Linear regression function
> 
> linear.lm <- lm(y~x)
> lines(x, predict(linear.lm), lwd=2, col='green')
> 
> # Quadratic regression function
> 
> quadratic.lm <- lm(y~poly(x,2))
> lines(x, predict(quadratic.lm), lwd=2, col='yellow')
> 
> # Cubic regression function
> 
> cubic.lm <- lm(y~poly(x,3))
> lines(x, predict(cubic.lm), lwd=2, col='gray')
> 
> # Quartic regression function
> 
> quartic.lm <- lm(y~poly(x,4))
> lines(x, predict(quartic.lm), lwd=2, col='black')
> 
> 
> # Summary of the model
> 
> print(summary(quartic.lm))

Call:
lm(formula = y ~ poly(x, 4))

Residuals:
Min 1Q Median 3Q Max 
-5.1437 -1.0470 -0.3087 1.0893 4.4970 

Coefficients:
Estimate Std. Error t value Pr(>|t|) 
(Intercept) 34.179 0.583 58.629 < 2e-16 ***
poly(x, 4)1 32.298 2.541 12.710 4.46e-09 ***
poly(x, 4)2 -45.371 2.541 -17.855 4.98e-11 ***
poly(x, 4)3 -14.567 2.541 -5.732 5.18e-05 ***
poly(x, 4)4 -3.193 2.541 -1.256 0.230 
---
Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

Residual standard error: 2.541 on 14 degrees of freedom
Multiple R-Squared: 0.9735,	Adjusted R-squared: 0.966 
F-statistic: 128.7 on 4 and 14 DF, p-value: 7.126e-11 

> 
> # A little function to compute SSE of a model
> 
> SSE <- function(input.lm) {
+ return(sum(resid(input.lm)^2))}
> 
> # Another function to implement the F test
> 
> Ftest <- function(F.lm, R.lm) {
+ SSE.F <- SSE(F.lm)
+ SSE.R <- SSE(R.lm)
+ df.N <- R.lm$df - F.lm$df
+ df.D <- F.lm$df
+ SSE.N <- SSE.R - SSE.F
+ SSE.D <- SSE.F 
+ F <- (SSE.N / df.N) / (SSE.D / df.D)
+ pval <- 1 - pf(F, df.N, df.D)
+ return(data.frame(F, df.N, df.D, pval))
+ }
> 
> print(Ftest(quartic.lm, cubic.lm))
F df.N df.D pval
1 1.578663 1 14 0.2295146
> 
> print(Ftest(quartic.lm, quadratic.lm))
F df.N df.D pval
1 17.22002 2 14 0.0001684474
> proc.time()
[1] 0.55 0.03 0.61 0.00 0.00