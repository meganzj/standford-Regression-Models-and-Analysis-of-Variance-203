> invisible(options(echo = TRUE))
> 
> #install.packages('lars')
> # Data is in this library -- also contains code for LASSO
> 
> library(lars)
> data(diabetes)
> 
> #install.packages('VR') -- Venables & Ripley
> library(MASS)
> 
> # Generalized cross-validation error
> 
> diabetes.ridge <- lm.ridge(diabetes$y ~ diabetes$x, lambda=seq(0,10,0.05))
> plot(diabetes.ridge$lambda, diabetes.ridge$GCV, xlab='Lambda', ylab='GCV', type='l', lwd=3, col='orange')
> 
> # What is the "optimal" lambda?
> 
> print(select(diabetes.ridge))
modified HKB estimator is 5.462251 
modified L-W estimator is 7.641667 
smallest value of GCV at 3.25 
NULL
> 
> proc.time()
[1] 0.67 0.06 0.76 0.00 0.00