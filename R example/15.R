> invisible(options(echo = TRUE))
> sattherwaite.CI <- function(a, MS, df, alpha=0.05) {
+ L.hat <- sum(a*MS)
+ df.T <- sum(a*MS)^2 / sum((a*MS)^2/df)
+ U <- L.hat*df.T/qchisq(0.5*alpha, df.T)
+ L <- L.hat*df.T/qchisq(0.5*(1-alpha), df.T)
+ return(data.frame(L.hat, df.T, U, L))
+ }
> 
> sodium.table <- read.table('http://www-stat.stanford.edu/~jtaylo/courses/stats203/data/sodium.table', header=T)
> attach(sodium.table)
> 
> sodium.anova <- anova(lm(sodium ~ factor(brand)))
> print(sodium.anova)
Analysis of Variance Table

Response: sodium
Df Sum Sq Mean Sq F value Pr(>F) 
factor(brand) 5 854.53 170.91 238.71 < 2.2e-16 ***
Residuals 42 30.07 0.72 
---
Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 
> 
> mstr <- sodium.anova$Mean[1]
> mse <- sodium.anova$Mean[2]
> 
> n <- 8
> print(sattherwaite.CI(c(1/8, -1/8), c(mstr, mse), c(5, 42)))
L.hat df.T U L
1 21.27374 4.958186 129.3565 25.53516
> proc.time()
[1] 0.47 0.04 0.53 0.00 0.00