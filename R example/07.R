> invisible(options(echo = TRUE))
> # Rehab example
> 
> rehab.table <- read.table('http://www-stat.stanford.edu/~jtaylo/courses/stats203/data/rehab.table', header=T, sep=',')
> attach(rehab.table)
> rehab.lm <- lm(Time ~ factor(Fitness))
> anova(rehab.lm)
Analysis of Variance Table

Response: Time
Df Sum Sq Mean Sq F value Pr(>F) 
factor(Fitness) 2 672.00 336.00 16.962 4.129e-05 ***
Residuals 21 416.00 19.81 
---
Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 
> 
> # Check F-test
> 
> source('http://www-stat.stanford.edu/~jtaylo/courses/stats203/R/inference+polynomial/Ftest.R')
> Ftest(rehab.lm, lm(Time ~ 1))
F df.N df.D pval
1 16.96154 2 21 4.129301e-05
> 
> # Kidney data
> 
> kidney.table <- read.table('http://www-stat.stanford.edu/~jtaylo/courses/stats203/data/kidney.table', header=T)
> kidney.table$Duration <- factor(kidney.table$Duration)
> kidney.table$Weight <- factor(kidney.table$Weight)
> kidney.table$logDays <- log(kidney.table$Days + 1)
> attach(kidney.table)
> 
> # Fit model: use log days to "stabilize" variance
> 
> kidney.lm <- lm(logDays ~ Duration * Weight)
> 
> # Look at cell means to visually inspect for interactions
> # Interactions will be present if red "curve" is
> # very different from "blue" curve
> 
> new.grid <- data.frame(Duration=factor(rep(c(1,2),3)), Weight=factor(c(1,1,2,2,3,3))
+ )
> predict(kidney.lm, new.grid)
1 2 3 4 5 6 
1.0211560 0.9169518 1.8650191 1.3377192 2.5482706 1.9949474 
> new.grid$Yhat <- predict(lm(logDays ~ Duration * Weight), new.grid)
> 
> col=c('red', 'blue')
> 
> # Cell means
> plot(as.numeric(new.grid$Weight), new.grid$Yhat, type='n', xlab='Weight', ylab='Cell mean')
> for (i in 1:3) {
+ for (j in 1:2) {
+ points(new.grid$Weight[2*(i-1) + j], new.grid$Yhat[2*(i-1)+j], pch=23, bg=col[j], cex=2)
+ }}
> 
> # Look at ANOVA table
> 
> print(anova(kidney.lm))
Analysis of Variance Table

Response: logDays
Df Sum Sq Mean Sq F value Pr(>F) 
Duration 1 2.3397 2.3397 4.3583 0.04156 * 
Weight 2 16.9713 8.4856 15.8067 3.945e-06 ***
Duration:Weight 2 0.6357 0.3178 0.5920 0.55675 
Residuals 54 28.9892 0.5368 
---
Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 
> 
> # Look at interactions
> 
> print('Interaction Ftest')
[1] "Interaction Ftest"
> print(Ftest(kidney.lm, lm(logDays ~ Duration + Weight)))
F df.N df.D pval
1 0.5920404 2 54 0.5567479
> 
> # Look for main effect of Weight: note it is different from
> # anova(.) output! Should use anova output in general,
> # unless you really know there are no interactions.
> 
> print('Weight main effect Ftest')
[1] "Weight main effect Ftest"
> print(Ftest(lm(logDays ~ Duration + Weight), lm(logDays ~ Duration)))
F df.N df.D pval
1 16.04045 2 56 3.108672e-06
> 
> print('Duration main effect Ftest')
[1] "Duration main effect Ftest"
> print(Ftest(lm(logDays ~ Duration + Weight), lm(logDays ~ Weight)))
F df.N df.D pval
1 4.422732 1 56 0.03996966
> 
> proc.time()
[1] 0.62 0.05 0.75 0.00 0.00