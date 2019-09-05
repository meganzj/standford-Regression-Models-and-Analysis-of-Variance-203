petrol 

> library(lattice) # for the fancy plots
> library(nlme) # for the mixed effects model
> 
> data(petrol)
> petrol <- petrol
> petrol[,2:5] <- scale(petrol[,2:5], scale=F)
> attach(petrol)
> 
> print(petrol)
No SG VP V10 EP Y
1 A 11.55 4.41875 -51.5 -127.09375 12.2
2 A 11.55 4.41875 -51.5 -57.09375 22.3
3 A 11.55 4.41875 -51.5 12.90625 34.7
4 A 11.55 4.41875 -51.5 74.90625 45.7
5 B 1.55 -0.68125 -31.5 -114.09375 8.0
6 B 1.55 -0.68125 -31.5 -59.09375 13.1
7 B 1.55 -0.68125 -31.5 14.90625 26.6
8 C 0.75 1.91875 -24.5 -120.09375 7.4
9 C 0.75 1.91875 -24.5 -60.09375 18.2
10 C 0.75 1.91875 -24.5 7.90625 30.4
11 D -0.85 1.91875 -21.5 -97.09375 6.9
12 D -0.85 1.91875 -21.5 -32.09375 15.2
13 D -0.85 1.91875 -21.5 32.90625 26.0
14 D -0.85 1.91875 -21.5 77.90625 33.6
15 E 1.05 0.61875 -10.5 -25.09375 14.4
16 E 1.05 0.61875 -10.5 34.90625 26.8
17 E 1.05 0.61875 -10.5 62.90625 34.9
18 F -7.05 1.01875 -5.5 -65.09375 10.0
19 F -7.05 1.01875 -5.5 27.90625 24.8
20 F -7.05 1.01875 -5.5 69.90625 31.7
21 G 2.05 -2.38125 25.5 -97.09375 2.8
22 G 2.05 -2.38125 25.5 -57.09375 6.4
23 G 2.05 -2.38125 25.5 25.90625 16.1
24 G 2.05 -2.38125 25.5 83.90625 27.8
25 H -1.15 -2.98125 32.5 -47.09375 5.0
26 H -1.15 -2.98125 32.5 32.90625 17.6
27 H -1.15 -2.98125 32.5 111.90625 32.1
28 I -7.05 -1.78125 42.5 18.90625 14.0
29 I -7.05 -1.78125 42.5 91.90625 23.2
30 J -7.45 -3.98125 74.5 32.90625 8.5
31 J -7.45 -3.98125 74.5 46.90625 14.7
32 J -7.45 -3.98125 74.5 95.90625 18.0
> 
> # petrol data
> xyplot(Y ~ EP | No, data=petrol, xlab='End point', ylab='Yield',
+ panel = function(x,y) {
+ panel.grid()
+ m <- sort.list(x)
+ panel.xyplot(x[m], y[m], type='b', lwd=2, col.line='red', pch=18, col.symbol='blue', cex=2)
+ }
+ )
> 
> petrol1.lm <- lm(Y ~ No/EP - 1) # different slope and intercepts
> print(matrix(round(coef(petrol1.lm), 2), 2, 10, byrow=T, dimnames=list(c("b0", "b1"), levels(petrol$No))))
A B C D E F G H I J
b0 32.75 23.62 28.99 21.13 19.82 20.42 14.78 12.68 11.62 6.18
b1 0.17 0.15 0.18 0.15 0.23 0.16 0.14 0.17 0.13 0.13
> 
> petrol2.lm <- lm(Y ~ No + EP - 1) # no interactions
> print(matrix(round(coef(petrol1.lm), 2), 2, 10, byrow=T, dimnames=list(c("b0", "b1"), levels(petrol$No))))
A B C D E F G H I J
b0 32.75 23.62 28.99 21.13 19.82 20.42 14.78 12.68 11.62 6.18
b1 0.17 0.15 0.18 0.15 0.23 0.16 0.14 0.17 0.13 0.13
> 
> print(anova(petrol2.lm, petrol1.lm)) # equivalent to Ftest
Analysis of Variance Table

Model 1: Y ~ No + EP - 1
Model 2: Y ~ No/EP - 1
Res.Df RSS Df Sum of Sq F Pr(>F)
1 21 74.132 
2 12 30.329 9 43.803 1.9257 0.1439
> 
> petrol3.lm <- lm(Y ~ SG + VP + V10 + EP)
> 
> print(anova(petrol3.lm, petrol2.lm))
Analysis of Variance Table

Model 1: Y ~ SG + VP + V10 + EP
Model 2: Y ~ No + EP - 1
Res.Df RSS Df Sum of Sq F Pr(>F) 
1 27 134.804 
2 21 74.132 6 60.672 2.8645 0.03368 *
---
Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 
> 
> # A random intercept model -- intercept varies by "No"
> 
> petrol3.lme.reml <- lme(Y ~ SG + VP + V10 + EP, random = ~ 1 | No)
> petrol3.lme.ml <- lme(Y ~ SG + VP + V10 + EP, random = ~ 1 | No, method="ML")
> 
> print(anova(petrol3.lme.ml, petrol3.lm))
Model df AIC BIC logLik Test L.Ratio p-value
petrol3.lme.ml 1 7 149.3833 159.6435 -67.69166 
petrol3.lm 2 6 148.8308 157.6252 -68.41540 1 vs 2 1.447482 0.2289
> 
> # Try to drop SG and VP from the model
> 
> petrol4.lme.ml <- lme(Y ~ V10 + EP, random = ~ 1 | No, method="ML")
> print(anova(petrol3.lme.ml, petrol4.lme.ml))
Model df AIC BIC logLik Test L.Ratio p-value
petrol3.lme.ml 1 7 149.3833 159.6435 -67.69166 
petrol4.lme.ml 2 5 149.6119 156.9406 -69.80594 1 vs 2 4.22855 0.1207
> 
> # Random slopes for EP?
> 
> petrol5.lme.ml <- lme(Y ~ V10 + EP, random = ~ 1 + EP| No, method="ML")
> print(anova(petrol5.lme.ml, petrol4.lme.ml))
Model df AIC BIC logLik Test L.Ratio p-value
petrol5.lme.ml 1 7 153.6155 163.8757 -69.80776 
petrol4.lme.ml 2 5 149.6119 156.9406 -69.80594 1 vs 2 0.003646107 0.9982
> 
> # Random slopes for EP and V10?
> 
> petrol6.lme.ml <- lme(Y ~ V10 + EP, random = ~ 1 + EP + V10| No, method="ML")
> print(anova(petrol6.lme.ml, petrol4.lme.ml))
Model df AIC BIC logLik Test L.Ratio p-value
petrol6.lme.ml 1 10 154.6156 169.2729 -67.30778 
petrol4.lme.ml 2 5 149.6119 156.9406 -69.80594 1 vs 2 4.996307 0.4163
> proc.time()
[1] 3.65 0.08 8.76 0.00 0.00
> 