> invisible(options(echo = TRUE))
> # Load the library with the Montgomery & Peck data
> # y - service time of vending machines
> # x1 - cases stocked
> # x2 - distance walked
> 
> # Do this first: uncomment the following line by deleting the "#" and
> # install the data from the Montgomery Peck book
> # install.packages('MPV')
> 
> library(MPV)
> 
> # Or, it is located on the course webpage
> # http://www-stat.stanford.edu/~jtaylo/courses/stats203/data/softdrink.table
> 
> 
> # Find the softdrink data
> 
> data(softdrink)
> attach(softdrink)
> print(names(softdrink))
[1] "y" "x1" "x2"
> 
> # Fit the model
> 
> softdrink.lm <- lm(y ~ x1 + x2, data=softdrink)
> print(summary(softdrink.lm))

Call:
lm(formula = y ~ x1 + x2, data = softdrink)

Residuals:
Min 1Q Median 3Q Max 
-5.7880 -0.6629 0.4364 1.1566 7.4197 

Coefficients:
Estimate Std. Error t value Pr(>|t|) 
(Intercept) 2.341231 1.096730 2.135 0.044170 * 
x1 1.615907 0.170735 9.464 3.25e-09 ***
x2 0.014385 0.003613 3.981 0.000631 ***
---
Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

Residual standard error: 3.259 on 22 degrees of freedom
Multiple R-Squared: 0.9596,	Adjusted R-squared: 0.9559 
F-statistic: 261.2 on 2 and 22 DF, p-value: 4.687e-16 

> 
> # Confidence intervals
> # for all x's in the data set
> 
> print(predict(softdrink.lm, interval='confidence'))
fit lwr upr
1 21.708084 19.551297 23.864872
2 10.353615 8.556216 12.151013
3 12.079794 9.955744 14.203843
4 9.955646 7.980522 11.930770
5 14.194398 12.343039 16.045757
6 18.399574 17.000017 19.799132
7 7.155376 5.222061 9.088692
8 16.673395 14.966973 18.379818
9 71.820294 67.048610 76.591977
10 19.123587 16.128667 22.118507
11 38.092507 36.108636 40.076378
12 21.593041 19.314141 23.871941
13 12.472991 10.801755 14.144227
14 18.682464 16.791631 20.573297
15 23.328798 21.958209 24.699388
16 29.662928 26.909298 32.416559
17 14.913640 13.265705 16.561574
18 15.551379 13.454112 17.648645
19 7.706807 5.607492 9.806121
20 40.887970 38.732422 43.043518
21 20.514179 17.766059 23.262299
22 56.006528 51.776559 60.236497
23 23.357568 21.984492 24.730644
24 24.402854 22.055286 26.750421
25 10.962584 9.217532 12.707636
> 
> # Prediction intervals
> # for all x's in the data set
> 
> print(predict(softdrink.lm, interval='prediction'))
fit lwr upr
1 21.708084 14.6126113 28.80356
2 10.353615 3.3589989 17.34823
3 12.079794 4.9942032 19.16538
4 9.955646 2.9132656 16.99803
5 14.194398 7.1857225 21.20307
6 18.399574 11.4964758 25.30267
7 7.155376 0.1246073 14.18615
8 16.673395 9.7016031 23.64519
9 71.820294 63.5460584 80.09453
10 19.123587 11.7301065 26.51707
11 38.092507 31.0476684 45.13735
12 21.593041 14.4595011 28.72658
13 12.472991 5.5097274 19.43625
14 18.682464 11.6632578 25.70167
15 23.328798 16.4315145 30.22608
16 29.662928 22.3638539 36.96200
17 14.913640 7.9559322 21.87135
18 15.551379 8.4737709 22.62899
19 7.706807 0.6285915 14.78502
20 40.887970 33.7928734 47.98307
21 20.514179 13.2171816 27.81118
22 56.006528 48.0324043 63.98065
23 23.357568 16.4597897 30.25535
24 24.402854 17.2470809 31.55863
25 10.962584 3.9812364 17.94393
> 
> # For new x's
> 
> newdata <- list(x1=c(9,5),x2=c(200,500))
> 
> print(predict(softdrink.lm, newdata, interval='confidence'))
fit lwr upr
1 19.76136 17.63680 21.88592
2 17.61318 15.25645 19.96992
> print(predict(softdrink.lm, newdata, interval='prediction'))
fit lwr upr
1 19.76136 12.67562 26.84710
2 17.61318 10.45440 24.77197
> 
> # Another way to compute R^2
> 
> SST <- sum((y - mean(y))^2)
> SSR <- sum((predict(softdrink.lm) - mean(y))^2)
> SSE <- SST - SSR # SSE <- sum(resid(softdrink.lm)^2)
> print(c(SSR / SST, summary(softdrink.lm)$r.squared))
[1] 0.9595937 0.9595937
> proc.time()
[1] 0.54 0.04 0.61 0.00 0.00
> 
