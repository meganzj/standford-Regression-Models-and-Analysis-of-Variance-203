> invisible(options(echo = TRUE))
> ar1.sim1 <- arima.sim(list(ar=0.95), n=200)
> # AR(1) -- alpha=0.95
> #postscript("ar1-1.ps", paper='special', height=6, width=6, horizontal=F)
> plot(ar1.sim1)
> #dev.off()
> print(arima(ar1.sim1, order=c(1,0,0)))

Call:
arima(x = ar1.sim1, order = c(1, 0, 0))

Coefficients:
ar1 intercept
0.9653 -0.2553
s.e. 0.0182 1.7662

sigma^2 estimated as 0.9403: log likelihood = -278.98, aic = 563.96
> 
> ar1.sim2 <- arima.sim(list(ar=0.5), n=200)
> # AR(1) -- alpha=0.5
> #postscript("ar1-2.ps", paper='special', height=6, width=6, horizontal=F)
> plot(ar1.sim2)
> #dev.off()
> print(arima(ar1.sim2, order=c(1,0,0)))

Call:
arima(x = ar1.sim2, order = c(1, 0, 0))

Coefficients:
ar1 intercept
0.5110 -0.1071
s.e. 0.0612 0.1321

sigma^2 estimated as 0.8431: log likelihood = -266.87, aic = 539.75
> 
> ar2.sim <- arima.sim(list(ar=c(0.9,-0.2)), n=200)
> # AR(2) -- alpha1=0.9, alpha2=-0.2
> #postscript("ar2.ps", paper='special', height=6, width=6, horizontal=F)
> plot(ar2.sim)
> #dev.off()
> print(ar(ar2.sim, order.max=9))

Call:
ar(x = ar2.sim, order.max = 9)

Coefficients:
1 2 3 
0.8435 -0.2468 0.1614 

Order selected 3 sigma^2 estimated as 0.9536 
> 
> arma.sim <- arima.sim(list(ar=c(0.9,-0.2), ma=c(1,2,4,3)), n=200)
> # ARMA(2,4)
> #postscript("arma.ps", paper='special', height=6, width=6, horizontal=F)
> plot(arma.sim)
> #dev.off()
> print(arima(arma.sim, order=c(2,0,4)))

Call:
arima(x = arma.sim, order = c(2, 0, 4))

Coefficients:
ar1 ar2 ma1 ma2 ma3 ma4 intercept
0.9175 -0.2848 1.4082 0.8300 0.5009 0.4136 1.2915
s.e. 0.1963 0.1632 0.1840 0.3059 0.2050 0.0828 2.2211

sigma^2 estimated as 7.904: log likelihood = -494.12, aic = 1004.23
> 
> proc.time()
[1] 0.85 0.06 0.91 0.00 0.00