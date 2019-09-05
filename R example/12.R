wtloss 

> 
> # weight loss data
> 
> data(wtloss)
> attach(wtloss)
> 
> #postscript('weightloss.ps', paper='special', height=6, width=6, horizontal=F)
> 
> plot(Days, Weight, pch=23, bg='yellow', cex=2)
> lines(Days, predict(lm(Weight ~ Days)), lty=2, col='green', lwd=3)
> 
> #dev.off()
> 
> # Simple format
> 
> wtloss.start <- c(b0=90, b1=95, th=120)
> wtloss.nls <- nls(Weight ~ b0 + b1 * exp(-Days/th), start=wtloss.start, trace=T)
6679.45 : 90 95 120 
239.0484 : 91.25191 92.46604 165.20223 
41.11602 : 83.4992 100.5141 197.7337 
39.24573 : 81.4347 102.6216 204.5543 
39.2447 : 81.37368 102.68423 204.73386 
> lines(Days, predict(wtloss.nls), lwd=3, col='red')
> 
> 
> 
> proc.time()
[1] 0.58 0.04 0.66 0.00 0.00
> 
