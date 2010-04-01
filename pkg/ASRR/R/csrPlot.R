csrPlot <- function(x,...){
  ## x is a coxph model fitted by survial::coxph
  ## see Box-Steffensmier and Jones(2004:124-145)
  require(survival)
  martingale <- residuals(x,type="martingale")
  coxSnellResidual <- 1- martingale
  ## Tableman and Kim (2003) about relationship between two types of residuals
  plot(survfit(Surv(coxSnellResidual,x$y[,'status'])~1),fun="cumhaz",firstx=0, 
       xlab="cox-snell residuals",ylab="H(t) based on cox-snell residuals")
  abline(a=0,b=1, lty=3)
}
