weighted.summary <- function(x,wt=rep(1,length(x)),type="analytic"){
  idx <- (!is.na(x)) & (!is.na(wt))
  x <- x[idx]
  wt <- wt[idx]
  n <- length(x)
  moment <- function(r,mu){
    1/n * sum(wt*((x-mu)^r))
  }
  if (type=="analytic"){
    wt <- wt*n/sum(wt)
    mean <- sum(wt*x)/n
    variance <- sum(wt*((x-mean)^2))/(n-1)
    sd <- sqrt(variance)
    skewness <- moment(3,mean)* (moment(2,mean)^(-3/2))
    kurtosis <- moment(4,mean)* (moment(2,mean)^-2)
  }
  ans <- list(mean=mean,variance=variance,sd=sd,skewness=skewness,kurtosis=kurtosis)
  ans
}

listcoef <- function(x,...) {
  if (is.null(x$x) || is.null(x$y)){
    stop("Please set 'x=TRUE' and 'y=TRUE' when estimating the model.")
  }
  UseMethod("listcoef")
}

listcoef.lm <- function(x,...){
  ##www.nd.edu/~rwilliam/stats1/x92b.pdf
  ## x is a lm model with x and y setted to TRUE.
  if (is.null(x$weight)) wt <- rep(1/length(x$y), length(x$y)) else wt <- x$weight
  ##SD <- sqrt(diag(cov.wt(cbind(x$y,x$x),wt=wt,method="ML")$cov))
  ##SDofY <- SD[1]
  ##SDofX <- SD[-1]
  SDofX <- apply(x$x,2,function(xx) weighted.summary(xx,wt)$sd)
  SDofY <- weighted.summary(x$y,wt)$sd
  Coef <- summary(x)$coef
  Est <- Coef[,"Estimate"]
  bStdY <- Est/SDofY
  bStdX <- Est*SDofX
  bStdXY <- Est*SDofX/SDofY
  ans <- cbind(Coef,bStdX=bStdX,bStdY=bStdY,bStdXY=bStdXY,SDofX=SDofX,SDofY=SDofY)
  ans
}

listcoef.glm <- function(x,...){
  listcoef_logistic <- function(x,...){
    ## logistic model
    ## http://www.nd.edu/~rwilliam/xsoc73994/L03.pdf
    ## http://www.nd.edu/~rwilliam/zsoc694/x04.pdf
    Xb <- predict(x)
    if (is.null(x$weight)) wt <- rep(1/length(x$y), length(x$y)) else wt <- x$weight
    ##SD <- sqrt(diag(cov.wt(cbind(x$y,x$x),wt=wt,method="ML")$cov))
    ##SDofY <- SD[1]
    ##SDofX <- SD[-1]
    SDofX <- apply(x$x,2,function(x) weighted.summary(x,wt)$sd)
    SDofY <- weighted.summary(x$y)$sd
    SDofYstar <- sqrt(var(Xb)+pi^2/3)
    Coef <- summary(x)$coef
    Est <- Coef[,"Estimate"]
    bStdY <- Est/SDofYstar
    bStdX <- Est*SDofX
    bStdXY <- Est*SDofX/SDofYstar
    ans <- cbind(Coef,"e^b"=exp(Est),
                 bStdX=bStdX,bStdY=bStdY,bStdXY=bStdXY,
                 "e^bStdX"=exp(bStdX),"e^bStdY"=exp(bStdY),"e^bStdXY"=exp(bStdXY),
                 SDofX=SDofX,SDofY=SDofY,"SDofY*"=SDofYstar)
    ans
  }
  listcoef_probit <- function(x,...){
    Xb <- predict(x)
    if (is.null(x$weight)) wt <- rep(1/length(x$y), length(x$y)) else wt <- x$weight
    ##SD <- sqrt(diag(cov.wt(cbind(x$y,x$x),wt=wt,method="ML")$cov))
    ##SDofY <- SD[1]
    ##SDofX <- SD[-1]  
    SDofX <- apply(x$x,2,function(x) weighted.summary(x,wt)$sd)
    SDofY <- weighted.summary(x$y)$sd
    SDofYstar <- sqrt(var(Xb)+1)
    Coef <- summary(x)$coef
    Est <- Coef[,"Estimate"]
    bStdY <- Est/SDofYstar
    bStdX <- Est*SDofX
    bStdXY <- Est*SDofX/SDofYstar
    ans <- cbind(Coef, bStdX=bStdX,bStdY=bStdY,bStdXY=bStdXY,
                 SDofX=SDofX,SDofY=SDofY,"SDofY*"=SDofYstar)
    ans
  }
  listcoef_poisson <- function(x,...){
    ##SDofX <- apply(x$x,2,sd)
    if (is.null(x$weight)) wt <- rep(1/length(x$y), length(x$y)) else wt <- x$weight
    ##SDofX <- sqrt(diag(cov.wt(x$x,wt=wt,method="ML")$cov))
    SDofX <- apply(x$x,2,function(x) weighted.summary(x,wt)$sd)
    Coef <- summary(x)$coef
    Est <- Coef[,"Estimate"]
    bStdX <- Est*SDofX
    ans <- cbind(Coef,bStdX=bStdX,"e^b"=exp(Est),"e^bStdX"=exp(bStdX),
                 "%"= (exp(Est)-1)*100,"%StdX"= (exp(bStdX)-1)*100,SDofX=SDofX)
  }
  ans <- NULL
  if (x$family$family == "binomial" && x$family$link == "logit") { ans <- listcoef_logistic(x) }
  if (x$family$family == "binomial" && x$family$link == "probit") { ans <- listcoef_probit(x) }
  if (x$family$family == "poisson" && x$family$link == "log") { ans <- listcoef_poisson(x) }
  ans
}


fitstat <- function(x,...) {
  if (is.null(x$y)){
    stop("Please set 'y=TRUE' when estimating the model.")
  }
  UseMethod("fitstat")
}

fitstat.lm <- function(x,...){
  mod0 <- lm(x$y~1)
  LL0 <-  logLik(mod0)[1]
  LL <- logLik(x)[1]
  Deviance <- -2*LL ## Deviance <- deviance(x)
  sum_lm <- summary(x)
  n <- sum(sum_lm$df[1:2])
  aic <- (Deviance+2*sum_lm$df[1])/n
  aicxn <- Deviance+2*sum_lm$df[1]
  bic <- Deviance - sum_lm$df[2]*log(n)
  ## 3 versions of BIC, differences between two models are the same.
  r.squared <- sum_lm$r.squared
  adj.r.squared <- sum_lm$adj.r.squared
  ans <- list(Deviance=Deviance,LL=LL,LL0=LL0,aic=aic,"aic*n"=aicxn,bic=bic,r.squared=r.squared,adj.r.squared=adj.r.squared)
  ans
}
