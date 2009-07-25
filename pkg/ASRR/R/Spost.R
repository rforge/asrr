listcoef <- function(x,...) {
  if (is.null(x$x) || is.null(x$y))  stop("set 'x=TRUE' and 'y=TRUE' when estimating the model.")
  UseMethod("listcoef")
}

listcoef.lm <- function(x,...){
  ##www.nd.edu/~rwilliam/stats1/x92b.pdf
  ## x is a lm model with x and y setted to TRUE.
  ## SDofX <- apply(x$x,2,sd)
  ## SDofY <- sd(x$y)
  if (is.null(x$weight)) wt <- rep(1/length(x$y), length(x$y)) else wt <- x$weight
  SD <- sqrt(diag(cov.wt(cbind(x$y,x$x),wt=wt,method="ML")$cov))
  SDofY <- SD[1]
  SDofX <- SD[-1]
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
    ##SDofX <- apply(x$x,2,sd)
    ##SDofY <- sd(x$y)
    if (is.null(x$weight)) wt <- rep(1/length(x$y), length(x$y)) else wt <- x$weight
    SD <- sqrt(diag(cov.wt(cbind(x$y,x$x),wt=wt,method="ML")$cov))
    SDofY <- SD[1]
    SDofX <- SD[-1]
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
    SD <- sqrt(diag(cov.wt(cbind(x$y,x$x),wt=wt,method="ML")$cov))
    SDofY <- SD[1]
    SDofX <- SD[-1]  
    ##SDofX <- apply(x$x,2,sd)
    ##SDofY <- sd(x$y)
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
    SDofX <- sqrt(diag(cov.wt(x$x,wt=wt,method="ML")$cov))
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
