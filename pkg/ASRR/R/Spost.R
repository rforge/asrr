## This is part of the ASRR package
## by Ronggui HUANG (2010).

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
    UseMethod("listcoef")
}

listcoef.lm <- function(x,...){
  ## www.nd.edu/~rwilliam/stats1/x92b.pdf
  ## x is a lm model with x and y setted to TRUE.
    X <- model.matrix(x)
    Y <- model.response(model.frame(x))
    if (is.null(x$weight)) wt <- rep(1/length(Y), length(Y)) else wt <- x$weight
    SDofX <- apply(X,2,function(xx) weighted.summary(xx,wt)$sd)
    SDofY <- weighted.summary(Y,wt)$sd
    Coef <- summary(x)$coef
    Est <- Coef[,"Estimate"]
    bStdY <- Est/SDofY
    bStdX <- Est*SDofX
    bStdXY <- Est*SDofX/SDofY
    ans <- cbind(Coef,bStdX=bStdX,bStdY=bStdY,bStdXY=bStdXY,SDofX=SDofX,SDofY=SDofY)
    ans
}

listcoef.glm <- function(x,...){
    x$y <- model.response(model.frame(x))
    x$x <- model.matrix(x)
    ## model reponse (Y) and model matrix (X)
    listcoef_logistic <- function(x,...){
        ## logistic model
        ## http://www.nd.edu/~rwilliam/xsoc73994/L03.pdf
        ## http://www.nd.edu/~rwilliam/zsoc694/x04.pdf
        Xb <- predict(x)
        if (is.null(x$weight)) wt <- rep(1/length(x$y), length(x$y)) else wt <- x$weight
        ## maybe use weights() directly
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
        if (is.null(x$weight)) wt <- rep(1/length(x$y), length(x$y)) else wt <- x$weight
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
    UseMethod("fitstat")
}

fitstat.default <- function(x,...) {
    cat("No method for this class Now!\n")
}

fitstat.lm <- function(x,...){
    if (is.null(x$y)) x$y <- model.response(model.frame(x))
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

fitstat.multinom <- function(x,...) {
    ## thanks Ripley, B. D. for telling how to get the LogLik and when is not obivous.
    object <- x
    if (!is.null(object$call$summ) && !identical(object$call$summ,0)) {
        stop("Sorry! When 'summ' argument is not zero,I don't know how to get Loglik!\n")
    } ## Improvement can be done here

    object.base <- update(object,.~1,trace=FALSE)
    dev.base <- object.base$deviance ; L.base <- - dev.base/2
    dev.full <- object$deviance ; L.full <- - dev.full/2
    G2 <- dev.base - dev.full
    df <- object$edf - object.base$edf
    LR.test.p <- pchisq(G2,df,lower=F)
    aic <- object$AIC
    n<-dim(object$residuals)[1]
    ## get the predict value to cal count R2
    pre <- predict(object,type="class")
    y <- model.response(model.frame(object))
    ## y <- eval.parent(object$call$data)[,as.character(object$call$formula[[2]])]
    if (!identical(length(y),length(pre))) stop("Length not matched.\nMaybe you should use na.action=\"na.exclude\" in modelling.")
    tab <- table(y,pre)
    if (!identical(dim(tab)[1],dim(tab)[2])) stop("pred and y have diff nlevels")
    ad <- max(rowSums(tab))#max of row sum
    ## cal R2
    ML.R2 <- 1-exp(-G2/n)
    Count.R2 <- sum(diag(tab))/sum(tab)
    Count.adj.R2 <- (sum(diag(tab))-ad)/(sum(tab)-ad)
    McFadden.R2 <- 1-(L.full/L.base)
    McFadden.Adj.R2 <- 1-((L.full-object.base$edf)/L.base)
    Cragg.Uhler.R2 <- ML.R2/(1-exp(2*L.base/n))
    ##get the result
    res <- list(LR=G2,df=df,LR.test.p =LR.test.p ,aic=aic,ML.R2=ML.R2,Cragg.Uhler.R2=Cragg.Uhler.R2,
                McFadden.R2 =McFadden.R2 ,McFadden.Adj.R2=McFadden.Adj.R2,Count.R2=Count.R2,
                Count.adj.R2=Count.adj.R2)
    ##print the result
    cat("\n",
        paste(rep("-",21)),
        "\n The Fitstats are : \n",
        sprintf("G2(%d) = %f",df,G2),
        " ,Prob ",format.pval(LR.test.p),
        "\n",sprintf("AIC   = %f",aic),
        sprintf(",ML.R2 = %f \n",ML.R2),
        paste(rep("-",21)),"\n",
        sprintf("Cragg.Uhler.R2  = %f \n",Cragg.Uhler.R2),
        sprintf("McFadden.R2     = %f \n",McFadden.R2),
        sprintf("McFadden.Adj.R2 = %f \n",McFadden.Adj.R2),
        sprintf("Count.R2        = %f \n",Count.R2),
        sprintf("Count.adj.R2    = %f \n",Count.adj.R2),
        "\n Note:The maxinum of ML R2 is less than 1 \n",
        paste(rep("-",21)),"\n")
    invisible(res)
}

fitstat.multinomial <- function(x,...) {
    ## function for multinomial logistic model using VGAM package
    ## N: object@misc$n 个案数
    ## logLik: logLik(object)
    ## K: object@misc$p 变量个数（包括常数项）
    ## K*: object@misc$p.big  需要拟合的参数数量
    ## the null model
    object <- x
    base.call <- object@call ; base.call[[2]][[3]] <- 1
    object.base <- eval(base.call)

    L.full <- logLik(object)
    L.base <- logLik(object.base)
    N <- object@misc$n

    ##LR test of the model (compared to null model)
    LR <- G2 <- 2*(L.full - L.base)
    df.LR <- object@misc$p.big - object.base@misc$p.big
    LR.test.p <- pchisq(LR,df.LR,lower=F)

    ## deviance.model <- deviance(object)
    aic <- AIC(object) #AIC

    ## get the prediction
    prediction <- object@fitted.values
    original <- object@y
    if (!identical((level <- colnames(prediction)),colnames(original)))
        stop("The levels are not identical!")
    prediction <- factor(level[max.col(prediction)],level)
    original <- factor(level[max.col(original)],level)
    tab <- table(original,prediction)
    ad <- max(rowSums(tab))

    ## cal R2
    Count.R2 <- sum(diag(tab))/sum(tab)
    Count.adj.R2 <- (sum(diag(tab))-ad)/(sum(tab)-ad)
    McFadden.R2 <- 1-(L.full/L.base)
    McFadden.Adj.R2 <- 1-((L.full-object@misc$p.big)/L.base)
    ML.R2 <- 1 - exp(2*(L.base-L.full)/N) #get the same result ML.R2 <- 1-exp(-G2/N)
    Cragg.Uhler.R2 <- ML.R2/(1-exp(2*L.base/N))

    ## return the result
    fitstat <- list(LR.test=list(LR=LR,DF=df.LR,p=LR.test.p),R2=list(McFadden.R2=McFadden.R2,McFadden.Adj.R2=McFadden.Adj.R2,ML.R2=ML.R2,Cragg.Uhler.R2=Cragg.Uhler.R2,Count.R2,Count.adj.R2,AIC=aic))

    ## print the reslut prettily.
    cat( paste(rep("-",21)),
        "\nThe levels of y is:",
        object@misc$ynames,
        "\nThe base category is :",
        sQuote(object@misc$ynames[[length(object@misc$ynames)]]),
        "\n\n")

    cat(paste(rep("-",21)),
        "\n",
        sprintf("LR(%d) = %f    ",df.LR,LR),
        "\n",
        "Prob ",format.pval(LR.test.p),
        "\n\n",
        sprintf("McFadden.R2     = %f \n",McFadden.R2),
        sprintf("McFadden.Adj.R2 = %f \n",McFadden.Adj.R2),
        sprintf("ML.R2           = %f \n",ML.R2),
        sprintf("Cragg.Uhler.R2  = %f \n",Cragg.Uhler.R2),
        sprintf("Count.R2        = %f \n",Count.R2),
        sprintf("Count.adj.R2    = %f \n",Count.adj.R2),
        sprintf("AIC             = %f \n",aic),
        "\n Note:The maxinum of ML R2 is less than 1 \n",
        paste(rep("-",21)),"\n")
    invisible(fitstat)
}
