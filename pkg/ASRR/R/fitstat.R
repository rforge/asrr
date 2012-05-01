## need improvement when weight is not NULL, esp. N changes

print.fitstat <- function(x, ...){
    cat(
        "\n",
        sprintf("McFadden R2           = %f \n",x$McFadden.R2),
        sprintf("McFadden Adj R2       = %f \n",x$McFadden.Adj.R2),
        sprintf("ML R2                 = %f \n",x$ML.R2),
        sprintf("Cragg & Uhler R2      = %f \n",x$Cragg.Uhler.R2),
        if (!is.na(x$Efron.R2)){
        sprintf("Efron R2              = %f \n",x$Efron.R2)},
        sprintf("McKelvey & Zavoina R2 = %f \n",x$M.Z.R2),
        sprintf("Count R2              = %f \n",x$Count.R2),
        sprintf("Count Adj R2          = %f \n",x$Count.Adj.R2),
        sprintf("AIC                   = %f \n",x$AIC),
        sprintf("AIC*n                 = %f \n",x$AICplusN),
        sprintf("BIC                   = %f \n",x$BIC1),
        sprintf("BIC'                  = %f \n",x$BIC2),
        "\n Note:The maxinum of ML R2 is less than 1. \n"
        )
}

fitstat.glm <- function(x, ...) {
    na <- na.action(x)
    if (!is.null(x$weight)) stop("It has not been implemented for weighted models.")
    if (is.null(na)) x.base <- update(x,.~1)
    else x.base <- update(x,.~1,subset=-na.action(x))
    LRT <- anova(x.base, x, test="LRT")
    if (x$family$family == "binomial" && (x$family$link %in% c("logit","probit"))){
        y <- x$y
        pr <- predict(x,type="response")
        Efron.R2 <- 1 - sum((y-pr)^2) / sum((y-mean(y))^2)
        ystar <- predict(x, type="link")
        Vystar <- var(ystar)
        M.Z.R2 <-  if (x$family$link == "logit"){
            Vystar/(Vystar+pi^2/3)
        } else {
            Vystar/(Vystar+1)
        }
        yhat <- as.numeric(pr >= 0.5)
        tab <- table(x$y, yhat)
        maxrow <- max(rowSums(tab))
        Count.R2 <- sum(diag(tab))/sum(tab)
        Count.Adj.R2 <- (sum(diag(tab))-maxrow)/(sum(tab)-maxrow)
        L.full <- logLik(x)
        P <- attr(L.full,"df")
        L.base <- logLik(x.base)
        Deviance=deviance(x)
        if (attr(L.full,"n")!= attr(L.base,"n")) stop("The number of obs in the intercept only model differs.")
        N <- attr(L.full,"n")
        attributes(L.full) <- attributes(L.base) <- NULL
        McFadden.R2 <- 1-(L.full/L.base)
        McFadden.Adj.R2 <- 1-((L.full-P)/L.base)
        ML.R2 <- 1 - exp(2*(L.base-L.full)/N) ## AKA. Cox-Snell
        Cragg.Uhler.R2 <- ML.R2/(1-exp(2*L.base/N)) ## AKA Nagelkerke
        AIC <- (-2*L.full+2*P)/N ## definition of Long(1997:109)
        AICplusN <- AIC*N
        BIC1 <- Deviance - (N-P)*log(N)
        BIC2 <- -2*(L.full-L.base) + (P-1)*log(N) ## P-1 is number of IVs
        ans <- list(LL=L.full,LL0=L.base,Deviance=Deviance, LRT=LRT,
                    Count.R2=Count.R2, Count.Adj.R2=Count.Adj.R2,
                    Efron.R2=Efron.R2, M.Z.R2=M.Z.R2,
                    McFadden.R2=McFadden.R2,McFadden.Adj.R2=McFadden.Adj.R2,
                    ML.R2=ML.R2,Cragg.Uhler.R2=Cragg.Uhler.R2,
                    AIC=AIC, AICplusN=AICplusN, BIC1=BIC1, BIC2=BIC2)
        class(ans) <- "fitstat"
        ans
    }
}

fitstat.polr <- function(x,...){
    na <- na.action(x)
    if (is.null(na)) x.base <- update(x,.~1)
    else x.base <- update(x,.~1,subset=-na.action(x))
    LRT <- anova(x.base, x, test="Chisq")
    if (is.null(x$model)) x <- update(x, model=TRUE)
    yhat <- predict(x)
    y <- model.response(x$model)
    tab <- table(y, yhat)
    maxrow <- max(rowSums(tab))
    Count.R2 <- sum(diag(tab))/sum(tab)
    Count.Adj.R2 <- (sum(diag(tab))-maxrow)/(sum(tab)-maxrow)
    L.full <- logLik(x)
    P <- attr(L.full,"df")
    L.base <- logLik(x.base)
    Deviance=deviance(x)
    if (attr(L.full,"n")!= attr(L.base,"n")) {
        stop("The number of obs in the intercept only model differs.")
    }
    N <- attr(L.full,"n")
    attributes(L.full) <- attributes(L.base) <- NULL
    McFadden.R2 <- 1-(L.full/L.base)
    McFadden.Adj.R2 <- 1-((L.full-P)/L.base)
    ML.R2 <- 1 - exp(2*(L.base-L.full)/N) ## AKA. Cox-Snell
    Cragg.Uhler.R2 <- ML.R2/(1-exp(2*L.base/N)) ## AKA Nagelkerke
    AIC <- (-2*L.full+2*P)/N ## definition of Long(1997:109)
    AICplusN <- AIC*N
    BIC1 <- Deviance - (N-P)*log(N)
    BIC2 <- -2*(L.full-L.base) + (P - length(x$lev) + 1)*log(N)
    ## number of IVs=df-(No.y.levels-1)
    Vystar <- var(x$lp)
    M.Z.R2 <-  if (x$method == "logistic"){
        Vystar/(Vystar+pi^2/3)
    } else  if (x$method == "probit") {
        Vystar/(Vystar+1)
    } else {NA}
    ans <- list(LL=L.full,LL0=L.base,Deviance=Deviance, LRT=LRT,
                Count.R2=Count.R2, Count.Adj.R2=Count.Adj.R2,
                Efron.R2=NA, M.Z.R2=M.Z.R2,
                McFadden.R2=McFadden.R2,McFadden.Adj.R2=McFadden.Adj.R2,
                ML.R2=ML.R2,Cragg.Uhler.R2=Cragg.Uhler.R2,
                AIC=AIC, AICplusN=AICplusN, BIC1=BIC1, BIC2=BIC2)
    class(ans) <- "fitstat"
    ans
}

