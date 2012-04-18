ginfluence <- function(object) {
## author: Ronggui HUANG
## disgnostics for logistic regression based on covariate patterns, which is impleted in Stata
## described in Hosmer, D. W. & Lemeshow, S. Applied Logistic Regression. John Wiley & Sons, 2000
## object is a logistic model fitted by glm()
    if (!(object$family$family == "binomial" && object$family$link == "logit")) {
        stop("Object is not a logistic model.")
    }
    if (is.null(object[["x"]])) {
        ## when x=FALSE, object$ becomes object$xlevels
        objectN <- update(object, x=TRUE)
        x <- objectN$x
    } else x <- object[["x"]]
    ## what to do if desgin matrix or response is not in the object?
    ## how to handle missing values?
    X <- mgcv:::uniquecombs(x)
    idx  <- attr(X,"index")
    prj <- binomial()$linkinv(X %*% coef(object))
    mj <- sapply(1:nrow(X),FUN=function(x) sum(idx==x))
    vj <- as.vector(mj * prj * (1-prj))
    V <- diag(vj)
    inv <- solve(t(X) %*% V %*% X)
    ghatvalues <- diag( sqrt(V) %*% X %*% inv %*% t(X) %*% sqrt(V) )
    ghatvalues <- ghatvalues[idx]
    prob <- prj[idx]
    m <- mj[idx]
    y <- object$y
    yj <- ave(y,idx,FUN=sum)
    residuals <- (yj-m*prob)/sqrt(m*prob*(1-prob))
    ## pearson residuals
    rstandard <- residuals/sqrt(1-ghatvalues)
    dbeta <- rstandard^2*ghatvalues/(1-ghatvalues)
    ## two limiting cases
    dj.s1 <- which(yj==0)
    djs1 <- -1*sqrt(2*m[dj.s1]*abs(log(1-prob[dj.s1])))
    dj.s2 <- which(yj==m)
    djs2 <- sqrt(2*m[dj.s2]*abs(log(prob[dj.s2])))
    deviance <- rep(NA,length(y)) ## deviance residuals
    deviance[dj.s1] <- djs1
    deviance[dj.s2] <- djs2
    dj.others <- -1* c(dj.s1,dj.s2)
    deviance[dj.others] <- sign(yj[dj.others] - m[dj.others]*prob[dj.others])*
                           sqrt(2*( yj[dj.others]*log(yj[dj.others]/(m[dj.others]*prob[dj.others])) +
                                   (m[dj.others]-yj[dj.others])*log((m[dj.others]-yj[dj.others])/
                                   (m[dj.others]*(1-prob[dj.others])))))
    dx2 <- rstandard^2
    ddeviance <- deviance^2/(1-ghatvalues)
    ans <- data.frame(ghatvalues=ghatvalues,prob=prob,m=m,
                      residuals=residuals,rstandard=rstandard,dbeta=dbeta,
                      deviace=deviance,ddeviance=ddeviance,dx2=dx2)
    ans
}
