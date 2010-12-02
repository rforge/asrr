## This is part of the QCA3 package
## By ronggui HUANG 2010.

directCalibration <- function(x,fullin, fullout, crossover, infz=0.95, outfz=0.05) {
    var.dev <- x - crossover
    inLogOdd <- log(infz/(1-infz))
    outLogOdd <- log(outfz/(1-outfz))
    inScalar <- inLogOdd/(fullin - crossover)
    outScalar <- outLogOdd/(fullout - crossover)
    scalars <-  rep(NA,length(x))
    scalars[var.dev > 0] <- inScalar
    scalars[var.dev < 0] <- outScalar
    product <-  scalars * var.dev
    fz <- exp(product )/(1+exp(product))
    ans <- data.frame(x=x,deviations=var.dev,scalars=scalars,logOdd=product,membership=fz)
    ans
}


