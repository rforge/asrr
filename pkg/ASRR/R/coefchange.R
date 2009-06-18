coefchange <- function (basemodel,...) 
{
  UseMethod("coefchange")
}

coefchange.default <- function(basemodel, fullmodel,...)
{
  stop(gettextf("No default method for %s model.",class(basemodel)))
}

coefchange.lm <- function(basemodel, fullmodel,overall=FALSE,boot=FALSE,R=500,whichcoef=names(coef(basemodel)),...)
{
  if (! (inherits(basemodel,"lm") && inherits(fullmodel,"lm"))) stop("Basemodel and fullmodel must be 'lm' models.")
  basemod.sum <- summary(basemodel)
  fullmod.sum <- summary(fullmodel) ## because they can be reused latter
  if (sum(basemod.sum$df[1:2]) != sum(fullmod.sum$df[1:2])) warning("The two models have different number of cases.")
  ## if they are different number of case, they are no longer nested models.
  if (boot){
    force(whichcoef)
    result <- coefchange_boot(basemodel,fullmodel,R=R,whichcoef=whichcoef,...)
    return(result)
  } else {
  coef_base <- coef(basemodel)
  coef_full <- coef(fullmodel)
  com_var <- intersect(names(coef_base),names(coef_full)) ## common variables names of the two model
  diff_var <- setdiff(names(coef_full),names(coef_base)) ## variables in full model only.
  coef_change <- coef_base[com_var] - coef_full[com_var] ## difference of the common variables between the models
  vcov_base <- vcov(basemodel)
  vcov_full <- vcov(fullmodel)
  vcov_full2 <- vcov_full[com_var,com_var]
  vcov_coefchange <- vcov_full2 - vcov_base * (fullmod.sum$sigma^2 / basemod.sum$sigma^2)
  ## see clogg et. al.(1995:1274)-> from beginning to formula (18).
  VarofCoefChange <- diag(vcov_coefchange)
  statistic <- coef_change / sqrt(VarofCoefChange) # t distribution
  p.value <- 2 * pt(abs(statistic),df=df.residual(fullmodel),lower.tail = FALSE)
  ans <- cbind(CoefChange=coef_change,Var= VarofCoefChange,T=statistic,p.value=p.value)
  if (overall) {
    ## need more checks.
    pstar <- min(c(length(coef_base),length(diff_var)))
    df2 <- df.residual(fullmodel)
    inv_vcov <- if(length(diff_var) < length(coef_base)) MASS:::ginv(vcov_coefchange) else solve(vcov_coefchange)
    ## Clogg et. al. (1995:1292): If q < p+1, then a generalized inverse must be used.
    F <- drop(t(coef_change) %*% inv_vcov %*% coef_change / pstar)
    p.value2 <- pf(F,pstar,df2,lower=FALSE)
    overall <- list(pstar=pstar,df=df2,F=F,p.value=p.value2)
  } else overall <- NULL
  result <- list(coefchange=ans,overall=overall,variables = diff_var)
  class(result) <- "coefchange"
  result
}
}

coefchange.glm <- function(basemodel, fullmodel,boot=FALSE,R=500,whichcoef=names(coef(basemodel)),...)
{
  if (! (inherits(basemodel,"glm") && inherits(fullmodel,"glm"))) stop("Basemodel and fullmodel must be glm models.")
  basemod.sum <- summary(basemodel)
  fullmod.sum <- summary(fullmodel) ## because they can be reused latter
  if (sum(basemod.sum$df[1:2]) != sum(fullmod.sum$df[1:2])) warning("The two models have different number of cases.")
  if (boot){
    force(whichcoef)
    result <- coefchange_boot(basemodel,fullmodel,R=R,whichcoef=whichcoef,...)
    return(result)
  } else {
  coef_base <- coef(basemodel)
  coef_full <- coef(fullmodel)
  com_var <- intersect(names(coef_base),names(coef_full))
  diff_var <- setdiff(names(coef_full),names(coef_base))
  coef_change <- coef_base[com_var] - coef_full[com_var]
  vcov_base <- vcov(basemodel)
  vcov_full <- vcov(fullmodel)
  vcov_full2 <- vcov_full[com_var,com_var]
  vcov_coefchange <- vcov_full2 + vcov_base %*% solve(vcov_full2) %*% vcov_base - 2 * vcov_base
  ## see clogg et. al.(1995:1280) and DeMaris(2004:292).
  ## solve(vcov_fill2) is the inverse of matrix vcov_full2. See DeMaris (2004:292)
  VarofCoefChange <- diag(vcov_coefchange)
  statistic <- coef_change / sqrt(VarofCoefChange)## normal distribution
  p.value <- 2 * pnorm(abs(statistic),lower=FALSE) 
  ans <- cbind(CoefChange=coef_change,Var=VarofCoefChange,Z=statistic,p.value=p.value)
  result <- list(coefchange=ans,overall=NULL,variables=diff_var)
  class(result) <- "coefchange"
  result
}
}

coefchange_boot <- function(basemodel,fullmodel,R=500,whichcoef=names(coef(basemodel)),...)
{
  require(boot)
  force(whichcoef)
  ##  nvar <- length(coef(basemodel))
  ##  nvar <- eval(substitute(1:nvar,list(nvar=nvar)))
  baseCall <- basemodel$call
  baseCall$data <- quote(dat)
  fullCall <- fullmodel$call
  fullCall$data <- quote(dat)
  ## build the boot.diff function
  boot.diff <- function(data,indices){}
  body(boot.diff)[[2]] <- quote(dat <- data[indices,])
  body(boot.diff)[[3]] <- substitute(fm1 <- formular1,list(formular1=baseCall))
  body(boot.diff)[[4]] <- substitute(fm2 <- formular2,list(formular2=fullCall))
  body(boot.diff)[[5]] <- quote(diff <- coef(fm1)[whichcoef] - coef(fm2)[whichcoef])
  body(boot.diff)[[6]] <- quote(diff)
  # evaluate the boot
  ans <-eval(substitute(boot(data, boot.diff,R=R),list(data=basemodel$call$data,parent.frame())))
  ans
}

print.coefchange <- function(x,digits=3,...)
{
  if (!is.null(x$overall)) {
    cat(gettextf("Overall test of coef change(s):\nF(%d,%d) = %f, Pr = %f.\n\n", 
                 x$overall$pstar,x$overall$df,x$overall$F,x$overall$p.value))
  }
  cat(gettextf(ngettext(length(x$variables),
                        "Coef change(s) when %s is added.",
                        "Coef change(s) when %s are added."),sQuote(paste(x$variables,collapse=","))),"\n")
  print(x$coefchange,digits=digits,...)
}

