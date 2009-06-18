`endog_test` <-
function(object,...){
  ##Testing for Endogeneity.
  UseMethod("endog_test")
}

`endog_test.tsls` <- function(object,...){
cat("  Please use 'tsls2' to refit the model.\n")
}


`endog_test.tsls2` <-
function(object,robust=FALSE,type=c("hc2", "hc0", "hc1", "hc3", "hc4"),...)
#object of class "tsls"
#Wooldridge,J.M.2002.Econometric Analysis of Cross Section and Panel Data.Cambridge,MA:MIT Press.,pp118-121
{
   #inst <- attr(terms(object$instruments),"term.labels") #all instruments
   #indep <- attr(terms(object$formula),"term.labels") # the independent variables
   inst <- colnames(object$Z)
   indep <- colnames(object$model.matrix)
   Z1 <- intersect(inst,indep) 
   Endo.v <- setdiff(indep,Z1)#endogenous variables
   Endo.vmat <- object$model.matrix[,Endo.v,drop=FALSE]
   nEndo.v <- ncol(Endo.vmat)
   Ncase <- nrow(object$model.matrix)
   residuals.aux <- matrix(numeric(0),nrow=Ncase,ncol=nEndo.v)
   for (i in 1:nEndo.v){
   residuals.aux[,i] <- residuals(lm(object$model.matrix[,Endo.v[i],drop=FALSE]~object$Z-1))
}
   lm.aux <- lm(object$response~object$model.matrix + residuals.aux-1)
   if (length(Endo.v)==1) nam <- "residuals.aux" 
   else nam <- paste("residuals.aux",1:nEndo.v,sep="")  #the name of rhos in lm
   ## require(car,quiet=T)
   if (robust) type <- match.arg(type) else type=FALSE
   TEST <- car:::lht(lm.aux,nam,test="F",white.adjust=type)
   result <- list(F=TEST$F[2],Df=abs(TEST$Df[2]),P=TEST$"Pr(>F)"[2],Endo.v=Endo.v)
   class(result) <- "endog_test"
   return(result)
}

`endog_test.ivreg` <-
function(object,robust=FALSE,type=c("hc2", "hc0", "hc1", "hc3", "hc4"),...)
#object of class "ivreg"
{
  if (con <- is.null(object$x)) {
    if (!exists("ivreg")) cat("Please load 'AER' package first.\n")
    object <-  tryCatch(update(object,x=TRUE),error=function(e){})
}
  if (!con | (con & !is.null(object))){
   inst <- colnames(object$x$instruments)
   indep <- colnames(object$x$regressors)
   Z1 <- intersect(inst,indep) 
   Endo.v <- setdiff(indep,Z1)#endogenous variables
   Endo.vmat <- object$x$regressors[,Endo.v,drop=FALSE]
   nEndo.v <- ncol(Endo.vmat)
   Ncase <- nrow(object$x$regressors)
   residuals.aux <- matrix(numeric(0),nrow=Ncase,ncol=nEndo.v)
   for (i in 1:nEndo.v){
   residuals.aux[,i] <- residuals(lm(object$x$regressors[,Endo.v[i],drop=FALSE]~object$x$instruments-1))
}
   lm.aux <- lm(object$y~object$x$regressors + residuals.aux-1)
   if (length(Endo.v)==1) nam <- "residuals.aux" 
   else nam <- paste("residuals.aux",1:nEndo.v,sep="")  #the name of rhos in lm
   ## require(car,quiet=T)
   if (robust) type <- match.arg(type) else type=FALSE
   TEST <- car:::lht(lm.aux,nam,test="F",white.adjust=type)
   result <- list(F=TEST$F[2],Df=abs(TEST$Df[2]),P=TEST$"Pr(>F)"[2],Endo.v=Endo.v)
   class(result) <- "endog_test"
   return(result)
 }
}

`print.endog_test` <-
function(x,...)
{
cat(sprintf("H0 = %s not endogenous.\n\n",paste(sQuote(x$Endo.v),collapse=" ")))
cat(sprintf("F(%d)= %f \n",x$Df,x$F))
cat(       "Prob=",format.pval(x$P),"\n")
invisible(x)
}

