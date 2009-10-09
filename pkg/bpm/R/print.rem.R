"print.rem" <-
function(object,...){
cat("\n Call:",deparse(object$call),"\n\n")
cat(sprintf("Total Obs = %f ; Total Units = %f \n\n",object$N,object$n),
    sprintf("Obs.per.grp.min = %f\n",object$g[1]),
    sprintf("Obs.per.grp.min = %f\n",object$g[2]))
coef<-with(object,cbind(coef,std.Err,tval,p.coef))
colnames(coef)<-c("Coef","Std.Err","Z-value","P>|Z|") 
cat("\nThe coefficents are:\n")
print.default(round(coef,min(options()$"digits",6)))
cat("\nThe R-square:\n",
    sprintf("Within  R2  = %f \n",object$R2.within),
    sprintf("Betwwen R2  = %f \n",object$R2.between),
    sprintf("Overall R2  = %f \n",object$R2.overall),
    "\n")
}

