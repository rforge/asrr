"print.bem" <-
function(object,...){
cat("\n Call:",deparse(object$call),"\n\n")
cat(sprintf(" Total Obs = %f ; Total Units= %f \n",object$N,object$n),
    sprintf("F(%d,%d)   = %f ; Prob > F   = %f \n",object$F[2],object$F[3],object$F[1],object$p.model)
    )
coef<-with(object,cbind(coef,std.Err,tval,p.coef))
colnames(coef)<-c("Coef","Std.Err","T-value","P-value") 
cat("\n\nThe coefficents are:\n")
print.default(round(coef,min(options()$"digits",5)))
cat("\nThe R-square:\n",
    sprintf("Within  R2  = %f \n",object$R2.within),
    sprintf("Betwwen R2  = %f \n",object$R2.between),
    sprintf("Overall R2  = %f \n",object$R2.overall),
    "\n")
}

