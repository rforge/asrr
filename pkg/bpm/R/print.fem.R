"print.fem" <-
function(object,...){
cat("\n Call:",deparse(object$call),"\n\n")
cat(sprintf(" Total Observations = %f ; Total Units= %f ; DF.model= %f\n\n",object$N
            ,object$n,object$df.model))
coef<-with(object,cbind(coef,std.Err,tval,p.coef))
colnames(coef)<-c("Coef","Std.Err","T-value","P-value") 
cat("The coefficents are:\n")
print.default(round(coef,min(options()$"digits",5)))
cat("\nThe F tests are:\n",
    sprintf(" F for model= %f \n",object$F.model),
    sprintf(" P(model)   = %f \n",object$p.model),
    sprintf(" F for ui   = %f \n",object$F.test.ui),
    sprintf(" P(all.ui=0)= %f \n",object$P.ui),
    "\nThe R-square:\n",
    sprintf("Within  R2  = %f \n",object$R2.within),
    sprintf("Betwwen R2  = %f \n",object$R2.between),
    sprintf("Overall R2  = %f \n",object$R2.overall),
    "\n")
}

