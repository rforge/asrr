round(apply(CoD[,c("econdev","eduhi","ethnolin","close","demex","nocom","parlia","efpahi","decent")],2,FUN=function(x) coverage(x,CoD$cod)),3)
