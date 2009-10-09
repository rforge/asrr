"vcov.fem" <-
function(object,robust=F,...) {
if (!isTRUE(robust)) object$vcov else {
##robust covariance for serial correlation
noNA <- object$drop.var
Xbat <- object$Xbat
Ybat <- object$Ybat
XX<-solve(t(Xbat[,noNA])%*%Xbat[,noNA])
fresi<-Ybat-Xbat[,noNA]%*%object$coef
Xu <- Xbat[,noNA]*as.vector(fresi)
S<- crossprod(rowsum(Xu,object$id))#Thanks to Thomas Lumley
rob<-XX%*%S%*%XX
return(rob)
 } 
}

