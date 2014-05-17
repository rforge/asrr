prfPlot <- function(obj)  {
  if (class(obj) != "poLCA") stop("obj must be a poLCA object.")
  probs <- obj$probs
  vars <- names(probs)
  ret <- data.frame(row.names=row.names(probs[[1]]))
  for (i in vars) {
    probs_i <- probs[[i]]
    ncat <- ncol(probs_i)
    probs_i <- probs_i[,ncat, drop=FALSE]
    colnames(probs_i) <- paste(i, colnames(probs_i), sep="_")
    ret <- cbind(ret,probs_i)
  }
  x <- 1:ncol(ret)
  plot(x, ret[1,], type="l", xaxt="n", xlab="", ylab="", ylim=c(0,1), lty=1)
  for (i in 2:nrow(ret)) {
    lines(x, ret[i,], lty=i)
  }
  axis(1, x, colnames(ret))
}
