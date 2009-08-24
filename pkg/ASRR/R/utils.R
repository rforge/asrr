renames <- function(x,...){
	UseMethod("renames")
	}
	
renames.data.frame <- function(x, from, to, info=TRUE, envir=parent.frame(n=1)) {	
	if (length(from)!=length(to)) stop("'from' and 'to' must be character vector of the same length.")
	xname <- deparse(substitute(x))
	currentNames <- names(x)
	idx <- match(from, currentNames)
	if (any(is.na(idx))) {
		naidx <- which(is.na(idx))
		msg <- sprintf("%s doesn't have name(s) of %s",shQuote(xname), paste(shQuote(from[naidx]),collapse=","))
		stop(msg)
		} else {	
	names(x)[idx] <- to
	assign(xname,x,envir=envir)
	if (info) {
		cat(paste(from,to,sep="->",collapse="; "),"\n")
		}
	invisible()
	}
	}


gkTau<-function(x){
  ##Goodman and Kruskal's tau.
  ##Measures of Proportional Reduction in Predictive Error.
  ##the variables are not interchangable, one is dependent variable.
  ##This is tau(y|x),y is the colum variable,x is the row variable.
  ## p value is based on approx.chi-square distribution.
  ##reference:Algorithms of SPSS----crosstable.
  ##x, a matrix of a r x c table.
tau<- function(x){
  if (!is.matrix(x))
    stop("Function only defined for 2-d - table.")
  w <- sum(x)
  cj <- colSums(x)
  ri <- rowSums(x)
  cj2s <- sum(cj^2)
  cjsqs <- sum(apply(x,1,function(x) x^2/sum(x)))
  delta <- w^2 - cj2s
  ny <- w * cjsqs - cj2s
  tau <- ny/delta
  ##browser()
  ASE <- sqrt(sum(apply(x,1,function(f){
    f*(((w*f^2/sum(f)-cj2s)-delta)*(1/sum(f)*f*cj-cj)-w*delta*(1/sum(f)^2*f^2-1/sum(f)*f))^2
  })*4/delta^4))
  nc <- ncol(x)
  nr <- nrow(x)
  sta <-(w-1)*(nc-1)*tau
  df <- (nc-1)*(nr-1)
  pvalue <- 1-pchisq(sta,df)
  nam <- names(dimnames(x))
  if (is.null(nam)) nam <- c("Row","Col") else if (any(nam=="")) {
	nam[nam==""] <- c("Row","Col")[nam==""]
	}
  cond <- sprintf("(%s|%s)",nam[2],nam[1])
  cat(sprintf("Goodman and Kruskal's tau %s is %f \n",cond,tau))
  cat(sprintf("Approx. Sig. is %s and ASE is %s\n\n",pvalue,ASE))
  invisible(ans <- list(tau=tau,p.value=pvalue,condition=cond))
}
invisible(ans <- list(CR=tau(x),RC=tau(t(x))))
}
#example
#require(vcd)
#data(Arthritis)
#tab <- xtabs(~Improved + Treatment, data = Arthritis)
#tau(tab)
x2<-matrix(c(14,6,9,11),2,byrow=T)
gkTau(x2) ## ASE=0.55
