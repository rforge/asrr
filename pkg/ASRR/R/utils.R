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

mcor.test <- function (x,...)
{
    ## x is a data.frame
    p <- ncol(x)
    Means <- mean(x,na.rm=TRUE)
    index <- t(combn(p, 2))
    out <- apply(index,1,FUN=function(idx){ans=cor.test(x[,idx[1]],x[,idx[2]]);c(ans$estimate,ans$p.value)})
    ans <- cbind(index,t(out))
    colnames(ans) <- c("idx1","idx2","estimate","p.value")
    vars <- names(x)
    ans <- list(cormat=ans,variables=vars,Means=Means)
    class(ans) <- "mcor.test"
    ans
}

