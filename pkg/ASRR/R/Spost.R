listcoef <- function(x,...) {
  UseMethod("listcoef")
  }

listcoef.lm <- function(x,...){
	##www.nd.edu/~rwilliam/stats1/x92b.pdf
	## x is a lm model with x and y setted to TRUE.
	SDofX <- apply(x$x,2,sd)
	SDofY <- sd(x$y)
	Coef <- summary(x)$coef
	Est <- Coef[,"Estimate"]
	bStdY <- Est/SDofY
	bStdX <- Est*SDofX
	bStdXY <- Est*SDofX/SDofY
	ans <- cbind(Coef,bStdX=bStdX,bStdY=bStdY,bStdXY=bStdXY,SDofX=SDofX,SDofY=SDofY)
	ans
	}
	
listcoef.glm <- function(x,...){	
	listcoef_logistic <- function(x,...){
		## logistic model
		## http://www.nd.edu/~rwilliam/xsoc73994/L03.pdf
		## http://www.nd.edu/~rwilliam/zsoc694/x04.pdf
		Xb <- predict(x)
		SDofX <- apply(x$x,2,sd)
		SDofY <- sd(x$y)
		SDofYstar <- sqrt(var(Xb)+pi^2/3)
		Coef <- summary(x)$coef
		Est <- Coef[,"Estimate"]
		bStdY <- Est/SDofYstar
		bStdX <- Est*SDofX
		bStdXY <- Est*SDofX/SDofYstar
		ans <- cbind(Coef,bStdX=bStdX,bStdY=bStdY,bStdXY=bStdXY,SDofX=SDofX,SDofY=SDofY,"SDofY*"=SDofYstar)
		ans
		}
	ans <- NULL
	if (x$family$family == "binomial" && x$family$link == "logit") { ans <- listcoef_logistic(x) }
	## if ...
	ans
	}

