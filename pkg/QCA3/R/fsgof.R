## Programme written by Ronggui HUANG (2010)
## Goodness-of-Fit Tests and Descriptive Measures in Fuzzy-Set Analysis
## Eliason S. & Stryker R. 2009. Sociological Methods & Research 38:102-146. 

zTransform <- function(x,damp=.01){
      x2 <- x
      x2[x < damp] <- damp
      x2[x > (1-damp)] <- 1-damp
	zx <- qnorm(x2)
	zx
}

Dnull <- function(y, damp=.01){
	yZ <- zTransform(y,damp=damp)
	df <- length(y) - 1
	ssd <- sum((yZ -mean(yZ))^2)
      msd <- ssd/df
	ans <- list(ssd=ssd,df=df,msd=msd)
	ans
}

Dnec <- function(y, x, damp=.01, error=.05) {
	yZ <- zTransform(y,damp=damp)
	xZ <- zTransform(x,damp=damp)
	d <- as.numeric(y > x)
	ssd <- sum(d*(yZ - xZ)^2)
	df <- sum(y > x)
	msd <- ssd/df
	emsd <- error^2*4
	ans <- list(ssd=ssd,df=df,msd=msd)
	ans
}

Dsuff <- function(y, x, damp=.01, error=.05) {
	xZ <- zTransform(x,damp=damp)
	yZ <- zTransform(y,damp=damp)
	d <- as.numeric(y > x)
	ssd <- sum((1-d)*(yZ - xZ)^2)
	df <- sum(y < x)
	msd <- ssd/df
	emsd <- error^2*4
	F <- msd/emsd
	ans <- list(ssd=ssd,df=df,msd=msd, emsd=emsd,F=F)
	ans
}

Dsuffnec <- function(y, x, damp=.01, error=.05) {
	xZ <- zTransform(x,damp=damp)
	yZ <- zTransform(y,damp=damp)
	d <- as.numeric(y > x)
	ssd <- sum(d*(yZ - xZ)^2) + sum((1-d)*(yZ - xZ)^2)
	df <- length(y)
	msd <- ssd/df
	emsd <- error^2*4
	ans <- list(ssd=ssd,df=df,msd=msd)
	ans
}

##y=c(0.8,0.5,0.5,0.4)
##x=c(0.7,0.3,0.4,0.45)
