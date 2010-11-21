\name{listcoef}
\Rdversion{1.1}
\alias{listcoef}
\alias{listcoef.lm}
\alias{listcoef.glm}
\title{
List transformed regression coefficients.
}
\description{
Transformed regression coefficients to make interpretation easier.
}
\usage{
listcoef(x, ...)
\method{listcoef}{lm}(x,...)
\method{listcoef}{glm}(x,...)
}
\arguments{
  \item{x}{a model object.}
  \item{\dots}{Not used currently.}
}
\details{
  Currently, it supports linear regression model, logistic model, probit
  model and poisson model. It is recommended to set argument x and y to
  TRUE when the model is estimated. \code{listcoef} needs model matrix
  and response to calculate various statistics.
  
  Definition of notation is as follow: (1) SDofX is the standard deviation
  of X. (2)SDofY is the standard deviation of Y.
}
\value{
  A data frame containing relevant components.
  \item{bStdX}{x-standardized coefficient.}
  \item{bStdY}{y-standardized coefficient.}
  \item{bStdXY}{fully standardized coefficient.}
  \item{SDofX}{standard deviation of X.}
  \item{SDofY}{standard deviation of Y.}
}
\references{
Long, J. S.2001. Regression Models for Categorical Dependent Variables Using Stata Stata Press.
}
\author{
HUANG Ronggui
}
\examples{
listcoef(lm(income~east+talent+allowance+time4Public,data=FuWeiHuang2007))

listcoef(glm(rich~talent+WorkersInCity+east+allowance,
             data=FuWeiHuang2007,fam=binomial))
}
