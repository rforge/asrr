\name{listcoef}
\Rdversion{1.1}
\alias{listcoef}

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
Currently, it supports linear regression model, logistic model, probit model and poisson model. When the model is estimated, argument x and y must be set to TRUE.

Definition of notation is as follow:
SDofX is the standard deviation of X.
SDofY is the standard deviation of Y.
}
\value{
A data frame containing relevant components.}
\references{
Long, J. S.2001. Regression Models for Categorical Dependent Variables Using Stata Stata Press.
}
\author{
HUANG Ronggui
}
\note{
The method of weighted covariance is 'ML', and the result is slightly different from that of Stata's summarize command.
}
%\seealso{}
\examples{
data(Chirot,package="car")
lm1 <- lm(intensity~inequality+commerce,data=Chirot,x=T,y=T)
listcoef(lm1)
}
