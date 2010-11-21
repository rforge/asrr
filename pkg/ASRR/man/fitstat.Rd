\name{fitstat}
\alias{fitstat}
\title{
  Measures of goodness of fit
}
\description{
  Various measures of goodness of fit described by Long (1997).
}
\usage{
fitstat(x, ...)
}
\arguments{
  \item{x}{
    A model object.
}
  \item{\dots}{
    Not used currently.
  }
}
\details{
  Curently, class of 'lm' is supported.
}
\value{
  A list with various components, each is a measure of goodness of it.
  \item{Deviance}{Deviance}
  \item{LL}{Log Likelihood}
  \item{LL0}{Log Likelihood of model with intercept only}
}
\references{
  Long (1997)
}
\author{
Ronggui HUANG
}
\seealso{
  \code{listcoef}
}
%\examples{}