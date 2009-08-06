\name{weighted.summary}
\Rdversion{1.1}
\alias{weighted.summary}
\title{
  Weigted summary of a variable.
}
\description{
Return the weighted summary statistics of a variable,including mean,
variance, skewness and kurtosis.
}
\usage{
weighted.summary(x, wt = rep(1, length(x)), type = "analytic")
}
\arguments{
  \item{x}{
    a numeric vector.
  }
  \item{wt}{
    a numeric vector which is the analysitc weight.
}
  \item{type}{
    type of weight. Currently only support analytics weight.
  }
}
\details{This function use the same formulas as that of Stata function summarzie.}
\value{
A list of 4 components
\item{mean}{weighted mean.}
\item{variance}{weighted variance.}
\item{sd}{weighted standard deviation.}
\item{kewness}{weighted kewness.}
\item{kurtosis}{weighted kurtosis.}
}
\references{
StataCorp.,2007, Stata Base Reference Manual, Release 10. Vol. 3, Stata
Press: College Station, Tex.
}
\author{
HUANG Ronggui
}
%\examples{}
