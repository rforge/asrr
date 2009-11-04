\name{coverage}
\alias{coverage}
\title{
Coverage of x by y.
}
\description{
  Given two fuzzy set score variables x and y, the function returns the coverage
  of x by y, given their relationship.
}
\usage{
coverage(x, y, alternative = c("less", "greater"))
}
\arguments{
  \item{x}{
    a numeric vector of fuzzy set score.
}
\item{y}{
  a numeric vector of fuzzy set score.
}
  \item{alternative}{
    specify the relationship between x and y.
}
}
\details{
  When alternative is "less", it returns coverage(x<=y). This is
  coverage for sufficent test. When "greater", it returns
  coverage(x>=y), it is coverage for necessary condition.

  This value can be used to evaluate the importance or relevance of x as
  a necessary condition for Y. When coverage(x>=y) is small, the
  constraining effect of x on y is negligible. In other words, small
  value of coverage(x>=y) indicates en empirically trivial necessary
  condition (Ragin, 2008: 61).
}
\value{
  a length-1 numeric vector.
}
\references{
Ragin, C. 2008, Redesigning social inquiry: fuzzy set and
  beyond. Chicago: The University of Chicago Press.
}
\author{
  Ronggui HUANG
}
%\seealso{}
\examples{
## evaluate the congerage of necessary conditions for "no democratic consolidation"
## see help(CoD) for more information.
## coverage for necessary test for not-CoD (Schneider, 2009:77)

coverage(1-CoD$econdev,1-CoD$cod,'greater')
# not socioeconomically developed
coverage(1-CoD$demex,1-CoD$cod,'greater')
# no previous democratic experience
coverage(pmin(1-CoD$demex,1-CoD$econdev),1-CoD$cod,'greater')
# not socioeconomic developed society without prior democratic experience
}

