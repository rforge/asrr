\name{tsls2}
\alias{tsls2}
\title{Two-Stage Least Squares or Single-equation instrumental-variables regression}
\description{
  Fit a two-Stage Least Squares or Single-equation instrumental-variables regression.
}
%\usage{
%tsls2(formula, instruments, data, subset, na.action, contrasts = NULL, ...)
%}

\details{
 This functions is a modification of tsls in sem package. The modified
 tsls2 return the instrumental variables matrix, so can use \code{endog_test}
 to conduct test of endogeneity. The usage of \code{tsls2} is the same
 as \code{\link[sem]{tsls}}.
}
\value{
 the same as that of tsls.formula with the instrumental variables matrix(Z).
}

\author{The original one is by John Fox, modified by Ronggui HUANG}
\seealso{\code{\link[sem]{tsls}} , \code{\link{endog_test}}}
\examples{
## see endog_test for examples.
}

%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }
