\name{endog_test}
\alias{endog_test}
\alias{endog_test.tsls2}
\alias{endog_test.ivreg}
\alias{print.endog_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test of endogeneity.}
\description{
Test of endogeneity for single equation instrumental variable regression
(two-stage regression).
}
\usage{
endog_test(object, ...)
%# method for tsls2
\method{endog_test}{tsls2}(object, robust = FALSE, type = c("hc2", "hc0", "hc1",
"hc3", "hc4"),...)
%# method for ivreg
\method{endog_test}{ivreg}(object, robust = FALSE, type = c("hc2", "hc0", "hc1",
"hc3", "hc4"),...)
%# print method for endog_test
\method{print}{endog_test}(x, ...)
}

\arguments{
  \item{object}{The model single equation instrumental variables 
    regression (two-stage regression) model.}
  \item{robust}{ Logic, use robust method when TRUE.}
  \item{type}{type of robust method. The details can be found in hccm in car package.}
  \item{x}{The result from endog_test.}
  \item{\dots}{Not used for the time being. }
}
\details{
The method for class tsls2 is used after the model is fitted by tsls2. The details about the caculation can be found in
  Wooldridge (2002:118-120). When the test is significant, we reject the
  claim of exdogeneity, in other words, we can not rule out the problem
  of endogeneity, then we should use two-stage estimation.
}
\value{
  \item{F}{The F statistics.}
  \item{Df}{Degree of freedom.}
  \item{P}{The probility.}
  \item{Endo.v}{The name of endogenous variable(s).}
}
\references{
Wooldridge,J.M. 2002. Econometric Analysis of Cross Section and Panel
  Data. Cambridge,MA:MIT Press., pp118-121.
}
\author{Ronggui HUANG}
\seealso{\code{\link[car]{hccm}} \code{\link[car]{lht}}}

\examples{
#example 6.1 from Wooldridge,J.M.2002.
\dontrun{
mroz <- readURL.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta")
fm6.1 <- tsls2(lwage~exper+I(exper^2)+educ,~exper+I(exper^2)+motheduc+fatheduc+huseduc,data=mroz)
endog_test(fm6.1)
endog_test(fm6.1,T)
}

#example 6.2 from Wooldridge,J.M.2002.
\dontrun{
 card <- readURL.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/card.dta")
 fm6.2 <-tsls2(lwage~black*educ+exper+expersq+smsa+smsa66+south+reg661+reg662+reg663+reg664+reg665+reg666+reg667+reg668,
 ~nearc4*black+exper+expersq+smsa+smsa66+south+reg661+reg662+reg663+reg664+reg665+reg666+reg667+reg668,
 data=card)
endog_test(fm6.2)
endog_test(fm6.2,T)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
