\name{coefchange}
\alias{coefchange}
\alias{coefchange.default}
\alias{coefchange.lm}
\alias{coefchange.glm}
%\alias{coefchange_boot}
\alias{print.coefchange}
\title{Test of coef change}
\description{
Test if the coef will change significantly when additional variables are
included in the model
}
\usage{
coefchange(basemodel, ...)
\method{coefchange}{default}(basemodel, fullmodel,...)
\method{coefchange}{lm}(basemodel, fullmodel, overall=FALSE,boot=FALSE,R=500,whichcoef = names(coef(basemodel)),...)
\method{coefchange}{glm}(basemodel, fullmodel,boot=FALSE,R=500,whichcoef=names(coef(basemodel)),...)
%# Test of coef change by bootstrapping, for internal use
%coefchange_boot(basemodel,fullmodel,R=500,whichcoef = names(coef(basemodel)),...)
\method{print}{coefchange}(x,digits=3,...)
}

\arguments{
  \item{basemodel}{The baseline model.}
  \item{fullmodel}{The full model, which includes additional variables.}
  \item{overall}{Logic, when TRUE reports the test of H0: all the
  coef changes equal to zero. Only available when model is class
  lm. This argument has no effect when boot is TRUE.}
  \item{boot}{When TRUE, use bootstrapping to calculate the StdError of coef change.}
  \item{R}{The number of bootstrap replicates.}
  \item{x}{object return by coefchange. If boot=TRUE, the outcome is of
    class 'boot', and handled by functions from boot package directly.}
  \item{whichcoef}{A character vector to indicate which coef should be
    tested. Used only when boot is TRUE.}
  \item{digits}{a non-null value for 'digits' specifies the minimum
  number of significant digits to be printed in values.}
  \item{\ldots}{Other arguments passed to the method.}
}

\details{
Coef change is useful for testing mediation effects. For example, if X1
has effect on Y through X2,then in the model without X2, X1 should be
significant, however, X1's effect becomes insignificant or at least
attenuates. If it becomes insifnificant, it is easy to detect by inspect
the full model. Yet, it is not easy to when it just attenuates. In this
case, test of coef change is a way to test the theoretical statement of
mediation effect.
}
\value{
  When boot is TRUE, the value return directly from function boot.

  When boot is FALSE, the return values is a list of three components.
  \item{coefchange}{A data frame of four variables, the coef
  changes(CoefChange), the Variance of CoefChange(Var), the statistics(T
  for lm model and Z for glm model), and the p value(p.value).}
  \item{overall}{For lm, this is the result of the overall test. For
    glm, it is always NULL. See Appendix of Clogg et.al.(1995) for more information.}
  \item{variables}{The names of additional variables.}
}
\references{
Clogg, C. C., Petkova, E., and Haritou, A. 1995. Statistical methods
for comparing regression coefficients between models. American Journal
of Sociology, 100, 1261-1293.

Allison, Paul D.1995. The impact of random predictors on comparisons of
coefficients between models. American Journal of Sociology
100:1294-1305.

DeMaris, A. 2004. "Regression With Social Data: Modeling Continuous and
Limited Response Variables." John Wiley \& Sons: pp96-98(for lm) and
pp291-293(for glm).
}

\author{Ronggui HUANG}

\note{
  The method proposed by Clogg et. al.(1995) has been criticized by
  Allison(1995).The standard errors of the coef changes tend to be
  underestimated due to the dependence of basemodel and full model
  (DeMaris 2004:293). Thus,when boot is FALSE, the result should be
  taken as exploratory, and should report the result from bootstrapping
  method whenever possible.
}
%\seealso{}
\examples{
\dontrun{
fm1 <- lm(EXAM1~SCORE,data=student)
fm2 <- lm(EXAM1~SCORE+COLGPA,data=student)
fm3 <- lm(EXAM1~SCORE+COLGPA+HOURS+PREVMATH,data=student)
coefchange(fm1,fm2)
# with overall test
coefchange(fm1,fm2,overall=TRUE)
# use bootstrapping method
coefchange(fm1,fm2,boot=TRUE)
# only interested in that of SCORE
print(ans <- coefchange(fm1,fm2,boot=TRUE,whichcoef="SCORE"))
# get the CI of it
boot.ci(ans)
}
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }
