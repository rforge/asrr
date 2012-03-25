\name{McCammonVanDyke}
\alias{McCammonVanDyke}
\docType{data}
\title{
Social movement coalition studies
}
\description{
Data set in McCammon and Van Dyke's meta-analysis of studies on social movement coalition
}
\usage{data(McCammonVanDyke)}
\format{
  A data frame with 24 observations on the following 6 variables.
  \describe{
    \item{\code{ideology}}{ideological alignment}
    \item{\code{threats}}{political threats}
    \item{\code{opportunity}}{political opportunities}
    \item{\code{ties}}{prior social ties between social movement organisations}
    \item{\code{resources}}{plentiful resources}
    \item{\code{coalition}}{outcome of coalition formation}
  }
}
%\details{}
\source{
Manually input from table 13.1 of the reference.
}
\references{
McCammon & Van Dyke. 2010. Applying Qualitative Comparative Analysis to Empirical Studies of Social Movement Coalition Formation. in Van Dyke, N. & McCammon, H. J. (ed.) Strategic alliances: coalition building and social movements. Minneapolis : University of Minnesota Press.
}
\examples{
data(McCammonVanDyke)
workdat <- McCammonVanDyke
workdat[workdat==-9] <- 0
reduce(workdat,"coalition",c("ideology","threats","opportunity","ties","resources"))
## result in figure 13.2

workdat <- McCammonVanDyke
idx <- apply(workdat, 1, function(x) any(x==-9))
ans <- reduce(workdat[!idx,],"coalition",c("ideology","threats","opportunity","ties","resources"))
constrReduce(ans,include=workdat[idx,1:5])
## result in figure 13.3
}
\keyword{datasets}
