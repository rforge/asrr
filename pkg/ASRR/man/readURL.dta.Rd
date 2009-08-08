\name{readURL.dta}
\alias{readURL.dta}
\title{Read Stata data from URL}
\description{
Read Stata data from the Internet.
}
\usage{
readURL.dta(url)
}
\arguments{
  \item{url}{One length character of the url of Stata data.}
}
\details{
This function creat a temporary directory, download the dataset by
download.file, then read the dataset by foreign:::read.dta. The
tmporary file will be cleaned when on exit.

The newest version of \code{read.dta} can read dta file from the 
Internet, which makes this function unncessary since R 2.9.1.
}
\value{
  A data frame.
}
\author{ Ronggui HUANG}
\seealso{\code{\link[foreign]{read.dta}}}
\examples{
\dontrun{
mroz <- readURL.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta")
}
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
