\name{Student}
\alias{student}
\docType{data}
\title{Students dataset}
\description{
The student dataset from DeMaris (2004).
}
\usage{data(student)}
\format{
  A data frame with 235 observations on the following 17 variables.
  \describe{
    \item{\code{EXAM1}}{Score on the first exam.}
    \item{\code{EXAM2}}{Score on the second exam.}
    \item{\code{FIRSTEX}}{Ordinal version of EXAM1.}
    \item{\code{ORDEXAM1}}{Onother ordinal version of EXAM1.}
    \item{\code{CLASSIF}}{Student classification.}
    \item{\code{COLGPA}}{Current college GPA.}
    \item{\code{HOURS}}{Number of class hours student is taking in current semester }
    \item{\code{MALE}}{Dummy.}
    \item{\code{PREVMATH}}{Number of previous college-level math courses taken.}
    \item{\code{RATIO}}{Defined as STUDYHRS/(TVHRS + .5)}
    \item{\code{RMAJOR}}{Student's major (1 = sociology, 2 = other social science, 3 = other field)}
    \item{\code{SOCSCI}}{Dummy: 1 = other social science major, 0 = other response.}
    \item{\code{OTHMAJ}}{Dummy: 1 = other field major, 0 = other response.}
    \item{\code{SCORE}}{Score on math diagnostic.}
    \item{\code{STATMOOD}}{Attitude toward statistics:+16 = most favorable.}
    \item{\code{STUDYHRS}}{Number of hours student studies in a typical week.}
    \item{\code{TVHRS}}{Number of hours student watches TV in a typical week.}
  }
}
\details{
"This is a sample of 235 students taking introductory statistics at Bowling Green State University (BGSU) from the author
between the years 1990 and 1999. Variables include student
characteristics collected in the first class session as well as the
scores on the first two exams, given, respectively,in the sixth and
tenth weeks of the course."(DeMaris 2004:14-15)
}
\source{
\url{ftp://ftp.wiley.com/public/sci_tech_med/regression_social_data/}
}
\references{
DeMaris, A. 2004. "Regression With Social Data: Modeling Continuous and
Limited Response Variables." John Wiley & Sons.p88, Table 3.1.
}
\examples{
\dontrun{
data(student)
}
## see 'coefchange' for more examples
}
\keyword{datasets}
