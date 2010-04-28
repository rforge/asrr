\name{QCA.methods}
\alias{print.QCA}
\alias{summary.QCA}
\alias{[.QCA}
\title{Methods for "QCA" an object}
\description{
   Various methods for object from \code{\link{reduce}}
}
\usage{
\method{print}{QCA}(x, traditional = TRUE, show.truthTable = TRUE, ...)

\method{summary}{QCA}(object, traditional = TRUE, show.case = TRUE, ...)

\method{[}{QCA}(object, which)
}
\arguments{
   \item{x}{an object of class 'QCA', which is usually returned from
    \code{reduce}.}
  \item{traditional}{logical, use traditional symbol when it is
    TRUE. Otherwise, use Tosmana-style symbol.}
  \item{show.truthTable}{logical, show truthTable when it is TRUE. Of
    course, it has effect only when the 'keepTruthTable' argument of
    \code{reduce} is set to TRUE.}
  \item{object}{an object of class 'QCA', which is usually returned from
    \code{reduce}.}
  \item{show.case}{logical, show case names when it is TRUE.}
  \item{which}{numeric vector, indices specifying elements to
extract. Extraction of a solution or (prime implicant) is essentially a
extraction on a list. you can refer to \code{[} for more details.}
  \item{\dots}{ For \code{print.QCA} and \code{summary.QCA}, currently not
    used.}
}
\details{
  The traditional way uses upper-case letters representing 1 and and
  lower-case letters reprensenting 0. The Tosmana-style uses
  \code{condition{value}} to represent the prime implicants.
}
\value{
 print method does not return any value.

 summary method returns an object of class "summary.QCA".

 The index method returns an object of class "QCA".
}

\author{ Ronggui HUANG}

\seealso{
\code{\link{reduce}} and \code{\link{constrReduce}}
}

