\name{consistGap}
\alias{consistGap}
\title{
Consistency gaps between configurations
}
\description{
Show the consistency gaps between configurations
}
\usage{
consistGap(x)
}
\arguments{
  \item{x}{
 A fuzzy set truthTable.}
}
\details{
Sometimes, QCA users should not mechanically use a pre-determined consistency cut-off. One possible way to determine such a cut-off is to examine the biggest consistency gap between configurations and place a cutting point between the two configuations.

The truthTable is sorted according to consistency and outcome first before calculating the consistency gap.
}
\value{
A data frame with 6 columns, with ConsistGap the most important variable.}
\author{
Ronggui HUANG}
\seealso{
\link{setOUT}, link{fs_truthTable}
}
\examples{
fst <- fs_truthTable(Lipset_fs,"Survived.FZ",c("Developed.FZ","Urban.FZ","Literate.FZ","Industrial.FZ",
     "Stable.FZ"),cases="Country",consistency_cutoff=0.7)

consistGap(fst)
}