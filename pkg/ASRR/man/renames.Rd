\name{renames}
\alias{renames}
\title{
renames the variable names of a data frame.
}
\description{
	Rename the variable names of a data frame.
}
\usage{
renames(x, ...)
\method{renames}{data.frame}(x, from, to, info=TRUE, envir=parent.frame(n=1),...)
}
\arguments{
  \item{x}{
	a data frame.
	}
  \item{from}{
	a character vector of variable names from data frame x.
	}
  \item{to}{
	a character vector of new variable names. It should be the same length of from and not duplicative with any names from data frame x.
	}
  \item{info}{
	when TRUE, print information on the correspondence of old (from) and new (to) names.
	}
  \item{envir}{
	the environment which the renamed new data frame is assigned to.
	}
  \item{\dots}{
	not used currently.
}
}
\details{
At the top level, a subset of names (from) of the data frame x are renamed to a new set of names (to); then the renamed data frame is assigned back to envir. 
It means this function assign the renamed data frame to an object with its own old name, and users do not have to assign it explicitly.
}
\value{
	It is used for the side effect. No value is returned.
}
\author{
Ronggui HUANG
}
%\seealso{}
\examples{
	Huang <- HuangGui2009
	names(Huang)
	renames(Huang,"BBS","Forum")
	names(Huang)
}
