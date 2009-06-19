\name{toHTML}
\Rdversion{1.1}
\alias{toHTML}
\title{
Formats models to HTML according to American Journal of Sociology.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
toHTML(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\note{
This table is in HTML format, which can be pasted to MS word directly
from web browser. To get the best effect, MS IE is recommended to use to
open the HTML file.
}
\references{
Guidelines for Tables,\url{http://www.journals.uchicago.edu/page/ajs/msprep-tables.html},retrieved on 18,June,2009.
}
\author{
Ronggui HUANG
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## report mutiple models in one table
data(Chirot,package="car")
fm1 <- lm(formula = intensity ~ commerce + tradition, data = Chirot)
fm2 <- lm(formula = intensity ~ commerce + tradition + midpeasant + inequality, data = Chirot)
toHTML(modelList(fm1,fm2),
       caption="This is the title of table",## title of table
       goffun=function(x) paste("R<sup>2</sup> = ",format(summary(x)$r.squared,digit=3),collapse=""), ## goodness of fit
       rgroup=list("Block A"=c("commerce","tradition"),"Block B"=c("midpeasant","inequality")), ## group of independent variables
       relabel=list(midpeasant="Strength of middle peasantry",inequality="Inequality of land tenure"), ## relabel variable names
       note="Note:\n1. This is a note.\n2. Models are OLS regression.", ## a note
       group.name=c("Group one","Group two"), ## group name, following prefix.
       )
## data frame to HTLM
toHTML(CO2[1:10,],cgroup=c("Col group one","Col group tow"),n.cgroup=c(2,3),stub.title="Stub title")
## with row groups.
toHTML(CO2[1:10,],cgroup=c("group one","group tow"),n.cgroup=c(2,3),stub.title="Stub title",rgroup=c("Rgroup A","Rgroup B"),n.rgroup=c(0,4,6))
toHTML(CO2[1:10,],cgroup=c("group one","group tow"),n.cgroup=c(2,3),stub.title="Stub title",rgroup=c("Rgroup A","Rgroup B"),n.rgroup=c(1,3,6))
}
