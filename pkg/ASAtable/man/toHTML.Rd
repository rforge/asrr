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

\method{toHTML}{default}(x, y = NULL, z = NULL, file = paste(tempfile(), ".html", 
    sep = ""), caption = "Add Table Title Here!", note = NULL, 
    tablewidth = 600, autobrowse = TRUE, msword = FALSE, append = TRUE, 
    cgroup = NULL, n.cgroup = NULL, rgroup = NULL, i.rgroup = NULL, 
    stub.title = "&nbsp", colwidth = "prop", colname = NULL, 
    firstline = "double", lwidth = "1pt", lang = "zh", fontsize = 10, 
    indent = 1, asterisk = FALSE, sepwidth = 12, codepage = "UTF-8", 
    digits = 2, ...)

\method{toHTML}{modelList}(x, prefix = "Model", begin.numering = 1, group.name = names(x), 
    stub.col = rep(c("Coefficient", "S.E."), times = length(x)), 
    digits = 2, emptycell = "...", rgroup = NULL, relabel = NULL, 
    goffun = gof, Nfun = N, file = NULL, coeffun = function(x) summary(x)$coef, 
    asterisk = TRUE, coefL2Mfun = cbindCoef, append = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{For modelList method, x is usually a object of modelList. This is useful for
           reporting multiple models. For the default method, x is a matrix.}
  \item{\dots}{
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A HTML file.
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
       rgroup=list("Block A"=c("commerce","tradition"),"Block B"=c("midpeasant","inequality")), ## group of independent variables
       relabel=list(midpeasant="Strength of middle peasantry",inequality="Inequality of land tenure"), ## relabel variable names
       note="Note:\n1. This is a note.\n2. Models are OLS regression.", ## a note
       group.name=c("Group one","Group two"), ## group name, following prefix.
       )
## data frame to HTLM
toHTML(CO2[1:10,],cgroup=c("Col group one","Col group tow"),n.cgroup=c(2,3),stub.title="Stub title")
## add additional info: ncol(y)==length(cgroup) and ncol(z)==nrow(x)
toHTML(CO2[1:10,],cgroup=c("Col group one","Col group tow"),n.cgroup=c(2,3),stub.title="Stub title",y=matrix(c("a","b"),ncol=2),z=matrix(1:5,ncol=5))
## with row groups.
toHTML(CO2[1:10,],cgroup=c("group one","group tow"),n.cgroup=c(2,3),stub.title="Stub title",rgroup=list("Rgroup A"=NULL,"Rgroup B"=NULL),i.rgroup=c(0,4))
toHTML(CO2[1:10,],cgroup=c("group one","group tow"),n.cgroup=c(2,3),stub.title="Stub title",rgroup=list("Rgroup A"=c("A1",NA,NA,"A2",NA),"Rgroup B"=NULL),i.rgroup=c(1,3))
}
