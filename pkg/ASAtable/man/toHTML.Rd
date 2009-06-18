\name{toHTML}
\Rdversion{1.1}
\alias{toHTML}
\title{
Formats models to HTML according to American Journal of Sociology, which can be pasted to MS word directly from MS IE.
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
To get the best effect, please use IE to open the html file rather than firefox etc.
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
\dontrun{
data(Chirot,package="car")
fm1 <- lm(formula = intensity ~ commerce + tradition, data = Chirot)
fm2 <- lm(formula = intensity ~ commerce + tradition + midpeasant +
inequality, data = Chirot)
toHTML(modelList(fm1,fm2))
toHTML(modelList(fm1,fm2),goffun=function(x) paste("R<sup>2</sup> = ",format(summary(x)$r.sq,digit=3),collapse="")) ## add goodness of fit
toHTML(modelList(fm1,fm2),relabel=list(midpeasant="Strength of middle peasantry")) ## relabel variable name
}}
