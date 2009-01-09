\name{cs_truthTable}
\alias{cs_truthTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Construction of truthTable for csQCA and mvQCA}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cs_truthTable(mydata, outcome = "", conditions = c(""), cutoff1 = 1, cutoff0 = 1, cutoffc = 1, complete = FALSE, weight = NULL, show.cases = TRUE, cases = NULL, nlevels = rep(2, length(conditions)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{ ~~Describe \code{mydata} here~~ }
  \item{outcome}{ ~~Describe \code{outcome} here~~ }
  \item{conditions}{ ~~Describe \code{conditions} here~~ }
  \item{cutoff1}{ ~~Describe \code{cutoff1} here~~ }
  \item{cutoff0}{ ~~Describe \code{cutoff0} here~~ }
  \item{cutoffc}{ ~~Describe \code{cutoffc} here~~ }
  \item{complete}{ ~~Describe \code{complete} here~~ }
  \item{weight}{ ~~Describe \code{weight} here~~ }
  \item{show.cases}{ ~~Describe \code{show.cases} here~~ }
  \item{cases}{ ~~Describe \code{cases} here~~ }
  \item{nlevels}{ ~~Describe \code{nlevels} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mydata, outcome = "", conditions = c(""), cutoff1 = 1, 
    cutoff0 = 1, cutoffc = 1, complete = FALSE, weight = NULL, 
    show.cases = TRUE, cases = NULL, nlevels = rep(2, length(conditions))) 
{
    if (outcome == "" || conditions == "") 
        stop("You must specific outcome and conditions first.")
    fulldata <- mydata[, c(outcome, conditions)]
    if (any(fulldata \%in\% c(0, 1))) 
        stop("data value must in [0,1].")
    outcomeData <- mydata[, outcome]
    conditionsData <- mydata[, conditions]
    if (!is.null(weight)) 
        weight <- mydata[[weight]]
    else weight <- rep(1, nrow(mydata))
    getId <- function(implicant, nlevels) {
        IDX <- cumprod(nlevels)/nlevels
        ans <- sum(implicant * IDX) + 1
        ans
    }
    rowid <- apply(conditionsData, 1, getId, nlevels = nlevels)
    N_total <- sum(weight, na.rm = TRUE)
    cutoff1 <- ifelse(cutoff1 < 1, cutoff1 * N_total, cutoff1)
    cutoff0 <- ifelse(cutoff0 < 1, cutoff0 * N_total, cutoff0)
    cutoffc <- ifelse(cutoffc < 1, cutoffc * N_total, cutoffc)
    Positive <- tapply(outcomeData, rowid, FUN = function(each) all(each == 
        1) && (sum(each == 1) >= cutoff1))
    Pid <- names(Positive)[Positive]
    Negative <- tapply(outcomeData, rowid, FUN = function(each) all(each == 
        0) && (sum(each == 0) >= cutoff0))
    Nid <- names(Negative)[Negative]
    Contradictory <- tapply(outcomeData, rowid, FUN = function(each) {
        c1 <- (!all(each == 0)) && (!all(each == 1))
        c2 <- pmin(sum(each == 0), sum(each == 1)) >= cutoffc
        c <- c1 & c2
        c
    })
    Cid <- names(Negative)[Contradictory]
    Dontcareid <- as.character(setdiff(rowid, c(Pid, Nid, Cid)))
    if (complete) {
        exp <- sprintf("c(0:\%i)", nlevels - 1)
        allExpress <- eval(parse(text = sprintf("expand.grid(\%s)", 
            paste(conditions, "=", exp, sep = "", collapse = ","))))
    }
    else {
        WhichUnique <- match(sort(unique(rowid)), rowid)
        allExpress <- conditionsData[WhichUnique, ]
        rownames(allExpress) <- as.character(sort(unique(rowid)))
    }
    allExpress$OUT <- "?"
    allExpress$OUT[match(Pid, rownames(allExpress))] <- "1"
    allExpress$OUT[match(Nid, rownames(allExpress))] <- "0"
    allExpress$OUT[match(Cid, rownames(allExpress))] <- "C"
    allExpress$OUT[match(Dontcareid, rownames(allExpress))] <- "-"
    allExpress$NCase <- 0
    Ncase <- tapply(weight, rowid, sum)
    allExpress$NCase[match(names(Ncase), rownames(allExpress))] <- Ncase
    allExpress$freq0 <- allExpress$freq1 <- "-"
    Ncase1 <- by(cbind(weight, outcomeData), rowid, FUN = function(idx) sum(idx[, 
        1][idx[, 2] == 1]))
    allExpress$freq1[match(names(Ncase1), rownames(allExpress))] <- Ncase1
    Ncase0 <- by(cbind(weight, outcomeData), rowid, FUN = function(idx) sum(idx[, 
        1][idx[, 2] == 0]))
    allExpress$freq0[match(names(Ncase0), rownames(allExpress))] <- Ncase0
    if (show.cases) {
        if (is.null(cases)) 
            casesNames <- rownames(mydata)
        else casesNames <- mydata[, cases]
        casesNames <- gsub(",", "_", casesNames)
        casesNames <- tapply(casesNames, rowid, FUN = function(each) paste(each, 
            sep = "", collapse = ", "))
        allExpress$Cases <- ""
        allExpress$Cases[match(names(casesNames), rownames(allExpress))] <- casesNames
    }
    allExpress
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
