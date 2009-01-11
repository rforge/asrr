\name{cs_truthTable}
\alias{cs_truthTable}
\alias{mv_truthTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Construct a truthTable for csQCA or mvQCA}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cs_truthTable(mydata, outcome, conditions, method = c("deterministic", "probabilistic"), complete = FALSE, weight = NULL, show.cases = TRUE, cases = NULL, nlevels = rep(2, length(conditions)), cutoff1 = 1, cutoff0 = 1, benchmark = 0.65, conf.level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{ ~~Describe \code{mydata} here~~ }
  \item{outcome}{ ~~Describe \code{outcome} here~~ }
  \item{conditions}{ ~~Describe \code{conditions} here~~ }
  \item{method}{ ~~Describe \code{method} here~~ }
  \item{complete}{ ~~Describe \code{complete} here~~ }
  \item{weight}{ ~~Describe \code{weight} here~~ }
  \item{show.cases}{ ~~Describe \code{show.cases} here~~ }
  \item{cases}{ ~~Describe \code{cases} here~~ }
  \item{nlevels}{ ~~Describe \code{nlevels} here~~ }
  \item{cutoff1}{ ~~Describe \code{cutoff1} here~~ }
  \item{cutoff0}{ ~~Describe \code{cutoff0} here~~ }
  \item{benchmark}{ ~~Describe \code{benchmark} here~~ }
  \item{conf.level}{ ~~Describe \code{conf.level} here~~ }
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
function (mydata, outcome, conditions, method = c("deterministic", 
    "probabilistic"), complete = FALSE, weight = NULL, show.cases = TRUE, 
    cases = NULL, nlevels = rep(2, length(conditions)), cutoff1 = 1, 
    cutoff0 = 1, benchmark = 0.65, conf.level = 0.95) 
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
    method <- match.arg(method)
    getId <- function(implicant, nlevels) {
        IDX <- cumprod(nlevels)/nlevels
        ans <- sum(implicant * IDX) + 1
        ans
    }
    rowid <- apply(conditionsData, 1, getId, nlevels = nlevels)
    N_total <- sum(weight, na.rm = TRUE)
    Positive <- tapply(outcomeData, rowid, FUN = function(each) all(each == 
        1))
    Pid <- names(Positive)[Positive]
    Negative <- tapply(outcomeData, rowid, FUN = function(each) all(each == 
        0))
    Nid <- names(Negative)[Negative]
    Contradictory <- tapply(outcomeData, rowid, FUN = function(each) {
        c1 <- (!all(each == 0)) && (!all(each == 1))
        c1
    })
    Cid <- names(Negative)[Contradictory]
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
    allExpress$NCase <- 0
    Ncase <- tapply(weight, rowid, sum)
    allExpress$NCase[match(names(Ncase), rownames(allExpress))] <- Ncase
    allExpress$freq0 <- allExpress$freq1 <- 0
    Ncase1 <- by(cbind(weight, outcomeData), rowid, FUN = function(idx) sum(idx[, 
        1][idx[, 2] == 1]))
    allExpress$freq1[match(names(Ncase1), rownames(allExpress))] <- Ncase1
    Ncase0 <- by(cbind(weight, outcomeData), rowid, FUN = function(idx) sum(idx[, 
        1][idx[, 2] == 0]))
    allExpress$freq0[match(names(Ncase0), rownames(allExpress))] <- Ncase0
    allExpress$OUT <- "?"
    if (method == "deterministic") {
        cutoff1 <- ifelse(cutoff1 < 1, cutoff1 * N_total, cutoff1)
        cutoff0 <- ifelse(cutoff0 < 1, cutoff0 * N_total, cutoff0)
        pidx <- intersect(match(Pid, rownames(allExpress)), which(allExpress$freq1 >= 
            cutoff1))
        allExpress$OUT[pidx] <- "1"
        nidx <- intersect(match(Nid, rownames(allExpress)), which(allExpress$freq0 >= 
            cutoff0))
        allExpress$OUT[nidx] <- "0"
        cidx1 <- intersect(match(Cid, rownames(allExpress)), 
            which(allExpress$freq1 >= cutoff1))
        cidx0 <- intersect(match(Cid, rownames(allExpress)), 
            which(allExpress$freq0 >= cutoff0))
        cidx <- intersect(cidx1, cidx0)
        allExpress$OUT[cidx] <- "C"
        Dontcare1 <- intersect(match(Cid, rownames(allExpress)), 
            which(allExpress$freq1 < cutoff1))
        Dontcare0 <- intersect(match(Cid, rownames(allExpress)), 
            which(allExpress$freq0 < cutoff0))
        Dontcareid <- intersect(Dontcare1, Dontcare0)
        allExpress$OUT[Dontcareid] <- "-"
        allExpress$OUT[intersect(cidx1, Dontcare0)] <- "1"
        allExpress$OUT[intersect(cidx0, Dontcare1)] <- "0"
    }
    if (method == "probabilistic") {
        limit1 <- lowerLimite(allExpress$freq1, allExpress$NCase, 
            conf.level)
        limit0 <- lowerLimite(allExpress$freq0, allExpress$NCase, 
            conf.level)
        pidx <- intersect(which(limit1 >= benchmark), match(c(Pid, 
            Cid), rownames(allExpress)))
        nidx <- intersect(which(limit0 >= benchmark), match(c(Nid, 
            Cid), rownames(allExpress)))
        Dontcareid <- setdiff(match(c(Nid, Cid, Pid), rownames(allExpress)), 
            c(pidx, nidx))
        allExpress$OUT[pidx] <- "1"
        allExpress$OUT[nidx] <- "0"
        allExpress$OUT[Dontcareid] <- "-"
    }
    if (show.cases) {
        if (is.null(cases)) 
            casesNames <- rownames(mydata)
        else casesNames <- mydata[, cases]
        casesNames <- gsub(",", "_", casesNames)
        casesNames[outcomeData == 0] <- paste("[", casesNames[outcomeData == 
            0], "]", sep = "")
        casesNames <- tapply(casesNames, rowid, FUN = function(each) paste(each, 
            sep = "", collapse = ", "))
        allExpress$Cases <- ""
        allExpress$Cases[match(names(casesNames), rownames(allExpress))] <- casesNames
        allExpress$Cases[allExpress$OUT != "C"] <- gsub("\\[|\\]", 
            "", allExpress$Cases[allExpress$OUT != "C"])
    }
    allExpress
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
