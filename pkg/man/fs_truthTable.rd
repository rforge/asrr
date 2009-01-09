\name{fs_truthTable}
\alias{fs_truthTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Construction of truthTable from fuzzy set score}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fs_truthTable(mydata, outcome = "", conditions = c(""), ncases_cutoff = 1, consistency_cutoff = 0.8, complete = FALSE, show.cases = TRUE, quiet = FALSE, cases = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{ ~~Describe \code{mydata} here~~ }
  \item{outcome}{ ~~Describe \code{outcome} here~~ }
  \item{conditions}{ ~~Describe \code{conditions} here~~ }
  \item{ncases_cutoff}{ ~~Describe \code{ncases_cutoff} here~~ }
  \item{consistency_cutoff}{ ~~Describe \code{consistency_cutoff} here~~ }
  \item{complete}{ ~~Describe \code{complete} here~~ }
  \item{show.cases}{ ~~Describe \code{show.cases} here~~ }
  \item{quiet}{ ~~Describe \code{quiet} here~~ }
  \item{cases}{ ~~Describe \code{cases} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function (mydata, outcome = "", conditions = c(""), ncases_cutoff = 1, 
    consistency_cutoff = 0.8, complete = FALSE, show.cases = TRUE, 
    quiet = FALSE, cases = NULL, ...) 
{
    membership_cutoff = 0.5
    if (consistency_cutoff > 1 || consistency_cutoff < 0) 
        stop("consistency_cutoff should be in [0,1].")
    if (consistency_cutoff < 0.75) 
        warning("It is suggested that consistency_cutoff be >= 0.75.")
    if (outcome == "" || conditions == "") 
        stop("You must specific outcome and conditions first.")
    fulldata <- mydata[, c(outcome, conditions)]
    if (any(fulldata < 0) || any(fulldata > 1)) 
        stop("Fuzzy set score must in [0,1].")
    ncases_cutoff <- ifelse(ncases_cutoff < 1, ncases_cutoff * 
        nrow(fulldata), ncases_cutoff)
    allExpress <- eval(parse(text = (sprintf("expand.grid(\%s)", 
        paste(conditions, "=1:0", sep = "", collapse = ",")))))
    conditions <- mydata[, conditions]
    getScore <- function(index, data) {
        Negative <- which(index == 0)
        Positive <- which(index == 1)
        if (length(Negative) > 0 && length(Positive) > 0) {
            score <- pmin(apply(1 - data[, Negative, drop = FALSE], 
                1, min), apply(data[, Positive, drop = FALSE], 
                1, min))
        }
        else if (length(Negative) > 0 && length(Positive) == 
            0) {
            score <- apply(1 - data[, Negative, drop = FALSE], 
                1, min)
        }
        else if (length(Negative) == 0 && length(Positive) > 
            0) {
            score <- apply(data[, Positive, drop = FALSE], 1, 
                min)
        }
    }
    score_mat <- apply(allExpress, 1, function(x) getScore(x, 
        data = conditions))
    allExpress$NCase <- apply(score_mat, 2, function(x) sum(x > 
        membership_cutoff))
    allExpress$Consistency <- apply(score_mat, 2, function(x, 
        outcome) {
        sum(pmin(x, outcome))/sum(x)
    }, outcome = mydata[, outcome])
    allExpress$OUT <- "?"
    allExpress$OUT[allExpress$NCase >= ncases_cutoff & allExpress$Consistency > 
        consistency_cutoff] <- "1"
    allExpress$OUT[allExpress$NCase >= ncases_cutoff & allExpress$Consistency <= 
        consistency_cutoff] <- "0"
    allExpress$OUT[allExpress$NCase < ncases_cutoff & allExpress$NCase > 
        0] <- "-"
    allExpress$freq0 <- allExpress$freq1 <- 0
    allExpress$freq0[allExpress$OUT == "0"] <- allExpress$NCase[allExpress$OUT == 
        "0"]
    allExpress$freq1[allExpress$OUT == "1"] <- allExpress$NCase[allExpress$OUT == 
        "1"]
    allExpress <- allExpress[, c(seq_len(length(conditions)), 
        (length(conditions) + 3):(length(conditions) + 5), (length(conditions) + 
            1):(length(conditions) + 2))]
    if (show.cases) {
        if (is.null(cases)) 
            cases <- rownames(mydata)
        else cases <- mydata[, cases]
        cases <- gsub(",", "_", cases)
        allExpress$Cases <- apply(score_mat, 2, function(x) paste(cases[which(x > 
            membership_cutoff)], sep = "", collapse = ","))
    }
    if (!complete) 
        allExpress <- allExpress[allExpress$OUT != "?", , drop = FALSE]
    allExpress
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
