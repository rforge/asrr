## use dim(arrage) <- NULL is better than as.vector(array)
## esubSet is much faster than subSet.

library(QCA3)
library(QCA)
data(mighty14)

Rprof(NULL)
Rprof()
reduce(mighty14,"OUT",LETTERS[1:11],remaind="include")
summaryRprof()

system.time(reduce(mighty14,"OUT",LETTERS[1:14],remaind="include"))
system.time(eqmcc(mighty14,"OUT",LETTERS[1:14],expl.1=T,incl.rem=T))

## test esubSet() 
im <- c(1,rep(NA,7))
nlevels <- c(2,3,3,2,2,2,3,2)
id <- QCA3:::implicant2Id(im,nlevels=nelvels)
system.time(x1 <- QCA3:::esubSet(im,inclu=F,nlevels=nlevels))
system.time(x2 <- QCA3:::subSet(im,inclu=F,nlevels=nlevels))
system.time(x3 <- QCA:::findSubsets(nlevels+1,id))
identical(x1,x3)
identical(x1,x2)

##superSet is slower by a factor of 2
#system.time(replicate(500,QCA3:::superSet(rep(1,4))))
#system.time(replicate(500,findPrimes(rep(2,4) + 1,81)))


esubSet <- function(implicant,include.itself=TRUE,nlevels=rep(2,length(implicant))){
  ##enhanced version of subSet()
  id <-  QCA3:::implicant2Id(implicant,nlevels=nlevels)
  idx1 <- which(is.na(implicant))
  idx2 <- idx1-1
  incr1 <- (nlevels[idx2] + 1) ^ idx2
  incr2 <- incr1*nlevels[idx1]
  incr2 <- c(0,incr2[1:length(incr2)-1])
  incr <- incr1 - cumsum(incr2)
  N <- prod(nlevels[which(is.na(im))]+1)
  ans <- vector(mode = "integer", length = N-1)
  idx3 <- 3^(0:length(idx1))
  for (i in 1:length(idx1)){
      ans[idx3[i]:(idx3[i+1]-1)] <- c(incr[i],ans[seq_len(idx3[i]-1)])
  }
 ans <- id + cumsum(ans)
 if (include.itself) ans <- c(id,ans)
 ans
}

$by.total
                      total.time total.pct self.time self.pct
reduce                     23.92      99.9      0.00      0.0
reduce.default             23.92      99.9      0.00      0.0
apply                      18.26      76.3      5.46     22.8
FUN                        11.88      49.6      3.58     15.0
as.vector                  11.22      46.9      0.02      0.1
solvePIChart               10.74      44.9      0.00      0.0
reduce1                    10.66      44.5      0.00      0.0
setdiff                    10.66      44.5      0.00      0.0
subSet                     10.56      44.1      0.00      0.0
colSums                     6.46      27.0      3.68     15.4
combn                       2.76      11.5      2.48     10.4
