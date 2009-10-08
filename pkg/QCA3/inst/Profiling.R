## use dim(arrage) <- NULL is better than as.vector(array)
## esubSet is much faster than subSet, by a factor of 20.

library(QCA3)
library(QCA)
data(mighty14)

Rprof(NULL)
Rprof()
reduce(mighty14,"OUT",LETTERS[1:11],remaind="include")
summaryRprof()

system.time(reduce(mighty14,"OUT",LETTERS[1:12],remaind="include"))
system.time(eqmcc(mighty14,"OUT",LETTERS[1:12],expl.1=T,incl.rem=T))

## test esubSet() 
im <- c(NA,NA,1,NA,1,NA,1,1,NA,NA,2,1)
nlevels <- c(2,3,3,2,2,2,2,3,3,4,3,2)
x1 <- QCA3:::esubSet(im,F,nlevels=nlevels)
x2 <- QCA3:::subSet(im,F,nlevels=nlevels)
identical(x1,x2)

##superSet is slower by a factor of 2
#system.time(replicate(500,QCA3:::superSet(rep(1,4))))
#system.time(replicate(500,findPrimes(rep(2,4) + 1,81)))

