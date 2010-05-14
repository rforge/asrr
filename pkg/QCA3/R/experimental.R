combnBySplit <- function(NR,k){
    library(bigmemory)
    bigMatrix <- filebacked.big.matrix(nrow=k,ncol=choose(NR,k),type="integer",backingfile="",descriptorfile="",init=0)
    NR1 <-  seq(from=1,to=round(NR*0.5))
    NR2 <-  seq(from=round(NR*0.5)+1,to=NR)

    idx <- 1
    for (i in 1:(k-1)){
        combos1 <- combn(NR1, i)
        combos2 <- combn(NR2, k-i)
        for (j1 in 1:ncol(combos1)){
            combos1n <- tryCatch(matrix(rep(combos1[,j1],each=ncol(combos2)),nrow=nrow(combos1),byrow=T),error=function(e)NULL)
            if (!is.null(combos1n)){
                combos <- tryCatch(rbind(combos1n,combos2),error=function(e)NULL)
                if (!is.null(combos)){
                    ## partially eliminate the for loop
                    bigMatrix[,seq(from=idx,length=ncol(combos))] <- combos
                    idx <- idx + ncol(combos)
                }} else {## partition combos2 and repeat the above will be better than loop ove all combos2
                    for (j2 in 1:ncol(combos2)){
                        bigMatrix[,idx] <- c(combos1[,j1],combos2[,j2])
                        idx <- idx +1
                    }
                }
        }
    }
    if (length(NR1)>=k){
        bigMatrix[,seq(from=idx,to=idx+(choose(length(NR1),k)-1))] <- combn(NR1, k)
        idx <- idx+(choose(length(NR1),k))
    }
    if (length(NR2)>=k){
        bigMatrix[,seq(from=idx,to=idx+(choose(length(NR2),k)-1))] <- combn(NR2, k)
        ##idx <- idx+(choose(length(NR2),k))
    }
    bigMatrix
}


## x=combnBySplit(100,5)
## combn(100,3) ~= combnBySplit(100,3)


