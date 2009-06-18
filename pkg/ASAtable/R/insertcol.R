insertcol <- function(
                      mat,
                      ncolgroup,
                      elements=NA,
                      insertcolnames=NA
                      )
  ##mat,matrix.
  ##ncolgroup, numeric vector, sum(ncolgroup)=ncol(charmat).
  ##   将mat按照col分组，在各个分组之间插入新的一列，ncolgroup设定每一组的的列数。
  ##elements, sepcified the elements of the inserted cols.
  ## insertcolnames, the colnames corresponding to the col inserted.
{
if (length(ncolgroup)==1) outvector <- mat else{
  if (!is.matrix(mat)) stop("mat must be matrix.")
  mat2vector <- as.vector(mat)
  atts <- attributes(mat)
  n.row <- NROW(mat)
  n.col <- NCOL(mat)
  n.col.insert <- length(ncolgroup)-1
  if (sum(ncolgroup)!=n.col) stop("wrong ncolgroup argument.")
  outvector <- vector(mode(mat),n.row*(n.col + n.col.insert))
  n.elem.be.ins <- (n.row*ncolgroup)[-length(ncolgroup)]
  ##how many elements before each insert char.
  insert.index <- rep(cumsum(n.elem.be.ins),each=n.row) + 1:(n.row*n.col.insert)
  outvector[insert.index] <- elements
  outvector[-insert.index] <- mat2vector
  atts$dim <- c(n.row,n.col.insert+n.col)
  if (!is.null(atts$dimnames[[2]])){
    dm.index <- cumsum(ncolgroup)[-length(ncolgroup)] + 1:n.col.insert
    newcolnames <- character(n.col + n.col.insert)
    newcolnames[dm.index] <- insertcolnames
    newcolnames[-dm.index] <- atts$dimnames[[2]]
    atts$dimnames[[2]] <- newcolnames
  }
  attributes(outvector) <- atts
}
  outvector
}
