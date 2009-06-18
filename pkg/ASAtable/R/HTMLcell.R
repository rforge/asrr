HTMLcell <- function(
		     charmat,
		     isFirstcol=TRUE,
		     row.title=NULL,
		     col.title=NULL,
		     top.headers=FALSE,
		     left.headers=FALSE,
		     colspan.mat=matrix(1,nrow=NROW(charmat),ncol=NCOL(charmat)),
		     rowspan.mat=matrix(1,nrow=NROW(charmat),ncol=NCOL(charmat)),
		     class.mat=matrix("celldefault",nrow=NROW(charmat),ncol=NCOL(charmat)),
		     width.mat=matrix(NA,nrow=NROW(charmat),ncol=NCOL(charmat)),
		     height.mat=matrix(NA,nrow=NROW(charmat),ncol=NCOL(charmat)),
                     ...
		     )
####################################
  ##                   col.title #
  ## row.title         bodycell  #
####################################
  
  ## charmat, character matrix, maybe colnamed and rownamed .numeric matrix can format/formatC to become suitable matrix.
  ## isFirstcol,logical. 如果等于TRUE，则包括<TR>在开头
  ## row.title and col.title, character vector for row.tile and col.title
  ## top.headers and left.headers,logical ,should the first row and first col treated as headers?
  ##     if yes, use <TH> instead of <TD>
  ## colspan.mat, rowspan.mat, class.mat, width.mat, height.mat,matrix of the same dimesion of the output table.
  ##      specified the parameters of table cell.eg,colspan=, rowspan=,...
  ## 结果是长度为1的character vector，包括相关的HTML table语句。

{
  if (!is.null(col.title)) {
    charmat <- rbind(col.title,charmat)
    if (!is.null(row.title)) {
      charmat <- cbind(c("&nbsp;",row.title),charmat)##这里应该用一个空白符号而不是NA
    }
  } else {                              
    if (!is.null(row.title)) {
      charmat <- cbind(row.title,charmat)
    }
  }

  emtpycell <- is.na(charmat)
  ## data element is missing
  ignoretags <- is.na(colspan.mat) | is.na(rowspan.mat)
  ## the HTML tags be igored in the final HTML.

  ##Here make use of the lazy evaluation mechanism, so the NROW(x) and NCOL(x) will get the right number.
  ##注意这里的注释.
  tags <- paste("<TD\tCLASS=",class.mat,"\tCOLSPAN=", colspan.mat,"","\tROWSPAN=",rowspan.mat, "\tWIDTH=",width.mat,"\t HEIGHT=",height.mat,"\t>", charmat, "\t</TD>",sep="")
  ##The HTML tags

  dim(tags) <- dim(charmat)

  tags[emtpycell] <- sub ("(>)[[:blank:]]{0,}NA[[:blank:]]{0,}(\t</TD>)$",">\t</TD>",tags[emtpycell]) #将NA变成空白
  if (isFirstcol) tags[,1] <- sub("<TD","<TR>\n<TD",tags[,1],fixed=TRUE)
  if (top.headers) tags[1,] <- sub("TD","TH",tags[1,],fixed=TRUE)
  if (left.headers) tags[,1] <- sub("TD","TH",tags[,1],fixed=TRUE)

  tags <- paste(c(t(tags))[!c(t(ignoretags))],collapse="\n")
  tags
}
