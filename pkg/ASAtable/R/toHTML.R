CSSgenerator <-  function(fontsize,indent,tablewidth,lwidth,firstline,...)
{
  ## see toHTML.matrix() for the meaning of the argument
  css.def <- "
<STYLE TYPE='text/css'>
   TABLE {
   font-size: fontsizept;
   }
   TD.BODYCELL{
     text-align:center;
    }
   TD.BODYCELLBOTTOM{
     text-align:center;
     border-top:none ;
     border-left:none;
     border-bottom:solid windowtext lwidth;
     border-right:'none';
    }
   TD.STUBCOLMAIN{
     text-align:left;
     text-indent:ptindentpt;
    }
   TD.STUBCOLRGROUP{
     text-align:left;
    }
   TD.CGROUP{
      border-top:none;
      border-left:none;
      border-bottom:solid windowtext lwidth;
      border-right:none;
      text-align:center;
    }
  TD.SEPCOL{
     border-top:none
     border-left:none;
     border-bottom:none;
     border-right:none;
     text-align:center;
    }
   TD.SEPCOLLEFT{
      border-top:none;
      border-left:none;
      border-bottom:none;
      border-right:none;
      text-align:left;
    }
  TD.FOOTNOTE{
      border-top:solid windowtext lwidth;
      text-align:left;
    }
  </STYLE>"
  css.def <- gsub("fontsize",fontsize,css.def,fixed=TRUE)
  css.def <- gsub("ptindent",indent*fontsize,css.def,fixed=TRUE)
  css.def <- gsub("tablewidth",tablewidth,css.def,fixed=TRUE)
  css.def <- gsub("lwidth",lwidth,css.def)
  css.def <- gsub("firstline",firstline,css.def)
  css.def
}


insertCol <- function(mat,ncolgroup=rep(2,NCOL(mat)/2),elements=NA,insertcolnames=NA)
{
  ngrp <- length(ncolgroup)
  nr <- NROW(mat)
  nc <- NCOL(mat)
  if (sum(ncolgroup)!=nc) stop("Wrong ncolgroup argument.")
  if (length(elements)==1) elements <- rep(elements,ngrp-1)
  if (length(insertcolnames)==1) insertcolnames <- rep(insertcolnames,ngrp-1)
  idx1.col <- seq_len(nc)
  idx2.col <- apply(outer(idx1.col,cumsum(ncolgroup),">"),1,sum) + idx1.col ## note the use of outer
  idx3.col <- cumsum(ncolgroup[1:(ngrp-1)]) + rank( cumsum(ncolgroup[1:(ngrp-1)])) ## note the use of rank
  ans <- matrix(nrow=NROW(mat),ncol=max(idx2.col))
  ans[,idx2.col] <- mat[,idx1.col]
  ans[,idx3.col] <- matrix(rep(elements,each=nr),nrow=nr)
  rownames(ans) <- rownames(mat)
  if (!is.null(colnames(mat))){
    colnames(ans)[idx2.col] <- colnames(mat)
    colnames(ans)[idx3.col] <- insertcolnames
  }
  ans
}
##mat <- matrix(1:24,ncol=6)
##insertCol(mat)

  
R2HTMLtable <- function(
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
                        height.mat=matrix(NA,nrow=NROW(charmat),ncol=NCOL(charmat))
                        )
#####################################
  ##                   col.title ##
  ## row.title         bodycell  ##
#####################################
  ## The above is a table, explaining the meanings of col.title,row.title and bodycell
  ## charmat: character matrix. Numeric matrix should format/formatC to a suitable char matrix.
  ## isFirstcol,logical. If TRUE, beginning with <TR> tag.
  ## row.title and col.title, character vector for row.tile and col.title
  ## top.headers and left.headers,logical, if the first row and first col treated as headers.
  ##     if yes, use <TH> instead of <TD>
  ## colspan.mat, rowspan.mat, class.mat, width.mat, height.mat
  ##     matrix of the same dimesion of the output table.
  ##     specified the parameters of table cell.eg,colspan=, rowspan=,...
  ## The value is length-1 char vector: the HTML syntax of a table.
{
  if (!is.null(col.title)) {
    charmat <- rbind(col.title,charmat)
    if (!is.null(row.title)) {
      charmat <- cbind(c("&nbsp;",row.title),charmat)## a HTLM tag of blank rather than NA.
    }
  } else {
    if (!is.null(row.title)) {
      charmat <- cbind(row.title,charmat)
    }
  } ## add title if necessary
  emtpycell <- is.na(charmat) ## data element is missing
  ignoretags <- is.na(colspan.mat) | is.na(rowspan.mat)
  ## the HTML tags be igored in the final HTML.
  ## make use of the lazy evaluation mechanism, so NROW(x) and NCOL(x) will get the right number.
  tags <- paste("<TD\tCLASS=",class.mat,"\tCOLSPAN=", colspan.mat,"","\tROWSPAN=",rowspan.mat, "\tWIDTH=",width.mat,"\t HEIGHT=",height.mat,"\t>", charmat, "\t</TD>",sep="")
  ## char vector of nrow(charmat)*ncol(charmat), representing the HTML tags for each cell.
  dim(tags) <- dim(charmat)## turn tags into a matrix
  tags[emtpycell] <- sub ("(>)[[:blank:]]{0,}NA[[:blank:]]{0,}(\t</TD>)$",">\t</TD>",tags[emtpycell]) ## sub NA with blank.
  if (isFirstcol) tags[,1] <- sub("<TD","<TR>\n<TD",tags[,1],fixed=TRUE)
  if (top.headers) tags[1,] <- sub("TD","TH",tags[1,],fixed=TRUE)
  if (left.headers) tags[,1] <- sub("TD","TH",tags[,1],fixed=TRUE)
  tags <- paste(c(t(tags))[!c(t(ignoretags))],collapse="\n")
  tags
}


toHTML <- function(x,...){
  UseMethod("toHTML")
}

toHTML.default <- function(
                          x,
                          y=NULL,
                          y2=NULL,
                          file=paste(tempfile(),".html",sep=""),
                          caption="Add Table Title Here!",
                          note=NULL,
                          tablewidth=600,
                          autobrowse=TRUE,
                          msword=FALSE,
                          append=TRUE,
                          cgroup=NULL,
                          n.cgroup=NULL,
                          rgroup=NULL,
                          n.rgroup=NULL,
                          stub.title="&nbsp",
                          colwidth="prop",
                          colname=NULL,
                          firstline="double",
                          lwidth="1pt",
                          lang="zh",
                          fontsize=10,
                          indent=1,
                          asterisk=FALSE,
                          sepwidth=12,
                          codepage="UTF-8",
                          digits=2
                          )
  ##x,y: matrix,table,data frame but not vector,possible (row/col) named.
  ##x is the main table
  ##y is appended to the end to the table,containg info such as number of case, goodness of fit.
  ##    NCOL(y)=length(cgroup).
  ##file: character string specified the output file name.
  ##capition: character string specified the table title("\n" should be changed to <br>)
  ##indent, non-negative integer, how many character to indent.
  ##note, character string to specified the footnote.Be pre-prepared before passed to the output file. especially the width
  ##digits, argument passed to format function. It determines the "exact" digits. passd to formatC().
  ##tablewidth: length-1 numeric,to specify table width; or something like "n%".
  ##    need to find a better solution to allocate space.
  ##    600 fit for default width of MS word. The other is 900.
  ##fontsize: numeric, the unit is pt. specified the fontsize for table entries.
  ##autobrowse: logical, if the file autobrowsed.
  ##append: logical, if the table appended to the current file.
  ##cgroup,character vector specified the col title. Works only when rownames is not null.
  ##n.cgroup, integer vector specified how many col a col group has.
  ##rgroup, not used now.
  ##n.rgoup, not used now.
  ##line style of the first line. "double","solid" ...
  ##stub.title: character specified the sub col title.
  ##    Default is "&nbsp" which is a html tag(means a space).
  ##toword: logical, convert html into MS Word. svViews is needed. Not work well with chinese.
  ##asterisk: logical,if specified the note about *. make it much more smart.
  ##colwidth: specified the col width, can be "equal", "prop" or
  ##   vector of length NROW(x)+ (length(cgroup)-1)
  ##codepage: html's codepage.
{  
  x <- as.matrix(x)
  ##如果分组，分组信息作为一部分，表的具体内容作为一个部分，附加的内容作为一个新的部分，将注释作为最后部分。
  ##用于增加一个列，使得group之间的横线不相互连在一起。需要注意这一列的宽度设置很小。
  CSS.def <- CSSgenerator(fontsize,indent,tablewidth,lwidth,firstline)
  HTML.def <- paste("<HTML lang=\"",lang,"\">\n",
                    "<HEAD> \n",
                    " <TITLE> Statistical report generated by R at ",date(),"</TITLE> \n",
                    "<META charset='",
                    codepage,
                    "'></HEAD> \n",
                    sep="")
  TBEGIN.def <- paste("<TABLE cellspacing=0 cellpadding=0 border=0 align='center' style='width:",tablewidth,"'>")
  CAPTION.def <- if (firstline=="double") {
    paste("<CAPTION style='border-bottom:double windowtext 1.5pt'>",caption,"</CAPTION>")
  } else {
    paste("<CAPTION style='border-bottom:solid windowtext 1.5pt'>",caption,"</CAPTION>")
  } ##todo: 今后使用css控制caption的外观。
  ##是否有对列进行分组，并计算组数
  hasgroup=FALSE
  if (!is.null(cgroup)) {
    hasgroup <- TRUE
    Ngroup <- length(cgroup)
  }
  ##分配表格的列宽度
  x4w <- gsub("&nbsp;","",x)
  if (length(colwidth)==1){
    colwidth <- switch(colwidth,
                       "prop"=c(max(nchar(rownames(x4w))),
                         apply(rbind(x4w,colnames(x4w)),2,function(tab) max(nchar(tab),na.rm=T))),
                       "equal"=rep(1,NCOL(x)+1)
                       )
  }
  if (hasgroup) {
    sepwidthtotal <- sepwidth*(Ngroup-1)
    pctcell <- (tablewidth-sepwidthtotal)/tablewidth
    colwidth <- colwidth/sum(colwidth)*pctcell
    colwidth <- c(colwidth[1],insertCol(t(colwidth[-1]),ncolgroup=n.cgroup,sepwidth/tablewidth))*100
    colwidth <- sprintf("%.1f%%",colwidth)
  } else {
    colwidth <- colwidth/sum(colwidth)
    colwidth <- sprintf("%.1f%%",colwidth*100)
  }
  ##如果有分组，则增加分组的信息
  if (!hasgroup)  STUBCOL1 <- NULL else {
    newcgroup <- insertCol(mat=t(cgroup),ncolgroup=rep(1,length(cgroup)),elements=" ")
    colspan <- cbind(1,insertCol(t(n.cgroup),ncolgroup=rep(1,length(cgroup)),elements=1))
    cl.mat.stub <- matrix("CGROUP",nrow=nrow(colspan),ncol=ncol(colspan))
    nonbotind <- seq_len(ncol(cl.mat.stub)-1)[seq_len(ncol(cl.mat.stub)-1) %%2 ==1]
    ##index，指出哪些列是不需要底边框的。
    cl.mat.stub[,nonbotind] <- "SEPCOL"
    cl.mat.stub[1,1] <- "SEPCOLLEFT"
    STUBCOL1 <- R2HTMLtable(newcgroup,row.title=stub.title,colspan=colspan,class.mat=cl.mat.stub)
  }
  ##表的主体部分
  if (!is.matrix(x)) stop("x must be a matrix.")
  if (mode(x)=="numeric") x <- gsub(" ","&nbsp;",format(formatC(x,digits=digits,format="f"),justify="right"),fixed=TRUE)
  if (is.null(colnames(x))) {
    warning("x does not have colnames, added by me.")
    colnames(x) <- if (is.null(colname)) paste("colname",as.character(seq_len(ncol(x))),sep=".") else colname
  }##如果没有colnames，则增加
  if (is.null(rownames(x))) {
    warning("x does not have rownames, added by me.")
    rownames(x) <- paste("rowname",as.character(seq_len(nrow(x))),sep=".")
  }##如果没有rownames，则增加
  ##todo: 允许没有rownames和colnames的情况
  new.x <- if (hasgroup) {
    insertCol(mat=x,ncolgroup=n.cgroup,elements="&nbsp;",insertcolnames="&nbsp;")
  } else x ##如果对列分组，则需要先调整表的内容。
  if (!is.null(rgroup)) {
    new.x <- t(insertCol(t(new.x),ncolgroup=c(0,n.rgroup),elements="&nbsp;",insertcolnames=rgroup))
  }##如果对行分组，则需要调整表的内容
  cl.body1 <- matrix("BODYCELL",nrow=nrow(new.x)+1,ncol=ncol(new.x)+1)
  ##如果R2HTMLtable中的row.title和col.title为NULL，需要进一步处理
  cl.body1[1,]<- "BODYCELLBOTTOM"
  cl.body1[2:nrow(cl.body1),1]  <- "STUBCOLMAIN"
  cl.body1[c(FALSE,rownames(new.x) %in% rgroup),1]  <- "STUBCOLRGROUP"
  TBODY1 <- R2HTMLtable(new.x,row.title=rownames(new.x),col.title=colnames(new.x),class.mat=cl.body1,width.mat=matrix(colwidth,byrow=TRUE,nrow=nrow(new.x)+1,ncol=ncol(new.x)+1))
  ##放用于增加例如样本数、模型拟合度等信息的部分。在表主体的下方每一个信息占据的列数等于n.cgroup
  if (!is.null(y)){
    if (!is.matrix(y)) stop("y must be a matrix.")
    if (mode(y)=="numeric") y <- gsub(" ","&nbsp;",format(formatC(y,digits=digits,format="f"),justify="right"),fixed=TRUE)
    new.y <- insertCol(y,ncolgroup=rep(1,length(cgroup)),elements=" ")
    colspan.y <- insertCol(matrix(n.cgroup,ncol=ncol(y),nrow=nrow(y),byrow=T),ncolgroup=rep(1,length(cgroup)),elements=1)
    if (is.null(rownames(new.y)))  {
      warning("y should have rownames.")
      row.title.y <- "&nbsp;"
    } else row.title.y <- rownames(new.y)
    cl.body2 <- matrix("BODYCELL",nrow=nrow(new.y),ncol=ncol(new.y)+1)
    cl.body2[,1]<- "STUBCOLRGROUP"
    TBODY2 <- R2HTMLtable(new.y,row.title=row.title.y,colspan=cbind(1,colspan.y),class.mat=cl.body2)
  } else TBODY2 <- NULL
  ##放用于增加新的信息。x的列数与y2的列数相同
  if (!is.null(y2)){
    if (!is.matrix(y2)) stop("y2 must be a matrix.")
    if (mode(y2)=="numeric") y2 <- gsub(" ","&nbsp;",format(formatC(y2,digits=digits,format="f"),justify="right"),fixed=TRUE)
    new.y2 <- insertCol(y2,ncolgroup=n.cgroup,elements=" ")
    ## colspan.y2 <- insertCol(matrix(1,ncol=ncol(y2),nrow=nrow(y2),byrow=T),ncolgroup=rep(1,length(cgroup)),elements=1)
    if (is.null(rownames(new.y2)))  {
      warning("y2 should have rownames.")
      row.title.y2 <- "&nbsp;"
    } else row.title.y2 <- rownames(new.y2)
    cl.body2b <- matrix("BODYCELL",nrow=nrow(new.y2),ncol=ncol(new.y2)+1)
    cl.body2b[,1]<- "STUBCOLRGROUP"
                                        #TBODY2b <- R2HTMLtable(new.y2,row.title=row.title.y2,colspan=cbind(1,colspan.y2),class.mat=cl.body2)
    TBODY2b <- R2HTMLtable(new.y2,row.title=row.title.y2,class.mat=cl.body2b)

  } else TBODY2b <- NULL
  ##处理表格的注释
  if (!is.null(note)) note <- gsub("\n","<br>",note,fixed=TRUE)#将\n换为HTML的换行符号
  totalcol <- if (hasgroup) sum(n.cgroup)+length(cgroup) else NCOL(x)+1 #因为有一列是从rownames来的。
  NOTE.def <- paste(
                    "<TR><TD CLASS=FOOTNOTE COLSPAN=",
                    totalcol,
                    ">",
                    if (is.null(note)) "Note: ADD NOTES HERE, FIRST DATA SOURCE, SECOND IS GENERAL INFO, THEN IS CALLOUTS."
                    else note,
                    if (asterisk) "<BR>+ p<.10 <BR>* p<.05 <BR>** p<.01<BR>", "</td>")
  END.def <- "</TABLE>" ## html tag of end of table
  cat(
      if (!append) HTML.def,
      CSS.def,
      TBEGIN.def,
      CAPTION.def,
      STUBCOL1,
      TBODY1,
      TBODY2,TBODY2b,
      NOTE.def,
      END.def,
      file=file,append=append,sep="\n"
      )
  if (autobrowse)  {
    fullpath <- if (basename(file)==file) file.path(getwd(),file) else file
    browseURL(fullpath)
  }
  if (.Platform$OS.type != "windows") msword <- FALSE
  if (msword){
    ##是否输出到word中.如果charset不设置，会乱码。
    svViews:::WordOpen()
    svViews:::WordGotoEnd()
    svViews:::WordInsertPara()
    svViews:::WordInsertFile(file)
  }
}

modelList <- function(...){
  ans <- list(...)
  class(ans) <- "modelList"
  ans
}

toHTML.modelList <- function(
                             x,
                             prefix="Model",
                             begin.numering=1,
                             group.name=NULL,
                             stub.col=rep(c("Coefficient","S.E."),times=length(x)),
                             digits=2,
                             emptycell="...",
                             relabel=NULL,
                             goffun=NULL,
                             file=NULL,
                             coeffun=function(x) summary(x)$coef,
                             asterisk=TRUE,
                             coefL2Mfun=cbindCoef,
                             ...
                             )
  ##x : named list of glm model. like x=list(model1=glm1...)
  ##prefix to the col title.
  ##digits, integer, the exact digits to report.
  ##character representing the emptycell.
  ##function to get the information to be reported. the result should has colnames and rownames.
  ##begin.numering, integer, the numbering of col title, eg, model 1, model 2,...
{
  nmodel <- length(x) ## number of model
  nobs <- sapply(x,function(x) nrow(x$model)) ## number of obs for each model
  add.info <- matrix(nobs,byrow=TRUE,ncol=nmodel) ## additional info at the end of table
  rownames(add.info) <- "Number of cases"
  if (!is.null(goffun)){ ## goodness of fit for each model
    gof <- t(as.matrix(sapply(x,goffun)))
    rownames(gof) <-"Goodness of fit"
    add.info <- rbind(add.info,gof)
  }
  if (is.null(group.name)) {
   model_name <- paste(paste(prefix,seq_len(nmodel)+begin.numering-1,sep=" "),sapply(x,function(x) as.character(formula(x$call)[[2]])),sep="<br>")
   ## as level 1 col.title
   } else model_name <- paste(paste(prefix,seq_len(nmodel)+begin.numering-1,sep=" "),group.name,sep="<br>")
  model_summary_coef <- lapply(x,coeffun)
  model_coef <- lapply(model_summary_coef,formatCoef,digits=digits)
  model_coef[["COLNAMES"]] <-  stub.col ## colnames as level 2 col.title
  model.coef.total <- do.call(coefL2Mfun,model_coef) # turn a list to a whole matrix.
  nvar.total <- NROW(model.coef.total)
  ncol.each <- ncol(model.coef.total)/nmodel
  if (!is.null(relabel)) {
    rownames(model.coef.total)<-sub(":","-",rownames(model.coef.total))
    names(relabel)<-sub(":","-",names(relabel)) ##recode cannot handle ":"
    rownamesNew <- car:::recode(rownames(model.coef.total),paste("'",names(unlist(relabel)),"'='",gsub(":","_",unlist(relabel)),"'",sep="",collapse=";"))
    rownames(model.coef.total) <- rownamesNew
  }
  n.mgroup <- rep(ncol.each,nmodel)
  file <- ifelse(is.null(file),paste(tempfile(),".html",sep=""),file)
  toHTML.default(x=model.coef.total,y=add.info,cgroup=model_name,n.cgroup=n.mgroup,stub.title="Independent Variables",asterisk=asterisk,file=file,...)
}


formatCoef <- function(x,...)
{
  ## format coef matrix: 1) each element has the same number of chars; 2) add asterisk according to p-value
  ## digits: exact digits. very small number will be trimmed as 0。
  has.p <- c("Pr(>|t|)","Pr(>|z|)") %in% colnames(x)
  if (have.P <- any (has.p))  model_p <- x[,c("Pr(>|t|)","Pr(>|z|)")[which(has.p)]]
  x <- format(eval(quote(formatC(x,digits=digits,format="f")),list(x=x),parent.frame(2)),justif="right")
  ## see S programming of p70, note the usage of substitute
  ## leanr more about format and formatC.
  if (have.P){
    x[model_p <0.01,"Estimate"]<- paste(x[model_p <0.01,"Estimate"],"**",sep="")
    x[model_p<0.05 & model_p >0.01,"Estimate"]<- paste(x[model_p<0.05 & model_p >0.01,"Estimate"],"*&nbsp;",sep="")
    x[model_p<0.1 & model_p >0.05,"Estimate"]<- paste(x[model_p<0.1 & model_p >0.05,"Estimate"],"+&nbsp;",sep="")
    x[model_p>0.1,"Estimate"]<- paste(x[model_p>0.1,"Estimate"],"&nbsp;&nbsp;",sep="")
  }
  x <- gsub(" ","&nbsp;",x,fixed=TRUE) ## add blank when necessary. In HTML blank is tagged as "&nbsp". note the ";".
  x
}


cbindCoef <- function(...,COLNAMES=NULL)
{
  ## helper function to combine model with common varaibles
  ##... is a list as cbind, but can deal with objects of different length.
  input <- list(...) #input will not capture argument of COLNAMES
  row_name_list <- lapply(input,rownames)
  all_row_name <- unique(unlist( row_name_list ))
  emptycell <- eval(expression(emptycell),parent.frame()) #get it from parent frame.
  x2 <- matrix(emptycell,ncol=2*length(input),nrow=length(all_row_name))
  rownames(x2) <-  all_row_name
  for (i in seq_along(input)) {
    rownames2 <- rownames(input[[i]])
    index <- match(rownames2,all_row_name)
    value<-input[[i]][,c("Estimate","Std. Error")]
    x2[index,(2*i-1):(2*i)] <- value
  }
  if (!is.null(COLNAMES)) colnames(x2) <- COLNAMES
  x2
}
