CSSgenerator <-  function(
                          fontsize,
                          indent,
                          tablewidth,
                          lwidth,
                          firstline,
                          ...)
  ##the meaning of the argument,see HTMLtable()
{
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


toHTML <- function(x,...){
UseMethod("toHTML")
}

toHTML.matrix <- function(
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
                      codepage="gb2312",
                      digits=2,
                      ...
                      )
  ##x,y, matrix,table,data frame,possible (row/col)named. x,y can't be vector.
  ##x is the main table
  ##y is appended to the end to the table,containg info(such as number of case, goodness of fit).NCOL(y)=length(cgroup).
  ##file, character string specified the output file name.
  ##capition, character string specified the table title("\n" should be changed to <br>)
  ##indent, non-negative integer, how many character to indent.
  ##note, character string to specified the footnote.Be pre-prepared before passed to the output file. especially the width
  ##digits, argument passed to format function. It determines the "exact" digits. passd to formatC().
  ##tablewidth,numeric, specified the table width; or something like "n%". find a better solution to allocate space.
  ## 600 刚好横向Word文档的宽度，900是纵向宽度.
  ##fontsize, numeric, the unit is pt. specified the fontsize for table entries.
  ##autobrowse,logical, if the file autobrowsed.
  ##append,logical ,if the table appended to the current file or override the current file.
  ##cgroup,character vector specified the col title.
  ##n.cgroup, integer vector specified how many col a col group has.
  ##rgroup, not used now.
  ##n.rgoup, not used now.
  ##line style of the first line. "double","solid" ...
  ##stub.title, character specified the sub col title. the default is "&nbsp" which is a html tag(means a space).
  ##toword, logical, transform the file into MS Word. needs svViews. but not works well with chinese.
  ##asterisk, logical,if specified the note about *. make it much more smart.
  ##colwidth, specified the col width, can be "equal", "prop" or
  ##   vector of length NROW(x)+lengtg(cgroup)-1(因为需要插入空白的间隔列).空白列应贯穿到底的。
  ##codepage,html's codepage.
{
  ##如果分组，分组信息作为一部分，表的具体内容作为一个部分，将附加的内容作为一个新的部分，将注释作为最后一个部分。
  ##用于增加一个列，使得group之间的横线不相互连在一起。需要注意这一列的宽度设置很小。
 
  CSS.def <- CSSgenerator(fontsize,indent,tablewidth,lwidth,firstline,...)
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
  }###todo: 今后使用css控制caption的外观。

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
    colwidth <- c(colwidth[1],insertcol(t(colwidth[-1]),ncolgroup=n.cgroup,sepwidth/tablewidth))*100
    colwidth <- sprintf("%.1f%%",colwidth)
  } else {
    colwidth <- colwidth/sum(colwidth)
    colwidth <- sprintf("%.1f%%",colwidth*100)
  }

  ##如果有分组，则增加分组的信息
  if (!hasgroup)  STUBCOL1 <- NULL else {
    newcgroup <- insertcol(mat=t(cgroup),ncolgroup=rep(1,length(cgroup)),elements=" ")
    colspan <- cbind(1,insertcol(t(n.cgroup),ncolgroup=rep(1,length(cgroup)),elements=1))
    cl.mat.stub <- matrix("CGROUP",nrow=nrow(colspan),ncol=ncol(colspan))
    nonbotind <- seq_len(ncol(cl.mat.stub)-1)[seq_len(ncol(cl.mat.stub)-1) %%2 ==1]##index，指出哪些列是不需要底边框的。
    cl.mat.stub[,nonbotind] <- "SEPCOL" 
    cl.mat.stub[1,1] <- "SEPCOLLEFT"
    STUBCOL1 <- HTMLcell(newcgroup,row.title=stub.title,colspan=colspan,class.mat=cl.mat.stub)
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
    insertcol(mat=x,ncolgroup=n.cgroup,elements="&nbsp;",insertcolnames="&nbsp;")
  } else x ##如果对列分组，则需要先调整表的内容。
  if (!is.null(rgroup)) {
    new.x <- t(insertcol(t(new.x),ncolgroup=c(0,n.rgroup),elements="&nbsp;",insertcolnames=rgroup))
  }##如果对行分组，则需要调整表的内容
  cl.body1 <- matrix("BODYCELL",nrow=nrow(new.x)+1,ncol=ncol(new.x)+1)
  ##如果HTMLcell中的row.title和col.title为NULL，需要进一步处理
  cl.body1[1,]<- "BODYCELLBOTTOM"
  cl.body1[2:nrow(cl.body1),1]  <- "STUBCOLMAIN"
  cl.body1[c(FALSE,rownames(new.x) %in% rgroup),1]  <- "STUBCOLRGROUP"  
  TBODY1 <- HTMLcell(new.x,row.title=rownames(new.x),col.title=colnames(new.x),class.mat=cl.body1,width.mat=matrix(colwidth,byrow=TRUE,nrow=nrow(new.x)+1,ncol=ncol(new.x)+1))
  
  ##放用于增加例如样本数、模型拟合度等信息的部分。在表主体的下方每一个信息占据的列数等于n.cgroup
  if (!is.null(y)){
   if (!is.matrix(y)) stop("y must be a matrix.")
   if (mode(y)=="numeric") y <- gsub(" ","&nbsp;",format(formatC(y,digits=digits,format="f"),justify="right"),fixed=TRUE)
    new.y <- insertcol(y,ncolgroup=rep(1,length(cgroup)),elements=" ")
    colspan.y <- insertcol(matrix(n.cgroup,ncol=ncol(y),nrow=nrow(y),byrow=T),ncolgroup=rep(1,length(cgroup)),elements=1)
    if (is.null(rownames(new.y)))  {
      warning("y should have rownames.")
      row.title.y <- "&nbsp;"
    } else row.title.y <- rownames(new.y)
  cl.body2 <- matrix("BODYCELL",nrow=nrow(new.y),ncol=ncol(new.y)+1)
  cl.body2[,1]<- "STUBCOLRGROUP"
    TBODY2 <- HTMLcell(new.y,row.title=row.title.y,colspan=cbind(1,colspan.y),class.mat=cl.body2)   
  } else TBODY2 <- NULL

  ##放用于增加新的信息。x的列数与y2的列数相同
  if (!is.null(y2)){
   if (!is.matrix(y2)) stop("y2 must be a matrix.")
   if (mode(y2)=="numeric") y2 <- gsub(" ","&nbsp;",format(formatC(y2,digits=digits,format="f"),justify="right"),fixed=TRUE)
    new.y2 <- insertcol(y2,ncolgroup=n.cgroup,elements=" ")
#    colspan.y2 <- insertcol(matrix(1,ncol=ncol(y2),nrow=nrow(y2),byrow=T),ncolgroup=rep(1,length(cgroup)),elements=1)
    if (is.null(rownames(new.y2)))  {
      warning("y2 should have rownames.")
      row.title.y2 <- "&nbsp;"
    } else row.title.y2 <- rownames(new.y2)
  cl.body2b <- matrix("BODYCELL",nrow=nrow(new.y2),ncol=ncol(new.y2)+1)
  cl.body2b[,1]<- "STUBCOLRGROUP"
    #TBODY2b <- HTMLcell(new.y2,row.title=row.title.y2,colspan=cbind(1,colspan.y2),class.mat=cl.body2)   
 TBODY2b <- HTMLcell(new.y2,row.title=row.title.y2,class.mat=cl.body2b)   

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
  
  ##结束表格
  END.def <- "</TABLE>"
  
  ##打印到文件
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
  
  ##是否自动在IE中打开
  if (autobrowse)  {
    fullpath <- if (basename(file)==file) file.path(getwd(),file) else file
    browseURL(fullpath)
  }

  ##是否输出到word中.如果charset不设置，会乱码。
  if (.Platform$OS.type != "windows") msword <- FALSE
  if (msword){
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
                           digits=2,
                           emptycell="...",
                           coeffun=function(x) summary(x)$coef,
                           coefL2Mfun=cbindcoeflist1,
                           begin.numering=1,
                           asterisk=TRUE,
                           STUBCOL=rep(c("Coefficient","S.E."),times=length(x)),
                           mgroup=NULL,
                           n.mgroup=rep(2,times=length(x)),
                           gname=NULL,
                           relabel=NULL,
                           goffun=NULL,
                           ...
                           )
  #x : named list of glm model. like x=list(model1=glm1...)
  #prefix to the col title.
  #digits, integer, the exact digits to report.
  #character representing the emptycell.
  #function to get the information to be reported. the result should be has colnames and rownames.
  #begin.numering, integer, the numbering of col title, eg, model 1, model 2,...
{
 
  #get the basic info.
  nmodel <- length(x)
  eachM2G <- ifelse(is.null(mgroup),TRUE,FALSE)#是否每个模型作为一组
  nobs <- sapply(x,function(x) nrow(x$model))
  add.info <- matrix(nobs,byrow=TRUE,ncol=nmodel)
  rownames(add.info) <- "Number of cases"
 if (!is.null(goffun)){
  .gof <- t(as.matrix(sapply(x,goffun)))
  rownames(.gof) <-"Goodness of fit"
 add.info <- rbind("&nbsp;",add.info,"&nbsp;",.gof)
 }
  if (is.null(gname))
  {model_name <- paste(paste(prefix,seq_len(nmodel)+begin.numering-1,sep=" "),sapply(x,function(x) as.character(formula(x$call)[[2]])),sep="<br>") 
   # as the first level col title
   }else model_name <- paste(paste(prefix,seq_len(nmodel)+begin.numering-1,sep=" "),gname,sep="<br>") 
  model_summary_coef <- lapply(x,coeffun)
  model_coef <- lapply(model_summary_coef,formatcoef,digits=digits)
  model_coef[["COLNAMES"]] <-  STUBCOL
  ##这是的colnames 将会变成level 2的col title。可以根据需要改变。
  model.coef.total <- do.call(coefL2Mfun,model_coef) # turn a list to a whole matrix.
  nvar.total <- NROW(model.coef.total)
  ncol.each <- ncol(model.coef.total)/nmodel
  if (!is.null(relabel)) {
  rownames(model.coef.total)<-sub(":","-",rownames(model.coef.total))
  names(relabel)<-sub(":","-",names(relabel))#recode不能处理还有":"的数据
  rownamesNew=car:::recode(rownames(model.coef.total),paste("'",names(unlist(relabel)),"'='",gsub(":","_",unlist(relabel)),"'",sep="",collapse=";"))
  rownames(model.coef.total) <- rownamesNew
}
  if (is.null(mgroup)){
  mgroup <- model_name
  n.mgroup <- rep(ncol.each,nmodel)
} 
  if (eachM2G) {
  toHTML.matrix(x=model.coef.total,y=add.info,cgroup=mgroup,n.cgroup=n.mgroup,stub.title="Independent Variables",asterisk=asterisk,...)
  } else HTMLtable(x=model.coef.total,y2=add.info,cgroup=mgroup,n.cgroup=n.mgroup,stub.title="Independent Variables",asterisk=asterisk,...)
}

formatcoef <- function(
                       x,
                       ...)
{
  ##格式化系数矩阵，包括补齐空白位以及增加显著性符号。
  ##digits保留小数点后的位数（exact digits），很小的数会被trimmed为0。
   has.p <- c("Pr(>|t|)","Pr(>|z|)") %in% colnames(x)
   if (have.P <- any (has.p))  model_p <- x[,c("Pr(>|t|)","Pr(>|z|)")[which(has.p)]]
##   x <- format(eval(substitute(formatC(x,digits=digits,format="f"),list(x=x)),parent.frame(2)),justif="right")
##或者
    x <- format(eval(quote(formatC(x,digits=digits,format="f")),list(x=x),parent.frame(2)),justif="right")

   ##see S programming of p70, note the usage of substitute
  ##值得多学习format和formatC这种格式化的函数
  if (have.P){
    x[model_p <0.01,"Estimate"]<- paste(x[model_p <0.01,"Estimate"],"**",sep="")
    x[model_p<0.05 & model_p >0.01,"Estimate"]<- paste(x[model_p<0.05 & model_p >0.01,"Estimate"],"*&nbsp;",sep="")
    x[model_p<0.1 & model_p >0.05,"Estimate"]<- paste(x[model_p<0.1 & model_p >0.05,"Estimate"],"+&nbsp;",sep="")
    x[model_p>0.1,"Estimate"]<- paste(x[model_p>0.1,"Estimate"],"&nbsp;&nbsp;",sep="")
  }
  x <- gsub(" ","&nbsp;",x,fixed=TRUE) #补齐空白位,注意空格是&nbsp;，注意后面有一个分号
  x
}


cbindcoeflist1 <- function(...,COLNAMES=NULL)
{
                                        # helper function to combine model with common varaibles
                                        #... is a list as cbind, but can deal with objects of different length.
  input <- list(...) #input不会包含任何COLNAMES的信息。
  row_name_list <- lapply(input,rownames)
  all_row_name <- unique(unlist( row_name_list ))
  emptycell <- eval(expression(emptycell),parent.frame()) #从parent frame获取该信息
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

cbindcoeflist2 <- function(...,COLNAMES=NULL)
{
                                        # helper function to combine model with common varaibles
                                        #... is a list as cbind, but can deal with objects of different length.
  input <- list(...) #input不会包含任何COLNAMES的信息。
  row_name_list <- lapply(input,rownames)
  all_row_name <- unique(unlist( row_name_list ))
  emptycell <- eval(expression(emptycell),parent.frame()) #从parent frame获取该信息
  x2 <- matrix(emptycell,ncol=length(input),nrow=length(all_row_name))
  rownames(x2) <-  all_row_name
  for (i in seq_along(input)) {
    rownames2 <- rownames(input[[i]])
    index <- match(rownames2,all_row_name)
    value<-as.matrix(paste(input[[i]][,c("Estimate")],"<br>(",gsub("&nbsp;","",input[[i]][,c("Std. Error")]),")",sep=""))
    x2[index,i] <- value
  }
  if (!is.null(COLNAMES)) colnames(x2) <- COLNAMES
  x2
}


