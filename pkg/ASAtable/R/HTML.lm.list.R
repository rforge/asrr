HTML.lm.list <- function(
                        x,
                        prefix="模型",
                           digits=2,
                           emptycell="...",
                           coeffun=function(x) summary(x)$coef,
                           coefL2Mfun=cbindcoeflist1,
                           begin.numering=1,
                           asterisk=TRUE,
                           STUBCOL=rep(c("系数","标准误"),times=length(x)),
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
  nobs <- sapply(x,function(x) length(x$y))
  add.info <- matrix(nobs,byrow=TRUE,ncol=nmodel)
  rownames(add.info) <- "有效样本量"
 if (!is.null(goffun)){
  .gof <- t(as.matrix(sapply(x,goffun)))
  rownames(.gof) <-"模型拟合"
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
  HTMLtable(x=model.coef.total,y=add.info,cgroup=mgroup,n.cgroup=n.mgroup,stub.title="自变量",asterisk=asterisk,...)
  } else HTMLtable(x=model.coef.total,y2=add.info,cgroup=mgroup,n.cgroup=n.mgroup,stub.title="自变量",asterisk=asterisk,...)

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
