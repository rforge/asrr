## part of the ASRR package.
## R/associations.R
## By Ronggui HUANG 25-03-2010

associations <- function(x,y){
## x, y are vectors, passed to table
## This function packs codes to make easy call to CoCo::partialAssociations
    require(CoCo)
    xname <- deparse(substitute(x))
    xname <- gsub("[$\\.]", "_", xname)
    yname <- deparse(substitute(y))
    yname <- gsub("[$\\.]", "_", yname)
    tab <- table(x, y)
    ## create a contingency table
    names(dimnames(tab)) <- c(xname,yname)
    tryCatch(CoCo::endCoCo(object=cc),error=function(e) invisible())
    ## end CoCo object if there is any
    cc <- CoCo::makeCoCo() ##  create a CoCo object
    CoCo::enterTable(tab, object = cc) ## add contingency table to CoCo object
    ## show variables in the CoCo object
    CoCo::partialAssociations(CoCo::returnVariableDescription(object=cc)$names,object = cc)
    ## return associations
    CoCo::endCoCo(object=cc,silent=TRUE)
}
