associations <- function(x,y){
    require(CoCo)
    tab <- table(x, y)
    ## create a contingency table
    tryCatch(CoCo::endCoCo(),error=function(e)invisible())
    ## end CoCo object if there is any
    cc <- CoCo::makeCoCo() ##  create a CoCo object
    CoCo::enterTable(tab, object = cc) ## add contingency table to CoCo object
    ## show variables in the CoCo object
    CoCo::partialAssociations(CoCo::returnVariableDescription(),object = cc)
    ## return associations
    CoCo::endCoCo()
}
