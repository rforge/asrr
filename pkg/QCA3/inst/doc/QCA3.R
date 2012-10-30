### R code from vignette source 'QCA3.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
library(QCA3)
options(SweaveHooks=list(twofig=function() {par(mfrow=c(1,2))},
                         twofig2=function() {par(mfrow=c(2,1))},
                         onefig=function() {par(mfrow=c(1,1))}))


###################################################
### code chunk number 2: QCA3.Rnw:52-55
###################################################
(cst <- cs_truthTable(Lipset_cs,outcome="SURVIVAL",
                      condition=c("GNPCAP", "URBANIZA", "LITERACY", "INDLAB", "GOVSTAB"),
                      cases="CASEID"))


###################################################
### code chunk number 3: QCA3.Rnw:83-84
###################################################
reduce(cst)


###################################################
### code chunk number 4: QCA3.Rnw:91-92
###################################################
reduce(cst, explain="negative")


###################################################
### code chunk number 5: QCA3.Rnw:100-101
###################################################
reduce(cst, remainders="include")


###################################################
### code chunk number 6: QCA3.Rnw:107-108
###################################################
reduce(cst, explain="negative", remainders="include")


###################################################
### code chunk number 7: QCA3.Rnw:119-121
###################################################
ansNeg <- reduce(cst, explain="negative", remainders="include")
SA(ansNeg)


