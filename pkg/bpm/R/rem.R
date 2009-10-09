"rem" <-
function(formula,id,data,...)
#Random-effects GLS regression 
{ 
cl <- cl2 <-cl3 <- match.call()
#within effect model
cl2[[1]]<-as.name("fem")
fix.eff <- eval(cl2,parent.frame())
sigma.e2.hat <- crossprod(fix.eff$residual) / (fix.eff$df.model)  #sigma_e^2
#Between effect model
cl3[[1]]<-as.name("bem")
between.eff <- eval(cl3,parent.frame())
RMSE2 <- crossprod(between.eff$residual)/between.eff$df2  #sigma_u^2
# T[i]
idx <- fix.eff$id
Ti <- tapply(idx,idx, length)
g <- range(Ti)
Tbat <- length(Ti)/sum(1/Ti)
#default method for u2
sigma.u2.hat <- max(0,(RMSE2-sigma.e2.hat/Tbat))
##
mf <- match.call(expand.dots = FALSE)
m <- match(c("formula", "id", "data"), names(mf), 0)
mf <- mf[c(1, m)]
mf[[1]]<-as.name("model.frame")
mf <- eval(mf, parent.frame())
mt <- attr(mf, "terms")
X<-model.matrix(mt,mf);Y<-model.response(mf);id<-model.extract(mf,"id")
N <- dim(X)[1] #total obs
# use theta and partially dmean
Ti <- tapply(id,factor(id),length)[factor(id)]
theta <- 1- sqrt(sigma.e2.hat/(as.vector(Ti)*sigma.u2.hat+sigma.e2.hat))
#quasi-demean
xd<-dmean(X,id,theta=theta)
yd<-dmean(as.matrix(Y),id,theta=theta)
n<-dim(xd$xm)[1]
Xbat <- xd$xdm
Ybat <- yd$xdm 
fm <- lm.fit(Xbat,Ybat) 
coe<-coef(fm) ;residual <- resid(fm)  ;fit <- fitted(fm)
sigma.e <- drop(crossprod(residual)/fm$df.residual)
vcov<-sigma.e*solve(crossprod(Xbat))
std.Err<-matrix(sqrt(diag(vcov)),ncol=1)
rownames(std.Err) <- colnames(vcov)
#R2
R2.overall <- cor(X%*%coe,Y)^2
R2.between <-  cor(fix.eff$xd$xm %*% coe, fix.eff$yd$xm,use="pairwise.complete.obs")^2
R2.within <- cor(fix.eff$xd$xdm%*%coe,fix.eff$yd$xdm,use="pairwise.complete.obs")^2
##df
df2 <- fm$df.residual
df1 <- N - df2 - 1 
## wald test of the model The following code is wrong!!
#rss<-sum((fit-mean(Ybat))^2)
#F.model<-(rss/df1)/sigma.e
#mdf <- ifelse(identical(g[1],g[2]),df1,df1-1) 
#Wald <- F.model*mdf
g <- range(Ti)
#t-value and p
tval<-coe/std.Err
p.coef<-2*pnorm(abs(tval),lower=FALSE)
#result
result <- list(coef=coe,vcov=vcov,std.Err=std.Err,R2.overall=R2.overall,
               R2.between=R2.between,R2.within=R2.within,g=g,call=cl,N=N,
               n=n,tval=tval,p.coef=p.coef,sigma.e2.hat=sigma.e2.hat,
               sigma.u2.hat=sigma.u2.hat,theta=theta)
class(result) <- "rem" ;return(result)              
}

