"bem" <-
function(formula,id,data,...) #between effects model
# formular has intercept
{
cl <- match.call()
mf <- match.call(expand.dots = FALSE)
m <- match(c("formula", "id", "data"), names(mf), 0)
mf <- mf[c(1, m)]
mf[[1]]<-as.name("model.frame")
mf <- eval(mf, parent.frame())
mt <- attr(mf, "terms")
X <-model.matrix(mt,mf);Y <-model.response(mf);id <- model.extract(mf,"id")
sst <-(length(Y)-1)*var(Y)
r2c <- cor(qr.fitted(qr(X),Y),Y)^2
#demean
xd <- dmean(X,id)
yd <- dmean(as.matrix(Y),id)
Xbati <- xd$xm
Ybati <- yd$xm  
N <-length(Y)
n <- dim(Xbati)[1] # number of units
fm <- lm.fit(Xbati,Ybati)
coe.pri <- coef(fm)
fit <- fitted(fm)
residual <- resid(fm)
##R-square
R2.between <- cor(fit,Ybati)^2
noNA <- !is.na(coe.pri)
drop.vars <- names(coe.pri)[!noNA] #the variables dropped
coe <- coe.pri[noNA]
R2.within <- cor(xd$xdm[,noNA] %*% coe,yd$xdm)^2
R2.overall <- cor(X[,noNA]%*%coe,Y)^2
##
sigma.e <- drop(crossprod(residual)/fm$df.residual)
vcov<-sigma.e*solve(crossprod(Xbati[,noNA]))
std.Err<-matrix(sqrt(diag(vcov)),ncol=1)
rownames(std.Err) <- colnames(vcov)
df2 <- fm$df.residual
df1 <- n - df2 - 1 
rss<-sum((fit-mean(Ybati))^2)
F.model<-(rss/df1)/sigma.e
F.stat <- c(F.model,df1,df2)
p.model <- pf(F.model,df1,df2,lower.tail = FALSE)
tval <- coe/std.Err
p.coef <- 2*pt(abs(tval),df2,lower.tail=FALSE)
result <- list(call=cl,coef=coe,std.Err=std.Err,tval=tval,p.coef=p.coef,F=F.stat,
               p.model=p.model,R2.within=drop(R2.within),R2.between=R2.between,
               R2.overall=R2.overall,N=N,n=n,vcov=vcov,drop.vars=drop.vars,
               residual=residual,df2=df2)
class(result)<-"bem"
return(result)
}

