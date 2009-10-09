"fem" <-
function(formula,id,data,...)
# formular has intercept
{
#deal with the formula
cl <- match.call()
mf <- match.call(expand.dots = FALSE)
m<- match(c("formula", "id", "data"), names(mf), 0)
mf <- mf[c(1, m)]
mf[[1]]<-as.name("model.frame")
mf <- eval(mf, parent.frame())
mt <- attr(mf, "terms")
X <- model.matrix(mt,mf);  Y<- model.response(mf); id <- model.extract(mf,"id")
sst<-(length(Y)-1)*var(Y)
r2c<-cor(qr.fitted(qr(X),Y),Y)^2
#demean
xd<-dmean(X,id)
yd<-dmean(as.matrix(Y),id)
Xbat <- xd$xdm
Ybat <- yd$xdm 
##add the total mean
xx<-matrix(colMeans(X),ncol=NCOL(X),nrow=NROW(X),byrow=T) #total mean for each variables.
Xbat <- Xbat + xx
Ybat <- Ybat+mean(Y)
#fit the model
fm <- lm.fit(Xbat,Ybat) 
coe.pri <- coef(fm)
noNA <- !is.na(coe.pri)
coe<-coe.pri[noNA]
residual<-resid(fm)
sse<-sum(residual^2)
fit<-fitted(fm)
#R2
R2.within<-cor(Ybat,fit)^2     #within R2 from the mean-deviated regression.
R2.between <- cor(xd$xm[,noNA] %*% coe,yd$xm,use="pairwise.complete.obs")^2
R2.overall <- cor(X[,noNA] %*% coe,Y,use="pairwise.complete.obs")^2
#
n <- dim(xd$xm)[1] # number of units
N <- length(Y)  #number of total obs
K <- length(coe) -1 # number of variables( exclude intercept)
df.model <- N-n-K
sigma.e<-sse/df.model
rss<-sum((fit-mean(Ybat))^2)
vcov<-sigma.e*solve(crossprod(Xbat[,noNA]))
std.Err<-matrix(sqrt(diag(vcov)),ncol=1)
tval<-coe/std.Err
p.coef <-2*pt(abs(tval),df.model,lower.tail=FALSE)
##F test
fsta<-(rss/K)/sigma.e
#f-test:if ai is significant.
R2=1-sse/sst      
fsta.2 <-( (R2-r2c) / (n-1) ) / ( (1-R2) / df.model)
p.model <-pf(fsta,K,df.model,lower.tail = FALSE)  #df of P(model):K,N-n-K
P.ui <-pf(fsta.2,n-1,df.model,lower.tail = FALSE) #df of P(ui=0):n-1,N-n-K
result <- list(call=cl,coef=coe,std.Err=std.Err,tval=tval,p.coef=p.coef,F.model=fsta,
               p.model=p.model,F.test.ui=fsta.2,P.ui=P.ui,R2.within=drop(R2.within),
               R2.between=R2.between,R2.overall=R2.overall,N=N,n=n,vcov=vcov,
               residual=residual,id=id,df.model=df.model,xd=xd,yd=yd,drop.var=noNA,
               Xbat=Xbat,Ybat=Ybat)
class(result) <-"fem"
return(result)           
}

