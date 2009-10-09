"dmean" <-
function(x,id,theta=1)
#id is a variable in x,x is matrix or data.frame
{
id<-as.factor(id)
xm<- apply(x,2,function(y,z) tapply(y,z, mean), z=id)
xdm<- x[] <- x-theta*xm[id,]
re<-list(xm=xm, xdm=xdm)
}

