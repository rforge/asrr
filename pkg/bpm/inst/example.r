#example
data(jtrain1)
(fm.fem <- fem(log(scrap)~d88+d89+grant+grant.1,id=fcode,data=jtrain1)) #from wood p430
(fm.bem <- bem(log(scrap)~d88+d89+grant+grant.1,id=fcode,data=jtrain1))

library(nlme)
summary(gls(log(scrap)~d88+d89+grant+grant.1,cor=corCompSymm(form=~1|fcode),data=jtrain1,na.action="na.exclude"))
