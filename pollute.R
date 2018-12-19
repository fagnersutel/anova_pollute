poluicao <- read.table("/Users/fsmoura/Desktop/Pollute.txt", header=T, sep="\t")
par(mfrow=c(1,3))
plot(Pollution~Industry, data=poluicao)
plot(Pollution~Temp, data=poluicao)
plot(Pollution~Wind, data=poluicao)
par(mfrow=c(1,1))
pol.m1 <- lm(Pollution~Industry, data=poluicao)
plot(Pollution~Industry, data=poluicao)
abline(pol.m1, col="blue")
abline(lm(Pollution~Industry, data=poluicao, subset=Industry<max(Industry)), col="red")

anova(pol.m1)
pol.m0 <- lm(Pollution~1, data=poluicao)
##compare:
anova(pol.m0,pol.m1)
anova(pol.m1)
summary(pol.m0)
mean(poluicao$Pollution)
sd(poluicao$Pollution)


plot(Pollution~Industry, data=poluicao)
abline(pol.m1, col="blue")
abline(h=mean(poluicao$Pollution),col="red")

pol.m2 <- lm(Pollution~Industry+Temp, data=poluicao)
anova(pol.m1,pol.m2)


pol.m3 <- update(pol.m2,.~.+Wind)
anova(pol.m2,pol.m3)
anova(pol.m3)

par(mfrow=c(2,2))
plot(pol.m2)
par(mfrow=c(1,1))



poluicao[41,c(1:3,5)]
par(mfrow=c(1,2))
plot(Pollution~Industry, data=poluicao)
points(Pollution~Industry, data=poluicao[41,], col="red", pch=19)
plot(Pollution~Temp, data=poluicao)
points(Pollution~Temp, data=poluicao[41,], col="red", pch=19)
par(mfrow=c(1,1))
summary(pol.m2)



library(lattice)
temp.shingle <- equal.count(poluicao$Temp,number=4)
summary(temp.shingle)


xyplot(Pollution~Industry|temp.shingle,data=poluicao,
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.abline(lm(y~x),...)
       }
)


xyplot(Pollution~Industry|equal.count(poluicao$Wind,number=4),data=poluicao,
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.abline(lm(y~x),...)
       }
)
