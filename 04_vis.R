library(tidyverse)
setwd("C:/Panni/IPSDS/_BalanceThesis/Balance/data/plots")

#we prepared a county list taking region into account

CNTRYlistv2 <- c("FI","SE","NO","DK",
                 "FR","GB","IE","BE","NL",
                 "UA","BG","LT","EE",
                 "PL","HU","SK","CZ","SI",
                 "CH","DE","AT",
                 "ES","PT","IT","CY")
colors <- c("deepskyblue1","navyblue","royalblue1","skyblue",
            "yellowgreen","green","darkgreen","lawngreen","seagreen",
            "firebrick","darkred","tomato","orangered",
            "coral","darksalmon","orange","orange3","sienna1",
            "darkorchid","darkslateblue","purple3",
            "yellow","gold","khaki1","lightgoldenrod1")

#dp: difference, gap at the ends of the scale


#### Openness to change - RAW data
otctrends <- E %>% group_by(cntry,essround) %>% summarise(meanOTC =mean(OTC,na.rm=T))
v <- otctrends
colnames(v) <- c("cntry","essround","m")
vmin <- round(min(v$m,na.rm=T) - dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
vmax <- round(max(v$m,na.rm=T) + dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
par(mar=c(8.1, 2.1, 1.1, 0.5), mgp=c(2, 0.5, 0))
plot(v$essround,v$m,
     type="l", lwd=3,ylim=c(vmin,vmax), col="white",
     axes=F,
     ylab="",xlab="")
axis(side=1,at=seq(1,9,by=1),tck=-0.03,cex.axis=0.75)
axis(side=2,at=seq(vmin,vmax,by=0.1),tck=-0.02,cex.axis=0.75,las=1,pos=0.7)
for (i in 1:25) {
  lines(subset(v,cntry==CNTRYlistv2[i])$essround,
        subset(v,cntry==CNTRYlistv2[i])$m, 
        type="l",lwd=3,col=colors[i])
}
legend("bottom",legend=CNTRYlistv2,col=colors,lty=1,
       lwd=3,bty="n",ncol=8,xpd=T,inset=c(0,-0.39))
dev.copy(jpeg,"OTCtrends.jpeg",width=640,height=400, quality=100)
dev.off()
v$m <- round(v$m,3)
write.table(v,"otctrends.txt")


#### Openness to change - ipsatized
ipotctrends <- E %>% group_by(cntry,essround) %>% summarise(meanOTC =mean(ipOTC,na.rm=T))
v <- ipotctrends
colnames(v) <- c("cntry","essround","m")
vmin <- round(min(v$m,na.rm=T) - dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
vmax <- round(max(v$m,na.rm=T) + dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
par(mar=c(8.1, 2.1, 1.1, 0.5), mgp=c(2, 0.5, 0))
plot(v$essround,v$m,
     type="l", lwd=3,ylim=c(vmin,vmax), col="white",
     axes=F,
     ylab="",xlab="")
axis(side=1,at=seq(1,9,by=1),tck=-0.03,cex.axis=0.75)
axis(side=2,at=round(seq(vmin,vmax,by=0.1),1),tck=-0.02,cex.axis=0.75,las=1,pos=0.7)
for (i in 1:25) {
  lines(subset(v,cntry==CNTRYlistv2[i])$essround,
        subset(v,cntry==CNTRYlistv2[i])$m, 
        type="l",lwd=3,col=colors[i])
}
legend("bottom",legend=CNTRYlistv2,col=colors,lty=1,
       lwd=3,bty="n",ncol=8,xpd=T,inset=c(0,-0.39))
dev.copy(jpeg,"ipOTCtrends.jpeg",width=640,height=400, quality=100)
dev.off()
v$m <- round(v$m,3)
write.table(v,"ipotctrends.txt")



#### Conservation - RAW data
contrends <- E %>% group_by(cntry,essround) %>% summarise(meanCON =mean(CON,na.rm=T))
v <- contrends
colnames(v) <- c("cntry","essround","m")
vmin <- round(min(v$m,na.rm=T) - dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
vmax <- round(max(v$m,na.rm=T) + dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
par(mar=c(8.1, 2.1, 1.1, 0.5), mgp=c(2, 0.5, 0))
plot(v$essround,v$m,
     type="l", lwd=3,ylim=c(vmin,vmax), col="white",
     axes=F,
     ylab="",xlab="")
axis(side=1,at=seq(1,9,by=1),tck=-0.03,cex.axis=0.75)
axis(side=2,at=seq(vmin,vmax,by=0.1),tck=-0.02,cex.axis=0.75,las=1,pos=0.7)
for (i in 1:25) {
  lines(subset(v,cntry==CNTRYlistv2[i])$essround,
        subset(v,cntry==CNTRYlistv2[i])$m, 
        type="l",lwd=3,col=colors[i])
}
legend("bottom",legend=CNTRYlistv2,col=colors,lty=1,
       lwd=3,bty="n",ncol=8,xpd=T,inset=c(0,-0.39))
dev.copy(jpeg,"CONtrends.jpeg",width=640,height=400, quality=100)
dev.off()
v$m <- round(v$m,3)
write.table(v,"contrends.txt")


#### Consrvation - ipsatized
ipcontrends <- E %>% group_by(cntry,essround) %>% summarise(meanCON =mean(ipCON,na.rm=T))
v <- ipcontrends
colnames(v) <- c("cntry","essround","m")
vmin <- round(min(v$m,na.rm=T) - dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
vmax <- round(max(v$m,na.rm=T) + dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
par(mar=c(8.1, 2.1, 1.1, 0.5), mgp=c(2, 0.5, 0))
plot(v$essround,v$m,
     type="l", lwd=3,ylim=c(vmin,vmax), col="white",
     axes=F,
     ylab="",xlab="")
axis(side=1,at=seq(1,9,by=1),tck=-0.03,cex.axis=0.75)
axis(side=2,at=round(seq(vmin,vmax,by=0.1),1),tck=-0.02,cex.axis=0.75,las=1,pos=0.7)
for (i in 1:25) {
  lines(subset(v,cntry==CNTRYlistv2[i])$essround,
        subset(v,cntry==CNTRYlistv2[i])$m, 
        type="l",lwd=3,col=colors[i])
}
legend("bottom",legend=CNTRYlistv2,col=colors,lty=1,
       lwd=3,bty="n",ncol=8,xpd=T,inset=c(0,-0.39))
dev.copy(jpeg,"ipCONtrends.jpeg",width=640,height=400, quality=100)
dev.off()
v$m <- round(v$m,3)
write.table(v,"ipcontrends.txt")


#### OPENNESS DIMENSION 
otrends <- E %>% group_by(cntry,essround) %>% summarise(meanO =mean(O,na.rm=T))
v <- otrends
colnames(v) <- c("cntry","essround","m")
vmin <- round(min(v$m,na.rm=T) - dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
vmax <- round(max(v$m,na.rm=T) + dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
par(mar=c(8.1, 2.1, 1.1, 0.5), mgp=c(2, 0.5, 0))
plot(v$essround,v$m,
     type="l", lwd=3,ylim=c(vmin,vmax), col="white",
     axes=F,
     ylab="",xlab="")
axis(side=1,at=seq(1,9,by=1),tck=-0.03,cex.axis=0.75)
axis(side=2,at=round(seq(vmin,vmax,by=0.1),1),tck=-0.02,cex.axis=0.75,las=1,pos=0.7)
for (i in 1:25) {
  lines(subset(v,cntry==CNTRYlistv2[i])$essround,
        subset(v,cntry==CNTRYlistv2[i])$m, 
        type="l",lwd=3,col=colors[i])
}
legend("bottom",legend=CNTRYlistv2,col=colors,lty=1,
       lwd=3,bty="n",ncol=8,xpd=T,inset=c(0,-0.39))
dev.copy(jpeg,"Otrends.jpeg",width=640,height=400, quality=100)
dev.off()
v$m <- round(v$m,3)
write.table(v,"otrends.txt")



#### Self-enhancement - RAW data
sentrends <- E %>% group_by(cntry,essround) %>% summarise(meanSEN =mean(SEN,na.rm=T))
v <- sentrends
colnames(v) <- c("cntry","essround","m")
vmin <- round(min(v$m,na.rm=T) - dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
vmax <- round(max(v$m,na.rm=T) + dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
par(mar=c(8.1, 2.1, 1.1, 0.5), mgp=c(2, 0.5, 0))
plot(v$essround,v$m,
     type="l", lwd=3,ylim=c(vmin,vmax), col="white",
     axes=F,
     ylab="",xlab="")
axis(side=1,at=seq(1,9,by=1),tck=-0.03,cex.axis=0.75)
axis(side=2,at=seq(vmin,vmax,by=0.1),tck=-0.02,cex.axis=0.75,las=1,pos=0.7)
for (i in 1:25) {
  lines(subset(v,cntry==CNTRYlistv2[i])$essround,
        subset(v,cntry==CNTRYlistv2[i])$m, 
        type="l",lwd=3,col=colors[i])
}
legend("bottom",legend=CNTRYlistv2,col=colors,lty=1,
       lwd=3,bty="n",ncol=8,xpd=T,inset=c(0,-0.39))
dev.copy(jpeg,"SENtrends.jpeg",width=640,height=400, quality=100)
dev.off()
v$m <- round(v$m,3)
write.table(v,"sentrends.txt")


#### self-enhancement - ipsatized
ipsentrends <- E %>% group_by(cntry,essround) %>% summarise(meanSEN =mean(ipSEN,na.rm=T))
v <- ipsentrends
colnames(v) <- c("cntry","essround","m")
vmin <- round(min(v$m,na.rm=T) - dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
vmax <- round(max(v$m,na.rm=T) + dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
par(mar=c(8.1, 2.1, 1.1, 0.5), mgp=c(2, 0.5, 0))
plot(v$essround,v$m,
     type="l", lwd=3,ylim=c(vmin,vmax), col="white",
     axes=F,
     ylab="",xlab="")
axis(side=1,at=seq(1,9,by=1),tck=-0.03,cex.axis=0.75)
axis(side=2,at=round(seq(vmin,vmax,by=0.1),1),tck=-0.02,cex.axis=0.75,las=1,pos=0.7)
for (i in 1:25) {
  lines(subset(v,cntry==CNTRYlistv2[i])$essround,
        subset(v,cntry==CNTRYlistv2[i])$m, 
        type="l",lwd=3,col=colors[i])
}
legend("bottom",legend=CNTRYlistv2,col=colors,lty=1,
       lwd=3,bty="n",ncol=8,xpd=T,inset=c(0,-0.39))
dev.copy(jpeg,"ipSENtrends.jpeg",width=640,height=400, quality=100)
dev.off()
v$m <- round(v$m,3)
write.table(v,"ipsentrends.txt")



#### self-tanscendence - RAW data
strtrends <- E %>% group_by(cntry,essround) %>% summarise(meanSTR =mean(STR,na.rm=T))
v <- strtrends
colnames(v) <- c("cntry","essround","m")
vmin <- round(min(v$m,na.rm=T) - dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
vmax <- round(max(v$m,na.rm=T) + dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
par(mar=c(8.1, 2.1, 1.1, 0.5), mgp=c(2, 0.5, 0))
plot(v$essround,v$m,
     type="l", lwd=3,ylim=c(vmin,vmax), col="white",
     axes=F,
     ylab="",xlab="")
axis(side=1,at=seq(1,9,by=1),tck=-0.03,cex.axis=0.75)
axis(side=2,at=seq(vmin,vmax,by=0.1),tck=-0.02,cex.axis=0.75,las=1,pos=0.7)
for (i in 1:25) {
  lines(subset(v,cntry==CNTRYlistv2[i])$essround,
        subset(v,cntry==CNTRYlistv2[i])$m, 
        type="l",lwd=3,col=colors[i])
}
legend("bottom",legend=CNTRYlistv2,col=colors,lty=1,
       lwd=3,bty="n",ncol=8,xpd=T,inset=c(0,-0.39))
dev.copy(jpeg,"STRtrends.jpeg",width=640,height=400, quality=100)
dev.off()
v$m <- round(v$m,3)
write.table(v,"strtrends.txt")


#### Self-transcendence - ipsatized
ipstrtrends <- E %>% group_by(cntry,essround) %>% summarise(meanSTR =mean(ipSTR,na.rm=T))
v <- ipstrtrends
colnames(v) <- c("cntry","essround","m")
vmin <- round(min(v$m,na.rm=T) - dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
vmax <- round(max(v$m,na.rm=T) + dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
par(mar=c(8.1, 2.1, 1.1, 0.5), mgp=c(2, 0.5, 0))
plot(v$essround,v$m,
     type="l", lwd=3,ylim=c(vmin,vmax), col="white",
     axes=F,
     ylab="",xlab="")
axis(side=1,at=seq(1,9,by=1),tck=-0.03,cex.axis=0.75)
axis(side=2,at=round(seq(vmin,vmax,by=0.1),1),tck=-0.02,cex.axis=0.75,las=1,pos=0.7)
for (i in 1:25) {
  lines(subset(v,cntry==CNTRYlistv2[i])$essround,
        subset(v,cntry==CNTRYlistv2[i])$m, 
        type="l",lwd=3,col=colors[i])
}
legend("bottom",legend=CNTRYlistv2,col=colors,lty=1,
       lwd=3,bty="n",ncol=8,xpd=T,inset=c(0,-0.39))
dev.copy(jpeg,"ipSTRtrends.jpeg",width=640,height=400, quality=100)
dev.off()
v$m <- round(v$m,3)
write.table(v,"ipstrtrends.txt")


#### Selftranscendence vs self-enhancement dimension
strends <- E %>% group_by(cntry,essround) %>% summarise(meanS =mean(S,na.rm=T))
v <- strends
colnames(v) <- c("cntry","essround","m")
vmin <- round(min(v$m,na.rm=T) - dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
vmax <- round(max(v$m,na.rm=T) + dp*(max(v$m,na.rm=T)-min(v$m,na.rm=T)),1)
par(mar=c(8.1, 2.1, 1.1, 0.5), mgp=c(2, 0.5, 0))
plot(v$essround,v$m,
     type="l", lwd=3,ylim=c(vmin,vmax), col="white",
     axes=F,
     ylab="",xlab="")
axis(side=1,at=seq(1,9,by=1),tck=-0.03,cex.axis=0.75)
axis(side=2,at=round(seq(vmin,vmax,by=0.1),1),tck=-0.02,cex.axis=0.75,las=1,pos=0.7)
for (i in 1:25) {
  lines(subset(v,cntry==CNTRYlistv2[i])$essround,
        subset(v,cntry==CNTRYlistv2[i])$m, 
        type="l",lwd=3,col=colors[i])
}
legend("bottom",legend=CNTRYlistv2,col=colors,lty=1,
       lwd=3,bty="n",ncol=8,xpd=T,inset=c(0,-0.39))
dev.copy(jpeg,"Strends.jpeg",width=640,height=400, quality=100)
dev.off()
v$m <- round(v$m,3)
write.table(v,"strends.txt")
