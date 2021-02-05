library(tidyverse)
setwd("C:/Panni/IPSDS/_BalanceThesis/Balance/data/plots")

#we prepared a county list taking region into account

CNTRYlistv2 <- c("FI","SE","NO","DK",
                 "FR","GB","IE","BE","NL",
                 "UA","BG","LT","EE",
                 "PL","HU","SK","CZ","SI",
                 "CH","DE","AT",
                 "ES","PT","IT","CY")
colors <- c("navy","blue3"," dodgerblue1","steeleblue2",
            "darkgreen","forrestgreen","limegreen","lawngreen","mediumseagreen",
            "firebrick4","firebrick3"," firebrick1","orangered1",
            "lightcyan4","lightcyan3","lightcyan2","azure4","azure3",
            "magenta4","magenta2","orchid2",
            "yellow","gold1"," goldenrod2"," darkorange")

#gS <- c("FI","SE","NO","DK")         #Scandinavian countries
#gW <- c(„FR","GB","IE","BE","NL")    #west European countries
#gE <- c(„"UA","BG","LT","EE")        #east European countries
#gCE <- c("PL","HU","SK","CZ","SI")   #central east European countries
#gC <- c("CH","DE","AT")              #central European countries
#gM <-c("ES","PT","IT","CY")          #Mediterranean countries


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


#write.table(otctrends,"otctrends.txt")



par(mar=c(8.1, 2.1, 1.1, 0.5), mgp=c(2, 0.5, 0))
plot(ipotctrends$essround,ipotctrends$meanOTC,
     type="l", lwd=3,ylim=c(-0.65,0.15), col="white",
     axes=F,
     ylab="",xlab="")
axis(side=1,at=seq(1,9,by=1),tck=-0.03,cex.axis=0.75)
axis(side=2,at=seq(-0.65,0.15,by=0.1),tck=-0.02,cex.axis=0.75,las=1,pos=0.7)
for (i in 1:25) {
  lines(subset(ipotctrends,cntry==CNTRYlistv2[i])$essround,
        subset(ipotctrends,cntry==CNTRYlistv2[i])$meanOTC, 
        type="l",lwd=2,col=colors[i])
}
legend("bottom",legend=CNTRYlistv2,col=colors,lty=1,
       lwd=3,bty="n",ncol=8,xpd=T,inset=c(0,-0.39))
