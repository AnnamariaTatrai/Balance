setwd
CNTRYlistv2 <- c("AT","BE","BG","CH","CY",
                 "CZ","DE","DK","EE","ES",
                 "FI","FR","GB","HU","IE",
                 "IT","LT","NL","NO","PL",
                 "PT","SE","SI","SK","UA")
#rect(2007.8,0,2013.2,0.4,border=F,col="grey87")
colors <- c("blue","blue4","red4","azure4","burlywood4",
            "chartreuse3","coral","coral4","darkcyan","darkgrey",
            "darkred","darkviolet","black","red","orchid",
            "orchid4","gray36","olivedrab", "darkorange4","wheat",
            "yellow3","grey64","greenyellow","green","gold2")


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


#### Openness to change

ipotctrends <- E %>% group_by(cntry,essround) %>% summarise(meanOTC =mean(ipOTC,na.rm=T))
otctrends
ipotctrends

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
