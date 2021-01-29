library(psych)
i21otc <- c("impdiff", "ipadvnt", "ipcrtiv", "impfree", "impfun", "ipgdtim")
# conservation
i21con <- c("impsafe", "ipstrgv", "ipfrule", "ipbhprp", "ipmodst", "imptrad")
# self-enhancement
i21sen <- c("imprich", "iprspot", "ipshabt", "ipsuces")
# self-transcendence
i21str <- c("ipeqopt", "ipudrst", "impenv", "iphlppl", "iplylfr")


# country and year and cronbach's alpha
#### OPENNESS TO CHANGE
# preparing an empty dataframe
caotc <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(caotc) <- c("cntry","round","ca")
# C stands for country
# r stands for round
# 
for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i21otc])$total$raw_alpha,2)
    caotc <- rbind(caotc,c(CNTRYlistv1[C],r,a))}  
  }  
caotc <- caotc[is.na(caotc$cntry)==F,]
caotc

caotc <- reshape(data=caotc,timevar =  "round",idvar=c("cntry"),direction="wide")
setwd("C:/Panni/IPSDS/_BalanceThesis/Balance/data/plots")
write.table(fn,"caotc.txt",row.names = F)
