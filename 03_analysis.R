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
#caotc
caotc <- reshape(data=caotc,timevar =  "round",idvar=c("cntry"),direction="wide")
#setwd("C:/Panni/IPSDS/_BalanceThesis/Balance/data/plots")
#write.table(fn,"caotc.txt",row.names = F)


#### conservation
# preparing an empty dataframe
cacon <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(cacon) <- c("cntry","round","ca")
# C stands for country
# r stands for round
# 
for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i21con])$total$raw_alpha,2)
    cacon <- rbind(cacon,c(CNTRYlistv1[C],r,a))}  
}  
cacon <- cacon[is.na(cacon$cntry)==F,]
#caotc
cacon <- reshape(data=cacon,timevar =  "round",idvar=c("cntry"),direction="wide")

#### self-stranscendence
# preparing an empty dataframe
castr <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(castr) <- c("cntry","round","ca")
# C stands for country
# r stands for round
# 
for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i21str])$total$raw_alpha,2)
    castr <- rbind(castr,c(CNTRYlistv1[C],r,a))}  
}  
castr <- castr[is.na(castr$cntry)==F,]
#caotc
castr <- reshape(data=castr,timevar =  "round",idvar=c("cntry"),direction="wide")


#### self-enhancement
# preparing an empty dataframe
casen <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(casen) <- c("cntry","round","ca")
# C stands for country
# r stands for round
# 
for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i21sen])$total$raw_alpha,2)
    casen <- rbind(casen,c(CNTRYlistv1[C],r,a))}  
}  
casen <- casen[is.na(casen$cntry)==F,]
#caotc
casen <- reshape(data=casen,timevar =  "round",idvar=c("cntry"),direction="wide")
