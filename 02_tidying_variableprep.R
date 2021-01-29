library(tidyverse) 

rm(list = ls())
setwd("C:/Panni/IPSDS/_BalanceThesis/Balance/data")
load("E.Rdata")

#21 items
i21 <- c("ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff", 
         "ipfrule", "ipudrst", "ipmodst", "ipgdtim", "impfree", "iphlppl", "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp", "iprspot", "iplylfr",
         "impenv", "imptrad", "impfun")


# computing the number of NA-s for every respondent in the 21 items
E$nmi <- rowSums(is.na(E[i21]))

# checking the distribution
apply(E[i21],2,table,useNA="always")

# recoding values 7 8 9 to NA
E <- E %>% mutate_at(all_of(i21), ~replace(.,.==7|.==8|.==9,NA))

# checking the distribution again #
apply(E[i21],2,table,useNA="always")

# computing the number of NA-s + invalid values (7,8,9) for every respondent in the 21 items
E$nmi789 <- rowSums(is.na(E[i21]))
table(E$nmi789,useNA = "always")

# cases to drop because of too high number of NA + invalid values (7,8,9)
E$dropM <- 0
E$dropM[E$nmi789>5] <- 1

table(E[E$dropM==1,]$cntry,E[E$dropM==1,]$essround)

round(table(E[E$dropM==1,]$cntry,E[E$dropM==1,]$essround) / table(E$cntry,E$essround),3)

# 1.2. Straightliners

# sraightliners
# variables showing how many times the respondent choose value 1,2,...6 
countcases <- function(x, n) { rowSums(x == n) }
E$i21_1 <- countcases(E[i21],1)
E$i21_2 <- countcases(E[i21],2)
E$i21_3 <- countcases(E[i21],3)
E$i21_4 <- countcases(E[i21],4)
E$i21_5 <- countcases(E[i21],5)
E$i21_6 <- countcases(E[i21],6)

# the maximum number of times using the same value
E$i21_16max <- pmax(E$i21_1,E$i21_2,E$i21_3,E$i21_4,E$i21_5,E$i21_6,na.rm=T)
table(E$i21_16max)

# cases to drop because of straightlining
E$dropS <- 0
E$dropS[E$i21_16max >16 ] <- 1

# straightliners: Frequency per country per round
table(E[E$dropS==1,]$cntry,E[E$dropS==1,]$essround)


##### rate
round(table(E[E$dropS==1,]$cntry,E[E$dropS==1,]$essround) / table(E$cntry,E$essround),3)

# 1.3. NA + straightliners together

#Frequency per country per ESSround

### cases to drop because of: high number of missing+ non valid values OR because of straightlining

table(E[E$dropS==1,]$cntry,E[E$dropS==1,]$essround) + table(E[E$dropM==1,]$cntry,E[E$dropM==1,]$essround)

### table with the final n-s
table(E[E$dropS==0&E$dropM==0,]$cntry,E[E$dropS==1&E$dropM==0,]$essround) 


#Rates per country per ESSround

round(table(E[E$dropS==1,]$cntry,E[E$dropS==1,]$essround) / table(E$cntry,E$essround),3) +
  round(table(E[E$dropM==1,]$cntry,E[E$dropM==1,]$essround) / table(E$cntry,E$essround),3)


## final n-s

table(E[E$dropS==0 & E$dropM==0,]$cntry,E[E$dropS==0 & E$dropM==0,]$essround)

fn <- as.data.frame(table(E[E$dropS==0 & E$dropM==0,]$cntry,E[E$dropS==0 & E$dropM==0,]$essround))
fn
setwd("C:/Panni/IPSDS/_BalanceThesis/Balance/data/plots")
write.table(fn,"FinalNs.txt")

# checking the distribution again
apply(E[i21],2,table,useNA="always")

E[E$dropS==1|E$dropM==1,i21] <- NA

# reversing scale

apply(E[i21],2,table,useNA="always")

E[i21] <- 7- E[i21]

# checking the distribution again
apply(E[i21],2,table,useNA="always")


# openness to change
i21otc <- c("impdiff", "ipadvnt", "ipcrtiv", "impfree", "impfun", "ipgdtim")
# conservation
i21con <- c("impsafe", "ipstrgv", "ipfrule", "ipbhprp", "ipmodst", "imptrad")
# self-enhancement
i21sen <- c("imprich", "iprspot", "ipshabt", "ipsuces")
# self-transcendence
i21str <- c("ipeqopt", "ipudrst", "impenv", "iphlppl", "iplylfr")

E$OTC <- rowMeans(E[i21otc],na.rm=T)
E$CON <- rowMeans(E[i21con],na.rm=T)
E$SEN <- rowMeans(E[i21sen],na.rm=T)
E$STR <- rowMeans(E[i21str],na.rm=T)

# opendim and selfdim
#opendim 'Max Conservation = -5, Max Openness to change = 5'
#selfdim 'Max Self-enhancement = -5, Max Self-trancendence = 5'.
E$O <- E$OTC - E$CON
E$S <- E$STR - E$SEN
# this comes from SCHWARTZ


E$POW = rowMeans(E[c("imprich", "iprspot")])
E$ACH = rowMeans(E[c("ipshabt", "ipsuces")])
E$HED = rowMeans(E[c("impfun", "ipgdtim")])
E$STI = rowMeans(E[c("impdiff", "ipadvnt")])
E$SEL  =rowMeans(E[c("ipcrtiv", "impfree")])
E$UNI = rowMeans(E[c("ipeqopt", "ipudrst", "impenv")])
E$BEN = rowMeans(E[c("iphlppl", "iplylfr")])
E$TRA = rowMeans(E[c("ipmodst", "imptrad")])
E$CON = rowMeans(E[c("ipbhprp", "ipfrule")])
E$SEC = rowMeans(E[c("impsafe", "ipstrgv")])

# APow 'Power – mean of raw rating'
# Aach 'Achievement – mean of raw rating'
# Ahed 'Hedonism – mean of raw rating'
# Asti 'Stimulation – mean of raw rating'
# Aself 'Self-Direction – mean of raw rating'
# Auni 'Universalism – mean of raw rating'
# ABen 'Benevolence – mean of raw rating'
# Atra 'Tradition – mean of raw rating'
# ACon 'Conformity – mean of raw rating'
# ASec 'Security – mean of raw rating'.

####### ipsatizing
# mrat 
E$mrat <- rowMeans(E[i21],na.rm=T)
#summary(E$mrat)


E$ipOTC <- rowMeans(E[i21otc],na.rm=T) - E$mrat
E$ipCON <- rowMeans(E[i21con],na.rm=T) - E$mrat
E$ipSEN <- rowMeans(E[i21sen],na.rm=T) - E$mrat
E$ipSTR <- rowMeans(E[i21str],na.rm=T) - E$mrat


E$ipPOW = rowMeans(E[c("imprich", "iprspot")]) - E$mrat
E$ipACH = rowMeans(E[c("ipshabt", "ipsuces")]) - E$mrat
E$ipHED = rowMeans(E[c("impfun", "ipgdtim")]) - E$mrat
E$ipSTI = rowMeans(E[c("impdiff", "ipadvnt")]) - E$mrat
E$ipSEL  =rowMeans(E[c("ipcrtiv", "impfree")])- E$mrat
E$ipUNI = rowMeans(E[c("ipeqopt", "ipudrst", "impenv")]) - E$mrat
E$ipBEN = rowMeans(E[c("iphlppl", "iplylfr")]) - E$mrat
E$ipTRA = rowMeans(E[c("ipmodst", "imptrad")]) - E$mrat
E$ipCON = rowMeans(E[c("ipbhprp", "ipfrule")]) - E$mrat
E$ipSEC = rowMeans(E[c("impsafe", "ipstrgv")]) - E$mrat




### country selection 
# countries participating all years
CNTRYlistv1 <- c("BE", "CH", "DE", "ES", "FI", "FR", "GB", "HU", "IE", "NL", "NO", "PL", "PT", "SE", "SI")
# european countries participating at least 5 years
# added: Austria, Bulgaria, Cyprus, Czechia,Denmark, Estonia, Italy, Lithuania, Slovakia, Ukraine
#CNTRYlistv2 <- c("AT","BE","BG" ,"CH", "CY","CZ", "DE","DK", "EE","ES", "FI", "FR", "GB", "HU", "IE","IT","LT", "NL", "NO", "PL", "PT", "SE", "SI","SK","UA")

#comment: 
#Country variables refer to: BE - Belgium, CH - Switzerland, DE - Germany, ES - Spain, FI - Finland, 
#FR - France, GB - United Kingdom, HU - Hungary, IE - Ireland, NL - Netherlands, NO - Norway, PL - Poland, 
#PT - Portugal, SE - Sweden, SI - Slovenia
# added: AT - Austria, BG - Bulgaria, CY - Cyprus, CZ - Czechia, DK - Denmark,
# EE - Estonia, IT - Italy, LT - Lithuania, SK - Slovakia, UA -Ukraine

# In one step I read in data + select relevant observations based on variabel cntry

#happy
E$happy
