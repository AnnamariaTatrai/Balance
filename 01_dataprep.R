#### Preparation of dataset

library(expss) #for .sav file import, 
library(tidyverse) #for everything else :)

# I went for expss package, since it returns data.frame and never concerts string variables to factors
# (factors + labels caused problems by adding waves since different waves have different labels, levels)

# 2032.01.15: we decided to use 


setwd("C:/Panni/IPSDS/_BalanceThesis/Balance/original")


w1 <- expss::read_spss("ESS1e06_6.sav")
w2 <- expss::read_spss("ESS2e03_6.sav")
w3 <- expss::read_spss("ESS3e03_7.sav")
w4 <- expss::read_spss("ESS4e04_5.sav")
w5 <- expss::read_spss("ESS5e03_4.sav")
w6 <- expss::read_spss("ESS6e02_4.sav")
w7 <- expss::read_spss("ESS7e02_2.sav")
w8 <- expss::read_spss("ESS8e02_2.sav")
w9 <- expss::read_spss("ESS9e03.sav")  

# selecting variables

ALLvars <- c("dweight", "pspwght", "pweight", "anweight", "nwspol", "netusoft", "netustm", "ppltrst", "pplfair", "pplhlp", "psppsgva", 
             "actrolga", "psppipla", "cptppola", "trstprl", "trstlgl", "trstplc", "trstplt", "trstprt", "trstep", "trstun", "vote", "lrscale", 
             "stflife", "stfeco", "stfgov", "stfdem", "stfedu", "stfhlth", "gincdif", "freehms", "hmsfmlsh", "hmsacld", "happy", "sclmeet", "inprdsc", 
             "sclact", "health", "hlthhmp", "atchctr", "atcherp", "rlgblg", "rlgdgr", "hhmmb", "gndr", "agea", "yrbrn", "rshpsts", "rshpsgb", 
             "marsts", "marstgb", "chldhhe", "domicil", "edulvlb", "eisced", "edulvlpb", "eiscedp", "crpdwk", "pdjobev", "emplrel", "tporgwk", 
             "hincsrca", "hincfel", "iincsrc", "atncrse", "region", "regunit", "ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff", 
             "ipfrule", "ipudrst", "ipmodst", "ipgdtim", "impfree", "iphlppl", "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp", "iprspot", "iplylfr",
             "impenv", "imptrad", "impfun", "name", "essround", "edition", "proddate", "idno", "inwtm", "prob", "stratum", "psu", "cntry")



#missing variables
setdiff(ALLvars,names(w1))
setdiff(ALLvars,names(w2))
setdiff(ALLvars,names(w3))
setdiff(ALLvars,names(w4))
setdiff(ALLvars,names(w5))
setdiff(ALLvars,names(w6))
setdiff(ALLvars,names(w7))
setdiff(ALLvars,names(w8))
setdiff(ALLvars,names(w9))

# selected colums : we keep all the variables we have from the list
w1 <- w1[intersect(names(w1),ALLvars)]
w2 <- w2[intersect(names(w2),ALLvars)]
w3 <- w3[intersect(names(w3),ALLvars)]
w4 <- w4[intersect(names(w4),ALLvars)]
w5 <- w5[intersect(names(w5),ALLvars)]
w6 <- w6[intersect(names(w6),ALLvars)]
w7 <- w7[intersect(names(w7),ALLvars)]
w8 <- w8[intersect(names(w8),ALLvars)]
w9 <- w9[intersect(names(w9),ALLvars)]

memory.limit(size=71730)
E <-  bind_rows(w1,w2,w3,w4,w5,w6,w7,w8,w9)
setwd("C:/Panni/IPSDS/_BalanceThesis/Balance/data")
save(E,file="E.Rdata")
```

