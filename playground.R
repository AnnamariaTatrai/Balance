FR <- E[E$cntry=="FR",]
LT <- E[E$cntry=="LT",]

hist(FR$S)
hist(LT$S)

LT$Sbalanced <- 0
LT$Sbalanced[LT$S > -0.5 & LT$S < 0.5] <- 1

table(LT$Sbalanced,useNA = "always")

table(LT$happy,useNA = "always")

LT$happy[LT$happy==88] <- NA
LT$happy[LT$happy==99] <- NA
LT$happy[LT$happy==77] <- NA


t.test(happy ~Sbalanced, data = LT)

FR$happy[FR$happy==88] <- NA
FR$happy[FR$happy==99] <- NA
FR$happy[FR$happy==77] <- NA

### balance: middle of histogram
FR$SbalancedV1 <- 0
FR$SbalancedV1[FR$S > 1.5 & FR$S < 2.5] <- 1




  
### balance: taking absolute values
FR$SbalancedV2 <- 0
FR$SbalancedV2[FR$S > -0.5 & FR$S < 0.5] <- 1

tale(S)  

t.test(happy ~SbalancedV1, data = FR)
t.test(happy ~SbalancedV2, data = FR)

table(FR$SbalancedV2,useNA="always")

sd(FR$S,na.rm=T)
mean(FR$S,na.rm=T)

FR$SbalancedV3 <- 0
FR$SbalancedV3[] <- 1

# FR vs LT
FRLT <- rbind(FR,LT)

FRLT$happy[FRLT$happy==88] <- NA
FRLT$happy[FRLT$happy==99] <- NA
FRLT$happy[FRLT$happy==77] <- NA


t.test(happy ~ cntry, data=FRLT)
