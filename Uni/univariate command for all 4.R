paste(paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("PV.[^GL]", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("VTA", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("^N.*[0-9]$", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")
TA.Univariate.cnj <- nominal(OrderType ~ RdplS + RdplW + Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.4Sg.Pl + Actor.1Pl + Actor.12Pl + Actor.2Pl + Actor.3Pl + Goal.1SgO + Goal.2SgO + Goal.3SgO + Goal.4Sg.PlO + Goal.1PlO + Goal.12PlO + Goal.2PlO + Goal.3PlO + PV.ki + PV.pe + PV.wi + PV.isi + PV.nitawi + PV.kakwe + PV.ohci + PV.kah + PV.ati + PV.misi + PV.wah + PV.kisi + PV.nohte + PV.pimi + PV.miyo + VTA2 + VTA7 + VTA1 + VTA4 + VTA5 + VTA3 + NA6 + NDA1 + NA1 + NA3 + NA7 + NDA2 + NA8, data = subset(AWCnj, TA))
### Check cochrane condiTAon; step 1) remove things that aren't about 1
for (i in TA.Univariate.cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### none, move to step 2) make sure 80% of expected are <5
for (i in TA.Univariate.cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
#### 21/(184) == 11.4%, under 20% threshold

###Write to file
sink(file = 'TAUni-cnj.txt')
print(summary(TA.Univariate.cnj), max.print = NA)
sink(NULL)
###Make significance table
TA.Uni.Sig.cnj<- subset(summary(TA.Univariate.cnj)$sumry.table, alpha.X2 <0.05)

sink(file = 'TAUniSig-cnj.txt')
print(TA.Uni.Sig.cnj, max.print = NA)
sink(NULL)