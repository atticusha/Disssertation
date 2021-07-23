# Ind v ecnj

##Bivariate
-----------
### II (result:NONE)
paste(rownames(II.Uni.Sig), collapse =" + ")
II.Bivariate <- nominal(. ~ Actor.3Pl + Actor.4Sg + Actor.4Pl + PV.ki + PV.ohci + PV.ka, data = subset(AWive, II))
subset(II.Bivariate$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
###FINAL SET: Actor.3Pl + Actor.4Sg + Actor.4Pl + PV.ki + PV.ohci + PV.ka


### AI (result: NONE)
paste(rownames(AI.Uni.Sig), collapse =" + ")
AI.Bivariate <- nominal(. ~ RdplW + Actor.3Sg + Actor.4Sg.Pl + Actor.1Pl + Actor.3Pl + PV.ki + PV.pe + PV.wi + PV.isi + PV.nitawi + PV.ohci + PV.ati + PV.ka + PV.kah + PV.mosci + PV.ta + PV.papa + PV.kisi + PV.ka_ki + PV.oh + VAI2 + VAI1 + VAI7 + VAI5 + VAI12 + VAI3 + NA6 + NA1 + NI12, data = subset(AWive, AI))
subset(AI.Bivariate$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
###FINAL SET: RdplW + Actor.3Sg + Actor.4Sg.Pl + Actor.1Pl + Actor.3Pl + PV.ki + PV.pe + PV.wi + PV.isi + PV.nitawi + PV.ohci + PV.ati + PV.ka + PV.kah + PV.mosci + PV.ta + PV.papa + PV.kisi + PV.ka_ki + PV.oh + VAI2 + VAI1 + VAI7 + VAI5 + VAI12 + VAI3 + NA6 + NA1 + NI12

### TI (result: VTI1, VTI2)
paste(rownames(TI.Uni.Sig), collapse =" + ")
TI.Bivariate <- nominal(. ~ RdplS + RdplW + Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.1Pl + Actor.2Pl + Actor.3Pl + PV.ki + PV.isi + PV.ka + PV.kah + PV.oh + PV.ta + VTI2 + VTI1 + NI13 + NI14 + NI8, data = subset(AWive, TI))
subset(TI.Bivariate$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
###FINAL SET: RdplS + RdplW + Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.1Pl + Actor.2Pl + Actor.3Pl + PV.ki + PV.isi + PV.ka + PV.kah + PV.oh + PV.ta + VTI2 + NI13 + NI14 + NI8


### TA (result: NONE)
paste(rownames(TA.Uni.Sig), collapse =" + ")
TA.Bivariate <- nominal(. ~ Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.4Sg.Pl + Actor.1Pl + Actor.2Pl + Actor.3Pl + Goal.3SgO + Goal.4Sg.PlO + Goal.1PlO + PV.ki + PV.pe + PV.wi + PV.isi + PV.ka + PV.ohci + PV.kakwe + PV.ka_ki + VTA7 + VTA2 + VTA4 + NA6 + NDA1 + NA1, data = subset(AWive, TA))
subset(TA.Bivariate$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
###FINAL SET: Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.4Sg.Pl + Actor.1Pl + Actor.2Pl + Actor.3Pl + Goal.3SgO + Goal.4Sg.PlO + Goal.1PlO + PV.ki + PV.pe + PV.wi + PV.isi + PV.ka + PV.ohci + PV.kakwe + PV.ka_ki + VTA7 + VTA2 + VTA4 + NA6 + NDA1 + NA1
#==========================================================================================================  

# ecnj vs ka vs other

### II (result:NONE)
paste(rownames(II.Uni.Sig.cnj), collapse =" + ")
II.Bivariate.cnj <- nominal(. ~ RdplS + Actor.3Sg + Actor.4Sg + PV.ki + PV.ati + VII2 + VII3 + NI12 + NI3 + NI9 + NI2, data = subset(AWCnj, II))
subset(II.Bivariate.cnj$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
###FINAL SET: RdplS + Actor.3Sg + Actor.4Sg + PV.ki + PV.ati + VII2 + VII3 + NI12 + NI3 + NI9 + NI2


### AI (result: NONE)
paste(rownames(AI.Uni.Sig.cnj), collapse =" + ")
AI.Bivariate.cnj <- nominal(. ~ RdplW + Actor.2Sg + Actor.3Sg + Actor.1Pl + Actor.12Pl + Actor.2Pl + PV.ki + PV.wi + PV.isi + PV.ati + PV.ohci + PV.kah + PV.kakwe + PV.wah + VAI2 + VAI1 + VAI12 + VAI5 + VAI6 + VAI3 + NA6 + NDA1, data = subset(AWCnj, AI))
subset(AI.Bivariate.cnj$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
###FINAL SET: RdplW + Actor.2Sg + Actor.3Sg + Actor.1Pl + Actor.12Pl + Actor.2Pl + PV.ki + PV.wi + PV.isi + PV.ati + PV.ohci + PV.kah + PV.kakwe + PV.wah + VAI2 + VAI1 + VAI12 + VAI5 + VAI6 + VAI3 + NA6 + NDA1

### TI (result: VTI1, VTI2)
paste(rownames(TI.Uni.Sig.cnj), collapse =" + ")
TI.Bivariate.cnj <- nominal(. ~ Actor.2Sg + Actor.4Sg.Pl + Actor.1Pl + Actor.12Pl + Actor.2Pl + PV.ki + PV.wi + PV.ohci + PV.nohte + PV.kah + VTI2 + VTI1 + VTI5 + NI12 + NI2 + NA6, data = subset(AWCnj, TI))
subset(TI.Bivariate$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
###VTI1 and 2 covaried, so removed VTI1FINAL SET: Actor.2Sg + Actor.4Sg.Pl + Actor.1Pl + Actor.12Pl + Actor.2Pl + PV.ki + PV.wi + PV.ohci + PV.nohte + PV.kah + VTI2 + VTI5 + NI12 + NI2 + NA6

### TA (result: NONE)
paste(rownames(TA.Uni.Sig.cnj), collapse =" + ")
TA.Bivariate.cnj <- nominal(. ~ Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.1Pl + Actor.12Pl + Actor.2Pl + Actor.3Pl + Goal.1SgO + Goal.2SgO + Goal.3SgO + Goal.4Sg.PlO + Goal.12PlO + Goal.2PlO + PV.ki + PV.pe + PV.wi + PV.isi + PV.ohci + PV.kah + VTA7 + VTA1 + VTA4 + NA6 + NDA2, data = subset(AWCnj, TA))
subset(TA.Bivariate$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
###FINAL SET: Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.1Pl + Actor.12Pl + Actor.2Pl + Actor.3Pl + Goal.1SgO + Goal.2SgO + Goal.3SgO + Goal.4Sg.PlO + Goal.12PlO + Goal.2PlO + PV.ki + PV.pe + PV.wi + PV.isi + PV.ohci + PV.kah + VTA7 + VTA1 + VTA4 + NA6 + NDA2
  
  