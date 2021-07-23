# Univariate
#3 sets: (1) ind cnj (2) Ind vs. eCnj, (3) eCnj vs. ka vs. other (4) ind e ka other
----------------
library(polytomous) 
## Ind v. eCnj
###=====================================================================================================
###=====================================================================================================
### II ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AW, II)[grep("PV.[^GL]", names(subset(AW, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AW, II)[grep("^II...*", names(subset(AW, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AW, II)[grep("^auto.*", names(subset(AW, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AW, II)[grep("actor$", names(subset(AW, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AW, II)[grep("goal$", names(subset(AW, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

II.Univariate.indimpcnj <- nominal(TopOrder ~  RdplS + RdplW + Actor.3Sg + Actor.3Pl + Actor.4Sg + Actor.4Pl + PV.e + PV.Time + PV.ki + PV.kaa + PV.Qual + PV.wi + PV.Move + PV.StartFinWantCan + PV.ka + PV.pe + PV.ati + PV.Position + II.natural.land + II.sense + II.weather + II.plural + auto.vii.1 + auto.vii.2 + auto.ni.3.actor + auto.vai.6 + auto.vii.3 + auto.vii.4 + auto.vii.5 + auto.ni.6.actor + auto.vai.1 + auto.vti.1 + auto.ni.7.actor + auto.vai.9 + auto.vii.6 + I.actor + Sg.actor + N.actor + NI.object.actor + auto.ni.3.actor + left.ACTOR.actor + ACTOR.right.actor + Dem.actor + Pron.actor + ôma.actor + kîkway.actor + Prox.actor + NI.nominal.actor + Pl.actor + Med.actor + auto.ni.6.actor + NI.place.actor + auto.ni.7.actor, data = subset(AW, II))

### Check cochrane condition; step 1) remove things that aren't about 1
for (i in II.Univariate.indimpcnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 80% of expected are <5
for (i in II.Univariate.indimpcnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
#### 58-43 = 15/(43*4) = 8.7%


###Write to file
sink(file = 'IIUni-indcnj.txt')
print(summary(II.Univariate.indimpcnj), max.print = NA)
sink(NULL)
###Make significance table
II.Uni.indimpcnj.Sig<- subset(summary(II.Univariate.indimpcnj)$sumry.table, alpha.X2 <0.05)

sink(file = 'IIUniSig-indcnj.txt')
print(II.Uni.indimpcnj.Sig, max.print = NA)
sink(NULL)






## Ind v. eCnj
###=====================================================================================================
###=====================================================================================================


### II ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWive, II)[grep("PV.[^GL]", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
paste(names(which(sort(colSums(subset(AWive, II)[grep("^II...*", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
paste(names(which(sort(colSums(subset(AWive, II)[grep("actor$", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
paste(names(which(sort(colSums(subset(AWive, II)[grep("goal$", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

II.Univariate.ive <- nominal(OrderType ~ RdplS + RdplW + Actor.3Sg + Actor.3Pl + Actor.4Sg + Actor.4Pl + PV.ki + PV.wi + PV.pe + PV.ati + PV.ka + II.natural.land + II.sense + II.weather + I.actor + Sg.actor + N.actor + NI.object.actor + auto.ni.3.actor + left.ACTOR.actor + ACTOR.right.actor + kîkway.actor + Dem.actor + Pron.actor + ôma.actor + NI.nominal.actor + Prox.actor + Pl.actor + Med.actor + auto.ni.6.actor + NI.place.actor + auto.ni.7.actor, data = subset(AWive, II))

### Check cochrane condition; step 1) remove things that aren't about 1
for (i in II.Univariate.ive$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 80% of expected are <5
for (i in II.Univariate.ive$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
#### 43-32 = 11/(32*4) = 8.6%


###Write to file
sink(file = 'IIUni-indve.txt')
print(summary(II.Univariate.ive), max.print = NA)
sink(NULL)
###Make significance table
II.Uni.ive.Sig<- subset(summary(II.Univariate.ive)$sumry.table, alpha.X2 <0.05)

sink(file = 'IIUniSig-indve.txt')
print(II.Uni.ive.Sig, max.print = NA)
sink(NULL)

### AI ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWive, AI)[grep("PV.[^GL]", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("^AI...*", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("actor$", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("goal$", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")
AI.Univariate.ive <- nominal(OrderType ~ RdplS + RdplW + Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.4Sg.Pl + Actor.1Pl + Actor.12Pl + Actor.2Pl + Actor.3Pl + PV.ki + PV.pe + PV.wi + PV.isi + PV.nitawi + PV.ohci + PV.ati + PV.ka + PV.misi + PV.kah + PV.nihta + PV.nipahi + PV.nohte + PV.kakwe + PV.pimi + PV.mosci + PV.ta + PV.wah + PV.miyo + PV.papa + PV.papami + PV.tah + PV.ka_ki + PV.oh + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.plural + AI.money.count + AI.pray + AI.heat.fire + A.actor + N.actor + NA.persons.actor + left.ACTOR.actor + Sg.actor + auto.na.5.actor + ACTOR.right.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + auto.nda.1.actor + Pers.actor + auto.na.7.actor + Dem.actor + awa.actor + Incl.actor + ayisiyiniw.actor + Med.actor + Prox.actor + auto.nda.3.actor + pn.1Sg.actor + NA.beast.of.burden.actor + pn.3Sg.actor + Obv.actor + auto.na.1.actor + auto.na.2.actor + pn.1Pl.actor + nôhkom.actor + nâpêw.actor + NA.food.actor + wiya.actor + auto.na.3.actor + niya.actor + nîsta.actor + Px3Sg.actor + iskwêw.actor + kisêyiniw.actor + aya.actor + kikâwînaw.actor + nisîmis.actor + niyanân.actor + kêhtê.aya.actor + nôcikwêsiw.actor + wîsta.actor + pn.3Pl.actor + Indef.actor + auto.ni.3.actor + awiyak.actor + auto.na.4.actor + nimosôm.actor + nîstanân.actor + NI.object.actor + nitânis.actor + auto.na.6.actor + awâsis.actor + kîkway.actor + nitawâsimis.actor + Px12Pl.actor + cistêmâw.actor + wîstawâw.actor + I.goal + Sg.goal + N.goal + GOAL.right.goal + NI.object.goal + auto.ni.3.goal + left.GOAL.goal + Pron.goal + Dem.goal + ôma.goal + kîkway.goal + Pl.goal + NI.nominal.goal + auto.ni.6.goal, data = subset(AWive, AI))

### Check cochrane condition; step 1) remove things that aren't about 1
for (i in AI.Univariate.ive$univariate )
{print(which(chisq.test(i$posthoc$ctable)$expected<0.8))}
### No issue, move on to step 2) make sure 80% of expected are <5
for (i in AI.Univariate.ive$univariate)
{print(which(chisq.test(i$posthoc$ctable)$expected<5))}
#### 14/(120*4) == 2.9%, under 20% threshold

###Write to file
sink(file = 'AIUni-indve.txt')
print(summary(AI.Univariate.ive), max.print = NA)
sink(NULL)
###Make significance table
AI.Uni.ive.Sig<- subset(summary(AI.Univariate.ive)$sumry.table, alpha.X2 <0.05)

sink(file = 'AIUniSig-indve.txt')
print(AI.Uni.ive.Sig, max.print = NA)
sink(NULL)

### TI ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWive, TI)[grep("PV.[^GL]", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("^TI...*", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("actor$", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("goal$", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")
TI.Univariate.ive <- nominal(OrderType ~ RdplS + RdplW + Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.4Sg.Pl + Actor.1Pl + Actor.12Pl + Actor.2Pl + Actor.3Pl + PV.ki + PV.wi + PV.pe + PV.ohci + PV.ka + PV.isi + PV.nohte + PV.ati + PV.nipahi + PV.kah + PV.nitawi + PV.oh + PV.ta + PV.kakwe + PV.misi + TI.do + TI.cognitive + TI.speech + TI.money.count + NA.persons.actor + A.actor + auto.na.5.actor + ACTOR.right.actor + N.actor + Pron.actor + left.ACTOR.actor + Sg.actor + Pers.actor + Pl.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + pn.1Sg.actor + auto.nda.1.actor + Incl.actor + auto.na.7.actor + niya.actor + Dem.actor + awa.actor + auto.na.1.actor + ayisiyiniw.actor + pn.3Sg.actor + pn.3Pl.actor + Prox.actor + wiya.actor + nîsta.actor + nôhkom.actor + auto.nda.3.actor + Med.actor + Indef.actor + awiyak.actor + kisêyiniw.actor + wîstawâw.actor + I.goal + Sg.goal + N.goal + NI.object.goal + auto.ni.3.goal + GOAL.right.goal + left.GOAL.goal + Dem.goal + Pron.goal + ôma.goal + Pl.goal + kîkway.goal + Prox.goal + Med.goal + NI.nominal.goal + auto.ni.9.goal + NI.natural.force.goal + auto.ni.2.goal + auto.ni.6.goal + auto.ni.7.goal + D.goal + NI.nature.plants.goal + auto.ndi.1.goal + auto.ni.10.goal + NDI.Body.goal + NI.place.goal + auto.ni.1.goal + Px3Sg.goal + maskihkiy.goal + Der/Dim.goal + auto.ni.5.goal + wiyaw.goal + auto.ni.8.goal + Px1Sg.goal + auto.ni.4.goal + pahkêkin.goal + askiy.goal + masinahikan.goal + kâhkêwak.goal + Px3Pl.goal + maskisin.goal, data = subset(AWive, TI))

### Check cochrane condition; step 1) remove things that aren't about 1
for (i in AI.Univariate.ive$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
### 18/(120*4) == 3.8%, under 20% threshold
### No issue, move on to step 2) make sure 80% of expected are <5
for (i in TI.Univariate.ive$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}

#### (118-104)/(104*4) == 3.4%, under 20% threshold

###Write to file
sink(file = 'TIUni-indve.txt')
print(summary(TI.Univariate.ive), max.print = NA)
sink(NULL)
###Make significance table
TI.Uni.ive.Sig<- subset(summary(TI.Univariate.ive)$sumry.table, alpha.X2 <0.05)

sink(file = 'TIUniSig-indve.txt')
print(TI.Uni.ive.Sig, max.print = NA)
sink(NULL)

### TA ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWive, TA)[grep("PV.[^GL]", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("^TA...*", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("actor$", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("goal$", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")
TA.Univariate.ive <- nominal(OrderType ~ RdplS + RdplW + Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.4Sg.Pl + Actor.1Pl + Actor.12Pl + Actor.2Pl + Actor.3Pl + Goal.1Sg + Goal.2Sg + Goal.3Sg + Goal.4Sg.Pl + Goal.1Pl + Goal.12Pl + Goal.2Pl + Goal.3Pl + PV.ki + PV.pe + PV.wi + PV.isi + PV.ohci + PV.ka + PV.nitawi + PV.kah + PV.ati + PV.kakwe + PV.nohte + PV.misi + PV.wah + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + TA.allow + A.actor + N.actor + NA.persons.actor + Sg.actor + left.ACTOR.actor + auto.na.5.actor + ACTOR.right.actor + Pron.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + auto.nda.1.actor + Dem.actor + awa.actor + Pers.actor + Obv.actor + auto.na.7.actor + Prox.actor + Pl.actor + auto.nda.3.actor + Incl.actor + kôhtâwînaw.actor + pn.1Sg.actor + auto.na.1.actor + Med.actor + NA.religious.actor + A.goal + N.goal + left.GOAL.goal + NA.persons.goal + Sg.goal + GOAL.right.goal + Obv.goal + auto.na.5.goal + D.goal + NDA.Relations.goal + auto.nda.1.goal + Pl.goal + Pron.goal + Px1Sg.goal + NA.beast.of.burden.goal + Dem.goal + awa.goal + auto.na.7.goal + auto.na.2.goal + auto.na.3.goal + ayisiyiniw.goal + Prox.goal + Px3Sg.goal + Pers.goal + auto.nda.3.goal + awâsis.goal + Med.goal + NA.food.goal + auto.na.6.goal + pn.1Sg.goal + Px3Pl.goal + Incl.goal + nâpêw.goal + Px12Pl.goal + auto.na.1.goal + ninîkihik.goal + nitawâsimis.goal + misatim.goal + mostos.goal + nôsisim.goal, data = subset(AWive, TA))

### Check cochrane condiTAon; step 1) remove things that aren't about 1
for (i in TA.Univariate.ive$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 80% of expected are <5
for (i in TA.Univariate.ive$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
#### (112-103)/(103*4) == 2.2%, under 20% threshold

###Write to file
sink(file = 'TAUni-indve.txt')
print(summary(TA.Univariate.ive), max.print = NA)
sink(NULL)
###Make significance table
TA.Uni.ive.Sig<- subset(summary(TA.Univariate.ive)$sumry.table, alpha.X2 <0.05)

sink(file = 'TAUniSig-indve.txt')
print(TA.Uni.ive.Sig, max.print = NA)
sink(NULL)



### II ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWCnj, II)[grep("PV.[^GL]", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("^II...*", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("actor$", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("goal$", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")
II.Univariate.Cnj <- nominal(OrderType ~ RdplS + RdplW + Actor.3Sg + Actor.3Pl + Actor.4Sg + Actor.4Pl + PV.ki + PV.wi + PV.pe + II.natural.land + II.weather + II.sense + I.actor + Sg.actor + N.actor + NI.object.actor + auto.ni.3.actor + left.ACTOR.actor + ACTOR.right.actor + kîkway.actor + Dem.actor + Pron.actor + ôma.actor + NI.nominal.actor + Prox.actor + Pl.actor + Med.actor + auto.ni.6.actor + NI.place.actor + auto.ni.7.actor, data = subset(AWCnj, II))
### Check cochrane condition; step 1) remove things that aren't about 1
for (i in II.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
###remove all trues, resulting in:
II.Univariate.Cnj <- nominal(OrderType ~ Actor.3Sg + Actor.3Pl + Actor.4Sg + PV.ki + II.natural.land + II.weather + II.sense + I.actor + Sg.actor + N.actor + NI.object.actor + auto.ni.3.actor + left.ACTOR.actor + ACTOR.right.actor + kîkway.actor + Dem.actor + Pron.actor + ôma.actor + Prox.actor, data = subset(AWCnj, II))

for (i in II.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
#### 19/(19*3) = 16.67% ####


###Write to file
sink(file = 'IIUni-cnj.txt')
print(summary(II.Univariate.Cnj), max.print = NA)
sink(NULL)
###Make significance table
II.Uni.cnj.Sig<- subset(summary(II.Univariate.Cnj)$sumry.table, alpha.X2 <0.05)

sink(file = 'IIUniSig-cnj.txt')
print(II.Uni.cnj.Sig, max.print = NA)
sink(NULL)


### AI ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("PV.[^GL]", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("^AI...*", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("actor$", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("goal$", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")
AI.Univariate.Cnj <- nominal(OrderType ~ RdplS + RdplW + Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.1Pl + Actor.12Pl + Actor.3Pl + Actor.4Sg.Pl + PV.ki + PV.pe + PV.wi + PV.isi + PV.nitawi + PV.ati + PV.ohci + PV.misi + PV.kah + PV.kakwe + PV.pimi + PV.nihta + PV.nipahi + PV.wah + PV.nohte + PV.mosci + PV.papa + PV.papami + PV.kisi + PV.miyo + PV.maaci + PV.sipwe + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.plural + AI.money.count + AI.pray + AI.heat.fire + A.actor + N.actor + NA.persons.actor + left.ACTOR.actor + Sg.actor + ACTOR.right.actor + auto.na.5.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Pers.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + Dem.actor + awa.actor + ayisiyiniw.actor + Incl.actor + Med.actor + NA.beast.of.burden.actor + Prox.actor + Obv.actor + auto.nda.3.actor + pn.1Pl.actor + auto.na.1.actor + pn.3Sg.actor + auto.na.3.actor + pn.1Sg.actor + auto.na.2.actor + NA.food.actor + niyanân.actor + nâpêw.actor + kisêyiniw.actor + iskwêw.actor + aya.actor + Px3Sg.actor + kêhtê.aya.actor + wiya.actor + nôhkom.actor + awâsis.actor + Indef.actor + awiyak.actor + nîsta.actor + kikâwînaw.actor + wîsta.actor + auto.na.4.actor + niya.actor + nisîmis.actor + nîstanân.actor + auto.na.6.actor + pn.3Pl.actor + ninâpêm.actor + nêhiyaw.actor + Px12Pl.actor + auto.ni.3.actor + nitânis.actor + I.goal + Sg.goal + N.goal + GOAL.right.goal + NI.object.goal + auto.ni.3.goal + left.GOAL.goal + Dem.goal + Pron.goal + ôma.goal, data = subset(AWCnj, AI))
### Check cochrane condition; step 1) remove things that aren't about 1
for (i in AI.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
for(i in c(112,116,120,124,368,372,376,380,384,388,392,420,424,428,432)/4) { print(AI.Univariate.Cnj$independents[i])}
###remove all trues, resulting in:
AI.Univariate.Cnj <- nominal(OrderType ~ RdplS + RdplW + Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.1Pl + Actor.12Pl + Actor.3Pl + Actor.4Sg.Pl + PV.ki + PV.pe + PV.wi + PV.isi + PV.nitawi + PV.ati + PV.ohci + PV.misi + PV.kah + PV.kakwe + PV.pimi + PV.nihta + PV.nipahi + PV.wah + PV.nohte + PV.mosci + PV.papa + PV.papami + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.plural + AI.money.count + AI.pray + AI.heat.fire + A.actor + N.actor + NA.persons.actor + left.ACTOR.actor + Sg.actor + ACTOR.right.actor + auto.na.5.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Pers.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + Dem.actor + awa.actor + ayisiyiniw.actor + Incl.actor + Med.actor + NA.beast.of.burden.actor + Prox.actor + Obv.actor + auto.nda.3.actor + pn.1Pl.actor + auto.na.1.actor + pn.3Sg.actor + auto.na.3.actor + pn.1Sg.actor + auto.na.2.actor + NA.food.actor + niyanân.actor + nâpêw.actor + kisêyiniw.actor + iskwêw.actor + aya.actor + Px3Sg.actor + kêhtê.aya.actor + wiya.actor + nôhkom.actor + awâsis.actor + Indef.actor + awiyak.actor + nîsta.actor + kikâwînaw.actor + wîsta.actor + auto.na.4.actor + niya.actor + nisîmis.actor + nîstanân.actor + I.goal + Sg.goal + N.goal + GOAL.right.goal + NI.object.goal + auto.ni.3.goal, data = subset(AWCnj, AI))

for (i in AI.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
#### (178-(372/4))/((372/4)*6) == 15.23% ####


###Write to file
sink(file = 'AIUni-cnj.txt')
print(summary(AI.Univariate.Cnj), max.print = NA)
sink(NULL)
###Make significance table
AI.Uni.cnj.Sig<- subset(summary(AI.Univariate.Cnj)$sumry.table, alpha.X2 <0.05)

sink(file = 'AIUniSig-cnj.txt')
print(AI.Uni.cnj.Sig, max.print = NA)
sink(NULL)



### TI ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("PV.[^GL]", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("^TI...*", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("actor$", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("goal$", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")
TI.Univariate.Cnj <- nominal(OrderType ~ RdplS + RdplW + Actor.1Sg + Actor.2Sg + Actor.3Sg + Actor.1Pl + Actor.12Pl + Actor.3Pl + Actor.4Sg.Pl + PV.ki + PV.wi + PV.isi + PV.pe + PV.ohci + PV.nohte + PV.nitawi + PV.ati + PV.kakwe + PV.nipahi + PV.kah + TI.do + TI.cognitive + TI.speech + TI.money.count + NA.persons.actor + A.actor + N.actor + ACTOR.right.actor + auto.na.5.actor + Sg.actor + left.ACTOR.actor + Pron.actor + Pl.actor + Pers.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + auto.na.1.actor + Dem.actor + awa.actor + ayisiyiniw.actor + Incl.actor + pn.1Sg.actor + pn.3Sg.actor + pn.3Pl.actor + wiya.actor + Indef.actor + Prox.actor + awiyak.actor + iskwêw.actor + auto.nda.3.actor + nôhkom.actor + I.goal + Sg.goal + N.goal + NI.object.goal + auto.ni.3.goal + GOAL.right.goal + left.GOAL.goal + Dem.goal + Pron.goal + ôma.goal + Pl.goal + kîkway.goal + Prox.goal + Med.goal + NI.nominal.goal + auto.ni.9.goal + NI.natural.force.goal + auto.ni.2.goal + auto.ni.6.goal + D.goal + NI.place.goal + NI.nature.plants.goal + auto.ndi.1.goal + auto.ni.10.goal + NDI.Body.goal + Px3Sg.goal + auto.ni.4.goal + auto.ni.7.goal + Der/Dim.goal + auto.ni.1.goal + auto.ni.8.goal + maskihkiy.goal + wiyaw.goal + pahkêkin.goal + askiy.goal + auto.ni.5.goal + kâhkêwak.goal + Px12Pl.goal + Px3Pl.goal + atoskêwin.goal + mihti.goal + mînis.goal, data = subset(AWCnj, TI))
### Check cochrane condition; step 1) remove things that aren't about 1
for (i in TI.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
#### NONE ####

for (i in TI.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
#### (194-96)/(96*6) == 17.01%####


###Write to file
sink(file = 'TIUni-cnj.txt')
print(summary(TI.Univariate.Cnj), max.print = NA)
sink(NULL)
###Make significance table
TI.Uni.cnj.Sig<- subset(summary(TI.Univariate.Cnj)$sumry.table, alpha.X2 <0.05)

sink(file = 'TIUniSig-cnj.txt')
print(TI.Uni.cnj.Sig, max.print = NA)
sink(NULL)


### TA ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("PV.[^GL]", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("^TA...*", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("actor$", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("goal$", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")
TA.Univariate.Cnj <- nominal(OrderType ~ RdplS + RdplW + Actor.1Sg + Goal.1Sg + Actor.2Sg + Goal.2Sg + Actor.3Sg + Goal.3Sg + Actor.1Pl + Goal.1Pl + Actor.12Pl + Goal.12Pl + Actor.3Pl + Goal.3Pl + Actor.4Sg.Pl + Goal.4Sg.Pl + Goal.5Sg.Pl + PV.ki + PV.pe + PV.wi + PV.isi + PV.ohci + PV.nitawi + PV.kakwe + PV.kah + PV.ati + PV.nohte + PV.misi + TA.do + TA.speech + TA.cognitive + TA.food + TA.money.count + TA.allow + A.actor + N.actor + Sg.actor + NA.persons.actor + left.ACTOR.actor + ACTOR.right.actor + auto.na.5.actor + D.actor + NDA.Relations.actor + Pron.actor + Px1Sg.actor + auto.nda.1.actor + Dem.actor + Obv.actor + auto.na.7.actor + awa.actor + Pers.actor + Prox.actor + Pl.actor + auto.nda.3.actor + auto.na.1.actor + kôhtâwînaw.actor + Med.actor + NA.religious.actor + Incl.actor + iskwêw.actor + A.goal + N.goal + left.GOAL.goal + NA.persons.goal + GOAL.right.goal + Sg.goal + Obv.goal + auto.na.5.goal + D.goal + NDA.Relations.goal + auto.nda.1.goal + Pl.goal + Pron.goal + NA.beast.of.burden.goal + Px1Sg.goal + Dem.goal + awa.goal + auto.na.7.goal + ayisiyiniw.goal + auto.na.2.goal + auto.na.3.goal + Prox.goal + Px3Sg.goal + Pers.goal + auto.nda.3.goal + nâpêw.goal + awâsis.goal + NA.food.goal + Med.goal + Px3Pl.goal + auto.na.6.goal + nitawâsimis.goal + Px12Pl.goal + auto.na.1.goal + pn.1Sg.goal + Incl.goal + iskwêw.goal + kêhtê.aya.goal + ninâpêm.goal + misatim.goal, data = subset(AWCnj, TA))
### Check cochrane condition; step 1) remove things that aren't about 1
for (i in TA.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
#### NONE ####

for (i in TA.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
#### (184-100)/(100*6) == 14% ####


###Write to file
sink(file = 'TAUni-cnj.txt')
print(summary(TA.Univariate.Cnj), max.print = NA)
sink(NULL)
###Make significance table
TA.Uni.cnj.Sig<- subset(summary(TA.Univariate.Cnj)$sumry.table, alpha.X2 <0.05)

sink(file = 'TAUniSig-cnj.txt')
print(TA.Uni.cnj.Sig, max.print = NA)
sink(NULL)