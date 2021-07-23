### II ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWCnj, II)[grep("PV.[^GL]", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("^II...*", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("actor$", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("goal$", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")
II.Univariate.Cnj <- nominal(OrderType ~ PV.e + PV.kaa + PV.ki + PV.wi + PV.pe + II.natural.land + II.weather + II.sense + I.actor + Sg.actor + NI.object.actor + auto.ni.3.actor + N.actor + ACTOR.right.actor + left.ACTOR.actor + Dem.actor + Pron.actor + ôma.actor + kîkway.actor + Prox.actor + Med.actor + NI.nominal.actor + Pl.actor + auto.ni.6.actor, data = subset(AWCnj, II))
### Check cochrane condition; step 1) remove things that aren't about 1
for (i in II.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
###remove all trues, resulting in:
II.Univariate.Cnj <- nominal(OrderType ~ PV.e + PV.kaa + PV.ki + II.natural.land + II.weather + II.sense + I.actor + Sg.actor + NI.object.actor + auto.ni.3.actor + N.actor + ACTOR.right.actor + left.ACTOR.actor + Dem.actor + Pron.actor + ôma.actor + kîkway.actor + Prox.actor, data = subset(AWCnj, II))

for (i in II.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
#### (35-18)/(18*6) = 15.7% ####


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
AI.Univariate.Cnj <- nominal(OrderType ~ PV.e + PV.ki + PV.kaa + PV.pe + PV.ka + PV.wi + PV.isi + PV.nitawi + PV.ati + PV.ohci + PV.misi + PV.kah + PV.kakwe + PV.pimi + PV.nihta + PV.nipahi + PV.wah + PV.nohte + PV.mosci + PV.papa + PV.papami + PV.kisi + PV.miyo + PV.maaci + PV.sipwe + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.plural + AI.money.count + AI.pray + AI.heat.fire + A.actor + N.actor + NA.persons.actor + left.ACTOR.actor + Sg.actor + ACTOR.right.actor + auto.na.5.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Pers.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + Dem.actor + awa.actor + ayisiyiniw.actor + Incl.actor + Med.actor + NA.beast.of.burden.actor + Prox.actor + Obv.actor + auto.nda.3.actor + pn.1Pl.actor + auto.na.1.actor + pn.3Sg.actor + auto.na.3.actor + pn.1Sg.actor + auto.na.2.actor + NA.food.actor + niyanân.actor + nâpêw.actor + kisêyiniw.actor + iskwêw.actor + aya.actor + Px3Sg.actor + kêhtê.aya.actor + wiya.actor + nôhkom.actor + awâsis.actor + Indef.actor + awiyak.actor + nîsta.actor + kikâwînaw.actor + wîsta.actor + auto.na.4.actor + niya.actor + nisîmis.actor + nîstanân.actor + auto.na.6.actor + pn.3Pl.actor + ninâpêm.actor + nêhiyaw.actor + Px12Pl.actor + auto.ni.3.actor + nitânis.actor + I.goal + Sg.goal + N.goal + GOAL.right.goal + NI.object.goal + auto.ni.3.goal + left.GOAL.goal + Dem.goal + Pron.goal + ôma.goal, data = subset(AWCnj, AI))
### Check cochrane condition; step 1) remove things that aren't about 1
for (i in AI.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
for(i in c(88,92,96,100,344,348,352,356,360,364,368,396,400,404,408)/4) { print(AI.Univariate.Cnj$independents[i])}
###remove all trues, resulting in:
AI.Univariate.Cnj <- nominal(OrderType ~ PV.e + PV.ki + PV.kaa + PV.pe + PV.ka + PV.wi + PV.isi + PV.nitawi + PV.ati + PV.ohci + PV.misi + PV.kah + PV.kakwe + PV.pimi + PV.nihta + PV.nipahi + PV.wah + PV.nohte + PV.mosci + PV.papa + PV.papami + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.plural + AI.money.count + AI.pray + AI.heat.fire + A.actor + N.actor + NA.persons.actor + left.ACTOR.actor + Sg.actor + ACTOR.right.actor + auto.na.5.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Pers.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + Dem.actor + awa.actor + ayisiyiniw.actor + Incl.actor + Med.actor + NA.beast.of.burden.actor + Prox.actor + Obv.actor + auto.nda.3.actor + pn.1Pl.actor + auto.na.1.actor + pn.3Sg.actor + auto.na.3.actor + pn.1Sg.actor + auto.na.2.actor + NA.food.actor + niyanân.actor + nâpêw.actor + kisêyiniw.actor + iskwêw.actor + aya.actor + Px3Sg.actor + kêhtê.aya.actor + wiya.actor + nôhkom.actor + awâsis.actor + Indef.actor + awiyak.actor + nîsta.actor + kikâwînaw.actor + wîsta.actor + auto.na.4.actor + niya.actor + nisîmis.actor + nîstanân.actor + I.goal + Sg.goal + N.goal + GOAL.right.goal + NI.object.goal + auto.ni.3.goal, data = subset(AWCnj, AI))

for (i in AI.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
#### (170-(348/4))/((348/4)*6) == 15.9% ####


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
TI.Univariate.Cnj <- nominal(OrderType ~ PV.e + PV.ki + PV.kaa + PV.ka + PV.wi + PV.isi + PV.pe + PV.ohci + PV.nohte + PV.nitawi + PV.ati + PV.kakwe + PV.nipahi + PV.kah + TI.do + TI.cognitive + TI.speech + TI.money.count + NA.persons.actor + A.actor + N.actor + ACTOR.right.actor + auto.na.5.actor + Sg.actor + left.ACTOR.actor + Pron.actor + Pl.actor + Pers.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + auto.na.1.actor + Dem.actor + awa.actor + ayisiyiniw.actor + Incl.actor + pn.1Sg.actor + pn.3Sg.actor + pn.3Pl.actor + wiya.actor + Indef.actor + Prox.actor + awiyak.actor + iskwêw.actor + auto.nda.3.actor + nôhkom.actor + I.goal + Sg.goal + N.goal + NI.object.goal + auto.ni.3.goal + GOAL.right.goal + left.GOAL.goal + Dem.goal + Pron.goal + ôma.goal + Pl.goal + kîkway.goal + Prox.goal + Med.goal + NI.nominal.goal + auto.ni.9.goal + NI.natural.force.goal + auto.ni.2.goal + auto.ni.6.goal + D.goal + NI.place.goal + NI.nature.plants.goal + auto.ndi.1.goal + auto.ni.10.goal + NDI.Body.goal + Px3Sg.goal + auto.ni.4.goal + auto.ni.7.goal + Der/Dim.goal + auto.ni.1.goal + auto.ni.8.goal + maskihkiy.goal + wiyaw.goal + pahkêkin.goal + askiy.goal + auto.ni.5.goal + kâhkêwak.goal + Px12Pl.goal + Px3Pl.goal + atoskêwin.goal + mihti.goal + mînis.goal, data = subset(AWCnj, TI))
### Check cochrane condition; step 1) remove things that aren't about 1
for (i in TI.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
#### NONE ####

for (i in TI.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
#### (183-(360/4))/((360/4)*6) == 17.2%####


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
TA.Univariate.Cnj <- nominal(OrderType ~ PV.e + PV.ki + PV.kaa + PV.ka + PV.pe + PV.wi + PV.isi + PV.ohci + PV.nitawi + PV.kakwe + PV.kah + PV.ati + PV.nohte + PV.misi + TA.do + TA.speech + TA.cognitive + TA.food + TA.money.count + TA.allow + A.actor + N.actor + Sg.actor + NA.persons.actor + left.ACTOR.actor + ACTOR.right.actor + auto.na.5.actor + D.actor + NDA.Relations.actor + Pron.actor + Px1Sg.actor + auto.nda.1.actor + Dem.actor + Obv.actor + auto.na.7.actor + awa.actor + Pers.actor + Prox.actor + Pl.actor + auto.nda.3.actor + auto.na.1.actor + kôhtâwînaw.actor + Med.actor + NA.religious.actor + Incl.actor + iskwêw.actor + A.goal + N.goal + left.GOAL.goal + NA.persons.goal + GOAL.right.goal + Sg.goal + Obv.goal + auto.na.5.goal + D.goal + NDA.Relations.goal + auto.nda.1.goal + Pl.goal + Pron.goal + NA.beast.of.burden.goal + Px1Sg.goal + Dem.goal + awa.goal + auto.na.7.goal + ayisiyiniw.goal + auto.na.2.goal + auto.na.3.goal + Prox.goal + Px3Sg.goal + Pers.goal + auto.nda.3.goal + nâpêw.goal + awâsis.goal + NA.food.goal + Med.goal + Px3Pl.goal + auto.na.6.goal + nitawâsimis.goal + Px12Pl.goal + auto.na.1.goal + pn.1Sg.goal + Incl.goal + iskwêw.goal + kêhtê.aya.goal + ninâpêm.goal + misatim.goal, data = subset(AWCnj, TA))
### Check cochrane condition; step 1) remove things that aren't about 1
for (i in TA.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
#### NONE ####

for (i in TA.Univariate.Cnj$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<5)}
####(161-(344/4))/((344/4)*6) == 14.5% ####


###Write to file
sink(file = 'TAUni-cnj.txt')
print(summary(TA.Univariate.Cnj), max.print = NA)
sink(NULL)
###Make significance table
TA.Uni.cnj.Sig<- subset(summary(TA.Univariate.Cnj)$sumry.table, alpha.X2 <0.05)

sink(file = 'TAUniSig-cnj.txt')
print(TA.Uni.cnj.Sig, max.print = NA)
sink(NULL)


