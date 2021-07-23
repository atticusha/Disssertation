
library(polytomous) 
##ind vs cnj generally 2x2, 0 <5 allowed
cochran_ive <- function(uniobjuni) {
  length(which(chisq.test(uniobjuni$posthoc$ctable)$expected<5))>0
}



## Ind v. eCnj
###=====================================================================================================
###=====================================================================================================


### II ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWive, II)[grep("PV.[A-Z]", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, II)[grep("^II...*", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, II)[grep("Rdpl", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, II)[grep("^N.*[0-9]$", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWive, II)[grep("^auto.*", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, II)[grep("actor$", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, II)[grep("^actor", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, II)[grep("^goal", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, II)[grep("goal$", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

II.Univariate.ive.0 <- nominal(OrderType ~ PV.Qual + PV.StartFin + PV.Position + II.sense + II.natural.land + II.weather + RdplS +  auto.vii.1 + auto.vii.2 + auto.ni.3.actor + auto.vii.3 + auto.vii.4 + auto.vii.5 + auto.ni.6.actor + auto.ni.7.actor + Sg.actor + NI.object.actor + auto.ni.3.actor + Dem.actor + Pron.actor + Err.Orth.actor + Med.actor + NI.nominal.actor + Pl.actor + auto.ni.6.actor + auto.ni.7.actor + Prox.actor + actor.3 + actor.4, data = subset(AWive, II))

### Check cochrane condition; step 1) remove things that aren't about 1
for (i in II.Univariate.ive.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 0 of expected are <5
issues.ii.ive<-lapply(II.Univariate.ive.0$univariate,cochran_ive)
str(issues.ii.ive)

II.Univariate.ive <- nominal(OrderType ~ PV.Qual + PV.StartFin + II.sense + II.natural.land + II.weather +  auto.vii.1 + auto.vii.2 + auto.ni.3.actor + auto.vii.3 + auto.vii.4 + auto.vii.5 + auto.ni.6.actor + Sg.actor + NI.object.actor + auto.ni.3.actor + Dem.actor + Pron.actor + Err.Orth.actor + Med.actor + NI.nominal.actor + Pl.actor + auto.ni.6.actor + actor.3 + actor.4, data = subset(AWive, II))

###Write to file
sink(file = 'ive/issues.ii.ive.txt')
print(issues.ii.ive, max.print = NA)
sink(NULL)

###Write to file
sink(file = 'ive/IIUni-indve.txt')
print(summary(II.Univariate.ive), max.print = NA)
sink(NULL)
###Make significance table
II.Uni.ive.Sig<- subset(summary(II.Univariate.ive)$sumry.table, alpha.X2 <0.05)

sink(file = 'ive/IIUniSig-indve.txt')
print(II.Uni.ive.Sig, max.print = NA)
sink(NULL)

### AI ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWive, AI)[grep("PV.[A-Z]", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("^AI...*", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("Rdpl", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("^N.*[0-9]$", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("^auto.*", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("actor$", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("^actor", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("^goal", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("goal$", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

AI.Univariate.ive.0 <- nominal(OrderType ~ PV.Move + PV.Qual + PV.Position + PV.StartFin + PV.Discourse + PV.Time + PV.WantCan + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.money.count + AI.plural + AI.pray + AI.heat.fire + RdplS + RdplW +  auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.na.5.actor + auto.vai.11 + auto.vai.9 + auto.nda.1.actor + auto.vai.3 + auto.vai.13 + auto.vai.4 + auto.na.7.actor + auto.vai.1 + auto.vai.8 + auto.nda.3.actor + auto.vai.7 + auto.vai.10 + auto.na.1.actor + auto.na.2.actor + auto.na.3.actor + auto.na.4.actor + auto.na.6.actor + auto.ni.3.actor + NA.persons.actor + Sg.actor + auto.na.5.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Err.Orth.actor + Pers.actor + Px1Sg.actor + Dem.actor + auto.nda.1.actor + auto.na.7.actor + Prox.actor + Med.actor + Incl.actor + Obv.actor + auto.nda.3.actor + NA.beast.of.burden.actor + auto.na.1.actor + auto.na.2.actor + auto.na.3.actor + Px3Sg.actor + NA.food.actor + Emph.actor + Indef.actor + auto.na.4.actor + auto.na.6.actor + auto.ni.3.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWive, AI))

### Check cochrane condition; step 1) remove things that aren't about 1
for (i in AI.Univariate.ive.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 0 of expected are <5
issues.ai.ive<-lapply(AI.Univariate.ive.0$univariate,cochran_ive)
str(issues.ai.ive)

AI.Univariate.ive <- nominal(OrderType ~ PV.Move + PV.Qual + PV.Position + PV.StartFin + PV.Discourse + PV.Time + PV.WantCan + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.money.count + AI.plural + AI.pray + AI.heat.fire + RdplS + RdplW +  auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.na.5.actor + auto.vai.11 + auto.vai.9 + auto.nda.1.actor + auto.vai.3 + auto.vai.13 + auto.vai.4 + auto.na.7.actor + auto.vai.1 + auto.vai.8 + auto.nda.3.actor + auto.vai.7 + auto.vai.10 + auto.na.1.actor + auto.na.2.actor + auto.na.3.actor + auto.na.4.actor + auto.na.6.actor + NA.persons.actor + Sg.actor + auto.na.5.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Err.Orth.actor + Pers.actor + Px1Sg.actor + Dem.actor + auto.nda.1.actor + auto.na.7.actor + Prox.actor + Med.actor + Incl.actor + Obv.actor + auto.nda.3.actor + NA.beast.of.burden.actor + auto.na.1.actor + auto.na.2.actor + auto.na.3.actor + Px3Sg.actor + NA.food.actor + Emph.actor + Indef.actor + auto.na.4.actor + auto.na.6.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWive, AI))

###Write to file
sink(file = 'ive/issues.ai.ive.txt')
print(issues.ai.ive, max.print = NA)
sink(NULL)

###Write to file
sink(file = 'ive/AIUni-indve.txt')
print(summary(AI.Univariate.ive), max.print = NA)
sink(NULL)
###Make significance table
AI.Uni.ive.Sig<- subset(summary(AI.Univariate.ive)$sumry.table, alpha.X2 <0.05)

sink(file = 'ive/AIUniSig-indve.txt')
print(AI.Uni.ive.Sig, max.print = NA)
sink(NULL)

### TI ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWive, TI)[grep("PV.[A-Z]", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("^TI...*", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("Rdpl", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("^N.*[0-9]$", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("^auto.*", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("actor$", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("^actor", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("^goal", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("goal$", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

TI.Univariate.ive.0 <- nominal(OrderType ~ PV.Position + PV.Move + PV.Time + PV.StartFinWantCan + PV.Qual + PV.Discourse + PV.WantCan + PV.StartFin + TI.do + TI.cognitive + TI.speech + TI.money.count + RdplW + RdplS +  auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.na.5.actor + auto.vti.3 + auto.ni.10.goal + auto.ni.9.goal + auto.ni.2.goal + auto.ni.6.goal + auto.nda.1.actor + auto.ni.7.goal + auto.na.7.actor + auto.ni.1.goal + auto.ni.4.goal + auto.ndi.1.goal + auto.ni.5.goal + auto.ni.8.goal + auto.na.1.actor + auto.vti.5 + NA.persons.actor + auto.na.5.actor + Pron.actor + Pers.actor + Sg.actor + Err.Orth.actor + Pl.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + Dem.actor + Incl.actor + Prox.actor + auto.na.1.actor + Emph.actor + Med.actor + actor.3 + actor.1 + actor.2 + actor.4 + goal.3 + Sg.goal + NI.object.goal + auto.ni.3.goal + Dem.goal + Pron.goal + Pl.goal + Med.goal + Err.Orth.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + auto.ni.10.goal + NI.nature.plants.goal + auto.ni.9.goal + auto.ni.2.goal + auto.ni.6.goal + auto.ni.7.goal + NI.place.goal + Px3Sg.goal + auto.ni.1.goal + auto.ni.4.goal + D.goal + auto.ndi.1.goal + auto.ni.5.goal + auto.ni.8.goal + NDI.Body.goal + Px1Sg.goal + Px3Pl.goal + Der.Dim.goal, data = subset(AWive, TI))

for (i in TI.Univariate.ive.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 0 of expected are <5
issues.ti.ive<-lapply(TI.Univariate.ive.0$univariate,cochran_ive)
str(issues.ti.ive)

TI.Univariate.ive <- nominal(OrderType ~ PV.Position + PV.Move + PV.Time + PV.StartFinWantCan + PV.Qual + PV.Discourse + PV.WantCan + PV.StartFin + TI.do + TI.cognitive + TI.speech + TI.money.count + RdplW + RdplS +  auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.na.5.actor + auto.vti.3 + auto.ni.10.goal + auto.ni.9.goal + auto.ni.2.goal + auto.ni.6.goal + auto.nda.1.actor + auto.ni.7.goal + auto.na.7.actor + auto.ni.1.goal + auto.ni.4.goal + auto.ndi.1.goal + auto.ni.5.goal + auto.ni.8.goal + auto.na.1.actor + NA.persons.actor + auto.na.5.actor + Pron.actor + Pers.actor + Sg.actor + Err.Orth.actor + Pl.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + Dem.actor + Incl.actor + Prox.actor + auto.na.1.actor + actor.3 + actor.1 + actor.2 + actor.4 + goal.3 + Sg.goal + NI.object.goal + auto.ni.3.goal + Dem.goal + Pron.goal + Pl.goal + Med.goal + Err.Orth.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + auto.ni.10.goal + NI.nature.plants.goal + auto.ni.9.goal + auto.ni.2.goal + auto.ni.6.goal + auto.ni.7.goal + NI.place.goal + Px3Sg.goal + auto.ni.1.goal + auto.ni.4.goal + D.goal + auto.ndi.1.goal + auto.ni.5.goal + auto.ni.8.goal + NDI.Body.goal + Px1Sg.goal + Px3Pl.goal, data = subset(AWive, TI))

###Write to file
sink(file = 'ive/issues.ti.ive.txt')
print(issues.ti.ive, max.print = NA)
sink(NULL)

###Write to file
sink(file = 'ive/TIUni-indve.txt')
print(summary(TI.Univariate.ive), max.print = NA)
sink(NULL)
###Make significance table
TI.Uni.ive.Sig<- subset(summary(TI.Univariate.ive)$sumry.table, alpha.X2 <0.05)

sink(file = 'ive/TIUniSig-indve.txt')
print(TI.Uni.ive.Sig, max.print = NA)
sink(NULL)

### TA ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWive, TA)[grep("PV.[A-Z]", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("^TA...*", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("Rdpl", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("^N.*[0-9]$", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("^auto.*", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("actor$", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("^actor", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("^goal", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("goal$", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

TA.Univariate.ive.0 <- nominal(OrderType ~ PV.Move + PV.Discourse + PV.Position + PV.Qual + PV.Time + PV.StartFin + PV.WantCan + TA.speech + TA.do + TA.cognitive + TA.food + TA.money.count + TA.allow + RdplS + RdplW +  auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.nda.1.goal + auto.na.5.actor + auto.vta.4 + auto.vta.2 + auto.nda.1.actor + auto.na.7.goal + auto.na.2.goal + auto.na.3.goal + auto.nda.3.goal + auto.na.7.actor + auto.na.6.goal + auto.na.1.goal + auto.nda.3.actor + auto.na.1.actor + auto.vta.7 + NA.persons.actor + Sg.actor + auto.na.5.actor + D.actor + NDA.Relations.actor + Pron.actor + auto.nda.1.actor + Px1Sg.actor + Err.Orth.actor + Dem.actor + Pers.actor + Pl.actor + Obv.actor + auto.na.7.actor + Prox.actor + Med.actor + Incl.actor + auto.nda.3.actor + auto.na.1.actor + Px3Sg.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + NA.persons.goal + Obv.goal + auto.na.5.goal + Sg.goal + D.goal + NDA.Relations.goal + Pron.goal + auto.nda.1.goal + Pl.goal + Dem.goal + Err.Orth.goal + Px1Sg.goal + NA.beast.of.burden.goal + auto.na.7.goal + Prox.goal + auto.na.2.goal + auto.na.3.goal + Med.goal + Px3Sg.goal + Pers.goal + auto.nda.3.goal + NA.food.goal + auto.na.6.goal + auto.na.1.goal + Px3Pl.goal + Incl.goal + Px12Pl.goal, data = subset(AWive, TA))

for (i in TA.Univariate.ive.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 0 of expected are <5
issues.ta.ive<-lapply(TA.Univariate.ive.0$univariate,cochran_ive)
str(issues.ta.ive)

TA.Univariate.ive <- nominal(OrderType ~ PV.Move + PV.Discourse + PV.Position + PV.Qual + PV.Time + PV.StartFin + PV.WantCan + TA.speech + TA.do + TA.cognitive + TA.food + TA.money.count + TA.allow + RdplS + RdplW +  auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.nda.1.goal + auto.na.5.actor + auto.vta.4 + auto.vta.2 + auto.nda.1.actor + auto.na.7.goal + auto.na.2.goal + auto.na.3.goal + auto.nda.3.goal + auto.na.7.actor + auto.na.6.goal + auto.na.1.goal + auto.nda.3.actor + auto.na.1.actor + auto.vta.7 + NA.persons.actor + Sg.actor + auto.na.5.actor + D.actor + NDA.Relations.actor + Pron.actor + auto.nda.1.actor + Px1Sg.actor + Err.Orth.actor + Dem.actor + Pers.actor + Pl.actor + Obv.actor + auto.na.7.actor + Prox.actor + Med.actor + Incl.actor + auto.nda.3.actor + auto.na.1.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + NA.persons.goal + Obv.goal + auto.na.5.goal + Sg.goal + D.goal + NDA.Relations.goal + Pron.goal + auto.nda.1.goal + Pl.goal + Dem.goal + Err.Orth.goal + Px1Sg.goal + NA.beast.of.burden.goal + auto.na.7.goal + Prox.goal + auto.na.2.goal + auto.na.3.goal + Med.goal + Px3Sg.goal + Pers.goal + auto.nda.3.goal + NA.food.goal + auto.na.6.goal + auto.na.1.goal + Px3Pl.goal + Incl.goal + Px12Pl.goal, data = subset(AWive, TA))

###Write to file
sink(file = 'ive/issues.ta.ive.txt')
print(issues.ta.ive, max.print = NA)
sink(NULL)

###Write to file
sink(file = 'ive/TAUni-indve.txt')
print(summary(TA.Univariate.ive), max.print = NA)
sink(NULL)
###Make significance table
TA.Uni.ive.Sig<- subset(summary(TA.Univariate.ive)$sumry.table, alpha.X2 <0.05)

sink(file = 'ive/TAUniSig-indve.txt')
print(TA.Uni.ive.Sig, max.print = NA)
sink(NULL)

