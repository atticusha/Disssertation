## Ind v. Cnj
## Contingency tables here are 2x2 so no more than 0 < 5
library(polytomous)
cochran_indcnj <- function(uniobjuni) {
  length(which(chisq.test(uniobjuni$posthoc$ctable)$expected<5))>0
}
###=====================================================================================================
###=====================================================================================================
### II ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWnimp, II)[grep("PV.[A-Z]", names(subset(AWnimp, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, II)[grep("^II...*", names(subset(AWnimp, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, II)[grep("Rdpl", names(subset(AWnimp, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, II)[grep("^N.*[0-9]$", names(subset(AWnimp, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWnimp, II)[grep("^auto.*", names(subset(AWnimp, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, II)[grep("actor$", names(subset(AWnimp, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, II)[grep("^actor", names(subset(AWnimp, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, II)[grep("^goal", names(subset(AWnimp, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, II)[grep("goal$", names(subset(AWnimp, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

II.Univariate.indcnj.0 <- nominal(TopOrder ~ PV.StartFin + PV.Qual + PV.Position + PV.Time + PV.Move + II.sense + II.natural.land + II.weather + II.plural + RdplS +  auto.vii.1 + auto.vii.2 + auto.ni.3.actor + auto.vii.3 + auto.vii.4 + auto.vii.5 + auto.ni.6.actor + auto.ni.7.actor + auto.vii.6 + I.actor + Sg.actor +  NI.object.actor + auto.ni.3.actor + Dem.actor + Pro Err.Orth.actor + Med.actor + NI.nominal.actor + Pl.actor + auto.ni.6.actor + auto.ni.7.actor + Prox.actor + actor.3 + actor.4, data = subset(AWnimp, II))

### Check cochrane condition; step 1) remove things that aren't about 1
for (i in II.Univariate.indcnj.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 0 of expected are <5
issues.ii.indcnj<-lapply(II.Univariate.indcnj.0$univariate,cochran_indcnj)
str(issues.ii.indcnj)

II.Univariate.indcnj <- nominal(TopOrder ~ PV.StartFin + PV.Qual + II.sense + II.natural.land + II.weather +  auto.vii.1 + auto.vii.2 + auto.ni.3.actor + auto.vii.3 + auto.vii.4 + auto.vii.5 + I.actor + Sg.actor +  NI.object.actor + auto.ni.3.actor + Dem.actor + Pro Err.Orth.actor + Med.actor + NI.nominal.actor + Pl.actor + actor.3 + actor.4, data = subset(AWnimp, II))

###Write to file
sink(file = 'indcnj/issues.ii.indcnj.txt')
print(issues.ii.indcnj, max.print = NA)
sink(NULL)

sink(file = 'indcnj/IIUni-indcnj.txt')
print(summary(II.Univariate.indcnj), max.print = NA)
sink(NULL)

sink(file = 'indcnj/IIUniSig-indcnj.txt')
print(subset(summary(II.Univariate.indcnj)$sumry.table, alpha.X2 <0.05), max.print = NA)
sink(NULL)


###=====================================================================================================
###=====================================================================================================
### AI ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWnimp, AI)[grep("PV.[A-Z]", names(subset(AWnimp, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, AI)[grep("^AI...*", names(subset(AWnimp, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, AI)[grep("Rdpl", names(subset(AWnimp, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, AI)[grep("^N.*[0-9]$", names(subset(AWnimp, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWnimp, AI)[grep("^auto.*", names(subset(AWnimp, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, AI)[grep("actor$", names(subset(AWnimp, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, AI)[grep("^actor", names(subset(AWnimp, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, AI)[grep("^goal", names(subset(AWnimp, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, AI)[grep("goal$", names(subset(AWnimp, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")


AI.Univariate.indcnj.0 <- nominal(TopOrder ~ PV.Move + PV.Time + PV.Position + PV.StartFinWantCan + PV.Qual + PV.Discourse + PV.StartFin + PV.WantCan + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.money.count + AI.plural + AI.pray + AI.heat.fire + AI.childcare + RdplW + RdplS +  auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.na.5.actor + auto.vai.11 + auto.vai.9 + auto.vai.3 + auto.nda.1.actor + auto.vai.13 + auto.vai.4 + auto.na.7.actor + auto.vai.1 + auto.vai.8 + auto.vai.7 + auto.na.1.actor + auto.nda.3.actor + auto.vai.10 + auto.na.3.actor + auto.na.2.actor + auto.na.4.actor + auto.na.6.actor + auto.ni.3.actor + NA.persons.actor + Sg.actor + auto.na.5.actor + Pro Pl.actor + D.actor + NDA.Relations.actor + Dem.actor + Err.Orth.actor + Pers.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + Prox.actor + Med.actor + Incl.actor + Obv.actor + auto.na.1.actor + NA.beast.of.burde auto.nda.3.actor + auto.na.3.actor + auto.na.2.actor + NA.food.actor + Px3Sg.actor + Indef.actor + Emph.actor + auto.na.4.actor + auto.na.6.actor + auto.ni.3.actor + Px12Pl.actor + Px1Pl.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWnimp, AI))

### Check cochrane condition; step 1) remove things that aren't about 1
for (i in AI.Univariate.indcnj.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 0 of expected are <5
issues.ai.indcnj<-lapply(AI.Univariate.indcnj.0$univariate,cochran_indcnj)
str(issues.ai.indcnj)

AI.Univariate.indcnj <- nominal(TopOrder ~ PV.Move + PV.Time + PV.Position + PV.Qual + PV.Discourse + PV.StartFin + PV.WantCan + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.money.count + AI.plural + AI.pray + AI.heat.fire + RdplW + RdplS +  auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.na.5.actor + auto.vai.11 + auto.vai.9 + auto.vai.3 + auto.nda.1.actor + auto.vai.13 + auto.vai.4 + auto.na.7.actor + auto.vai.1 + auto.vai.8 + auto.vai.7 + auto.na.1.actor + auto.nda.3.actor + auto.vai.10 + auto.na.3.actor + auto.na.2.actor + auto.na.4.actor + auto.na.6.actor + NA.persons.actor + Sg.actor + auto.na.5.actor + Pro Pl.actor + D.actor + NDA.Relations.actor + Dem.actor + Err.Orth.actor + Pers.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + Prox.actor + Med.actor + Incl.actor + Obv.actor + auto.na.1.actor + NA.beast.of.burde auto.nda.3.actor + auto.na.3.actor + auto.na.2.actor + NA.food.actor + Px3Sg.actor + Indef.actor + Emph.actor + auto.na.4.actor + auto.na.6.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWnimp, AI))


###Write to file
sink(file = 'indcnj/AIUni-indcnj.txt')
print(summary(AI.Univariate.indcnj), max.print = NA)
sink(NULL)

sink(file = 'indcnj/issues.ai.indcnj.txt')
print(issues.ai.indcnj, max.print = NA)
sink(NULL)
###Make significance table
AI.Uni.indcnj.Sig<- subset(summary(AI.Univariate.indcnj)$sumry.table, alpha.X2 <0.05)

sink(file = 'indcnj/AIUniSig-indcnj.txt')
print(AI.Uni.indcnj.Sig, max.print = NA)
sink(NULL)


###=====================================================================================================
###=====================================================================================================
### TI ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWnimp, TI)[grep("PV.[A-Z]", names(subset(AWnimp, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, TI)[grep("^TI...*", names(subset(AWnimp, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, TI)[grep("Rdpl", names(subset(AWnimp, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, TI)[grep("^N.*[0-9]$", names(subset(AWnimp, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWnimp, TI)[grep("^auto.*", names(subset(AWnimp, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, TI)[grep("actor$", names(subset(AWnimp, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, TI)[grep("^actor", names(subset(AWnimp, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, TI)[grep("^goal", names(subset(AWnimp, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, TI)[grep("goal$", names(subset(AWnimp, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

TI.Univariate.indcnj.0 <- nominal(TopOrder ~  PV.Time + PV.Position + PV.Move + PV.WantCan + PV.Discourse + PV.Qual + PV.StartFin + TI.do + TI.cognitive + TI.speech + TI.money.count + RdplW + RdplS +  auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.na.5.actor + auto.vti.3 + auto.ni.10.goal + auto.ni.9.goal + auto.ni.2.goal + auto.ni.6.goal + auto.nda.1.actor + auto.ni.4.goal + auto.na.7.actor + auto.ni.7.goal + auto.ni.1.goal + auto.ndi.1.goal + auto.ni.8.goal + auto.na.1.actor + auto.ni.5.goal + auto.vti.5 + auto.vti.6 + auto.ni.11.goal + NA.persons.actor + auto.na.5.actor + Pro Sg.actor + Pers.actor + Pl.actor + Err.Orth.actor + D.actor + NDA.Relations.actor + Dem.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + Incl.actor + Prox.actor + auto.na.1.actor + Med.actor + Emph.actor + Indef.actor + awiyak.actor + actor.3 + actor.1 + actor.2 + actor.4 + goal.3 + Sg.goal + NI.object.goal + N.goal + auto.ni.3.goal + Dem.goal + Pron.goal + Pl.goal + Med.goal + Err.Orth.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + auto.ni.10.goal + auto.ni.9.goal + NI.nature.plants.goal + auto.ni.2.goal + auto.ni.6.goal + NI.place.goal + auto.ni.4.goal + auto.ni.7.goal + D.goal + Px3Sg.goal + auto.ni.1.goal + auto.ndi.1.goal + NDI.Body.goal + auto.ni.8.goal + auto.ni.5.goal + Px3Pl.goal + Px1Sg.goal + Der.Dim.goal + Px12Pl.goal + auto.ni.11.goal, data = subset(AWnimp, TI))

### Check cochrane condition; step 1) remove things that aren't about 1
for (i in TI.Univariate.indcnj.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 0 of expected are <5
issues.ti.indcnj<-lapply(TI.Univariate.indcnj.0$univariate,cochran_indcnj)
str(issues.ti.indcnj)

TI.Univariate.indcnj <- nominal(TopOrder ~  PV.Time + PV.Position + PV.Move + PV.WantCan + PV.Discourse + PV.Qual + PV.StartFin + TI.do + TI.cognitive + TI.speech + TI.money.count + RdplW + RdplS +  auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.na.5.actor + auto.vti.3 + auto.ni.10.goal + auto.ni.9.goal + auto.ni.2.goal + auto.ni.6.goal + auto.nda.1.actor + auto.ni.4.goal + auto.na.7.actor + auto.ni.7.goal + auto.ni.1.goal + auto.ndi.1.goal + auto.ni.8.goal + auto.na.1.actor + auto.ni.5.goal + NA.persons.actor + auto.na.5.actor + Pro Sg.actor + Pers.actor + Pl.actor + Err.Orth.actor + D.actor + NDA.Relations.actor + Dem.actor + Px1Sg.actor + auto.nda.1.actor + auto.na.7.actor + Incl.actor + Prox.actor + auto.na.1.actor + actor.3 + actor.1 + actor.2 + actor.4 + goal.3 + Sg.goal + NI.object.goal + N.goal + auto.ni.3.goal + Dem.goal + Pron.goal + Pl.goal + Med.goal + Err.Orth.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + auto.ni.10.goal + auto.ni.9.goal + NI.nature.plants.goal + auto.ni.2.goal + auto.ni.6.goal + NI.place.goal + auto.ni.4.goal + auto.ni.7.goal + D.goal + Px3Sg.goal + auto.ni.1.goal + auto.ndi.1.goal + NDI.Body.goal + auto.ni.8.goal + auto.ni.5.goal, data = subset(AWnimp, TI))



###Write to file
sink(file = 'indcnj/TIUni-indcnj.txt')
print(summary(TI.Univariate.indcnj), max.print = NA)
sink(NULL)

sink(file = 'indcnj/issues.ti.indcnj.txt')
print(issues.ti.indcnj, max.print = NA)
sink(NULL)
###Make significance table
TI.Uni.indcnj.Sig<- subset(summary(TI.Univariate.indcnj)$sumry.table, alpha.X2 <0.05)

sink(file = 'indcnj/TIUniSig-indcnj.txt')
print(TI.Uni.indcnj.Sig, max.print = NA)
sink(NULL)


###=====================================================================================================
###=====================================================================================================
### TA ANALYSIS
###=====================================================================================================
paste(paste(names(which(sort(colSums(subset(AWnimp, TA)[grep("PV.[A-Z]", names(subset(AWnimp, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, TA)[grep("^TA...*", names(subset(AWnimp, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, TA)[grep("^auto.*", names(subset(AWnimp, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWnimp, TA)[grep("actor$", names(subset(AWnimp, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, TA)[grep("^actor", names(subset(AWnimp, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, TA)[grep("^goal", names(subset(AWnimp, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWnimp, TA)[grep("goal$", names(subset(AWnimp, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

TA.Univariate.indcnj.0 <- nominal(TopOrder ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + PV.StartFin + TA.speech + TA.do + TA.cognitive + TA.food + TA.money.count + TA.allow + TA.religion + auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.nda.1.goal + auto.na.5.actor + auto.vta.4 + auto.vta.2 + auto.na.7.goal + auto.nda.1.actor + auto.na.3.goal + auto.na.2.goal + auto.na.7.actor + auto.nda.3.goal + auto.na.1.goal + auto.na.6.goal + auto.na.1.actor + auto.nda.3.actor + auto.vta.7 +   NA.persons.actor + Sg.actor + auto.na.5.actor + D.actor + Pro NDA.Relations.actor + auto.nda.1.actor + Px1Sg.actor + Err.Orth.actor + Dem.actor + Pers.actor + Pl.actor + Obv.actor + auto.na.7.actor + Prox.actor + Med.actor + auto.na.1.actor + auto.nda.3.actor + Incl.actor + Px3Sg.actor + actor.3 + actor.1 + actor.2 + actor.4 + goal.3 + goal.4 + goal.1 + goal.2 + goal.5 + NA.persons.goal + auto.na.5.goal + Sg.goal + Obv.goal + Pron.goal + D.goal + NDA.Relations.goal + Pl.goal + auto.nda.1.goal + Dem.goal + awa.goal + Err.Orth.goal + Px1Sg.goal + Prox.goal + NA.beast.of.burden.goal + auto.na.7.goal + Med.goal + auto.na.3.goal + auto.na.2.goal + Px3Sg.goal + Pers.goal + auto.nda.3.goal + auto.na.1.goal + NA.food.goal + auto.na.6.goal + Px3Pl.goal + Px12Pl.goal + Incl.goal + NA.religious.goal + NA.money.count.goal, data = subset(AWnimp, TA))

### Check cochrane condition; step 1) remove things that aren't about 1
for (i in TA.Univariate.indcnj.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 0 of expected are <5
issues.ta.indcnj<-lapply(TA.Univariate.indcnj.0$univariate,cochran_indcnj)
str(issues.ta.indcnj)



TA.Univariate.indcnj <- nominal(TopOrder ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + PV.StartFin + TA.speech + TA.do + TA.cognitive + TA.food + TA.money.count + TA.allow + auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.nda.1.goal + auto.na.5.actor + auto.vta.4 + auto.vta.2 + auto.na.7.goal + auto.nda.1.actor + auto.na.3.goal + auto.na.2.goal + auto.na.7.actor + auto.nda.3.goal + auto.na.1.goal + auto.na.6.goal + auto.na.1.actor + auto.nda.3.actor + auto.vta.7 + NA.persons.actor + Sg.actor + auto.na.5.actor + D.actor + Pro NDA.Relations.actor + auto.nda.1.actor + Px1Sg.actor + Err.Orth.actor + Dem.actor + Pers.actor + Pl.actor + Obv.actor + auto.na.7.actor + Prox.actor + Med.actor + auto.na.1.actor + auto.nda.3.actor + Incl.actor + actor.3 + actor.1 + actor.2 + actor.4 + goal.3 + goal.4 + goal.1 + goal.2 + NA.persons.goal + auto.na.5.goal + Sg.goal + Obv.goal + Pron.goal + D.goal + NDA.Relations.goal + Pl.goal + auto.nda.1.goal + Dem.goal + Err.Orth.goal + Px1Sg.goal + Prox.goal + NA.beast.of.burden.goal + auto.na.7.goal + Med.goal + auto.na.3.goal + auto.na.2.goal + Px3Sg.goal + Pers.goal + auto.nda.3.goal + auto.na.1.goal + NA.food.goal + auto.na.6.goal + Px3Pl.goal + Px12Pl.goal, data = subset(AWnimp, TA))


###Write to file
sink(file = 'indcnj/TAUni-indcnj.txt')
print(summary(TA.Univariate.indcnj), max.print = NA)
sink(NULL)

sink(file = 'indcnj/issues.ta.indcnj.txt')
print(issues.ta.indcnj, max.print = NA)
sink(NULL)
###Make significance table
TA.Uni.indcnj.Sig<- subset(summary(TA.Univariate.indcnj)$sumry.table, alpha.X2 <0.05)

sink(file = 'indcnj/TAUniSig-indcnj.txt')
print(TA.Uni.indcnj.Sig, max.print = NA)
sink(NULL)


