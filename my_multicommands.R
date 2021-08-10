#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glmer <- glmer(Ind ~ II.sense + II.weather + NI.object.actor + (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ii.txt')
print(summary(ii.ive.glmer)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.ive.glmer <- glmer(Ind ~ PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Time + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.plural + RdplW + NA.persons.actor + Pl.actor + NDA.Relations.actor + actor.1 + actor.2 + actor.4 + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ai.txt')
print(summary(ai.ive.glmer)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.ive.glmer <- glmer(Ind ~ PV.Time + PV.Qual + PV.Discourse + TI.cognitive + TI.speech + NA.persons.actor + actor.1 + actor.2 + actor.4 + Sg.goal + NI.object.goal + Pl.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal + Px1Sg.goal + Px3Pl.goal + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ti.txt')
print(summary(ti.ive.glmer)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.ive.glmer <- glmer(Ind ~ PV.Move + PV.Discourse + PV.Position + PV.Qual + PV.Time + TA.speech + TA.do + TA.cognitive + TA.food +  Sg.actor + D.actor + Pers.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + NA.beast.of.burden.goal + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ta.txt')
print(summary(ti.ive.glmer)$coefficients, max.print = NA)
sink(NULL)
#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glmer <- glmer(PV.e ~ II.sense + NI.object.actor + II.natural.land + II.weather + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ii.txt')
print(summary(ii.e.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)


### VAI
ai.e.cnjtype.glmer <- glmer(PV.e ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + Pers.actor + Prox.actor + Indef.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ai.txt')
print(summary(ai.e.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.e.cnjtype.glmer <- glmer(PV.e ~ PV.Time + PV.Position + PV.Discourse + PV.WantCan + PV.Qual + TI.cognitive + TI.speech + NA.persons.actor + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + NI.object.goal + Pl.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ti.txt')
print(summary(ti.e.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.e.cnjtype.glmer <- glmer(PV.e ~ PV.Time + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + RdplS + Sg.actor + D.actor + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + NDA.Relations.goal + Prox.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal  + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ta.txt')
print(summary(ta.e.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)

#-------------------------------------------------------
### VII
ii.kaa.cnjtype.glmer <- glmer(PV.kaa ~ II.sense + NI.object.actor + II.natural.land + II.weather + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ii.txt')
print(summary(ii.kaa.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.kaa.cnjtype.glmer <- glmer(PV.kaa ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + Pers.actor + Prox.actor + Indef.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ai.txt')
print(summary(ai.kaa.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.kaa.cnjtype.glmer <- glmer(PV.kaa ~ PV.Time + PV.Position + PV.Discourse + PV.WantCan + PV.Qual + TI.cognitive + TI.speech + NA.persons.actor + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + NI.object.goal + Pl.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ti.txt')
print(summary(ti.kaa.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.kaa.cnjtype.glmer <- glmer(PV.kaa ~ PV.Time + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + RdplS + Sg.actor + D.actor + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + NDA.Relations.goal + Prox.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ta.txt')
print(summary(ta.kaa.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)

#-------------------------------------------------------
### VII
ii.other.cnjtype.glmer <- glmer(OtherCnj ~ II.sense + NI.object.actor + II.natural.land + II.weather + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ii.txt')
print(summary(ii.other.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.other.cnjtype.glmer <- glmer(OtherCnj ~  PV.Time + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + Pers.actor + Prox.actor + Indef.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ai.txt')
print(summary(ai.other.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.other.cnjtype.glmer <- glmer(OtherCnj ~ PV.Time + PV.Position + PV.Discourse + PV.WantCan + PV.Qual + TI.cognitive + TI.speech + NA.persons.actor + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + NI.object.goal + Pl.goal + Err.Orth.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ti.txt')
print(summary(ti.other.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.other.cnjtype.glmer <- glmer(OtherCnj ~ PV.Time + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + RdplS + Sg.actor + D.actor + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + NDA.Relations.goal + Prox.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ta.txt')
print(summary(ta.other.cnjtype.glmer)$coefficients, max.print = NA)
sink(NULL)

#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glmer <- glmer(Ind ~ auto.vii.5 + (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ii.txt')
print(summary(ii.indcnj.glmer)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.indcnj.glmer <- glmer(Ind ~ PV.Move + PV.Time + PV.Position + PV.Qual + PV.Discourse + PV.StartFin + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.pray + RdplW + NA.persons.actor + Pron.actor + Pl.actor + Pers.actor + actor.3 + actor.1 + actor.2 + actor.4 + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ai.txt')
print(summary(ai.indcnj.glmer)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.indcnj.glmer <- glmer(Ind ~ PV.Time + PV.Discourse + PV.Qual + TI.cognitive + NA.persons.actor + actor.3 + actor.1 + actor.2 + actor.4 + Sg.goal + NI.object.goal + N.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ti.txt')
print(summary(ti.indcnj.glmer)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.indcnj.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TA.speech + TA.do + TA.cognitive + TA.food + TA.money.count + Sg.actor + actor.1 + actor.2 + actor.4 + goal.4 + goal.2 + NA.persons.goal + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ta.txt')
print(summary(ta.indcnj.glmer)$coefficients, max.print = NA)
sink(NULL)

