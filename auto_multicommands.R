#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glmer.auto <- glmer(Ind ~ auto.ni.3.actor + (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))
sink(file = 'SummaryTables/auto/ive-ii-auto.txt')
print(summary(ii.ive.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.ive.glmer.auto <- glmer(Ind ~ PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Time + RdplW + auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.na.5.actor + auto.vai.11 + auto.vai.9 + auto.nda.1.actor + auto.vai.3 + auto.vai.13 + auto.vai.4 + auto.vai.8 + auto.vai.7 + auto.vai.10 + Pl.actor + actor.3 + actor.1 + actor.2 + actor.4 + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
sink(file = 'SummaryTables/auto/ive-ai-auto.txt')
print(summary(ai.ive.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.ive.glmer.auto <- glmer(Ind ~ PV.Time + PV.Qual + PV.Discourse + auto.vti.4 + auto.vti.2 + auto.na.5.actor + auto.ni.10.goal + auto.ni.9.goal + auto.ni.2.goal + actor.3 + actor.2 + actor.4 + Sg.goal + Pl.goal + Px1Sg.goal + Px3Pl.goal + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
sink(file = 'SummaryTables/auto/ive-ti-auto.txt')
print(summary(ti.ive.glmer.auto)$coefficients, max.print = NA)
sink(NULL)


### VTA
ta.ive.glmer.auto <- glmer(Ind ~ PV.Move + PV.Discourse + PV.Position + PV.Qual + PV.Time + auto.vta.6 + auto.vta.3 + auto.vta.1 + auto.na.5.goal + auto.vta.4 + auto.vta.2 + auto.na.2.goal + Sg.actor + D.actor + Pers.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.4  + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))
sink(file = 'SummaryTables/auto/ive-ta-auto.txt')
print(summary(ti.ive.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glmer.auto <- glmer(PV.e ~ auto.ni.3.actor + auto.vii.3 + auto.vii.1 + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))
sink(file = 'SummaryTables/auto/e-cnj-ii-auto.txt')
print(summary(ii.e.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.e.cnjtype.glmer.auto <- glmer(PV.e ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + auto.vai.2 + auto.vai.6 + auto.na.5.actor + auto.vai.11 + auto.nda.1.actor + auto.na.7.actor + auto.na.1.actor + Sg.actor + Pl.actor + Prox.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
sink(file = 'SummaryTables/auto/e-cnj-ai-auto.txt')
print(summary(ai.e.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)



### VTI
ti.e.cnjtype.glmer.auto <- glmer(PV.e ~ PV.Time + PV.Position + PV.Discourse + PV.WantCan + PV.Qual + auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.ni.10.goal + auto.ni.2.goal + auto.ni.4.goal + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + Pl.goal + Prox.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
sink(file = 'SummaryTables/auto/e-cnj-ti-auto.txt')
print(summary(ti.e.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)


### VTA
ta.e.cnjtype.glmer.auto <- glmer(PV.e ~ PV.Time + PV.Position + PV.Discourse + RdplS + auto.vta.6 + auto.vta.3 + auto.vta.1 + auto.vta.5 + auto.nda.1.goal + auto.vta.4 + auto.vta.2 + Sg.actor + D.actor + Prox.actor + actor.3 + actor.1 + actor.2 + goal.4 + goal.1 + goal.2 + Sg.goal + Prox.goal + Px1Sg.goal + Px3Sg.goal + Pers.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))
sink(file = 'SummaryTables/auto/e-cnj-ta-auto.txt')
print(summary(ta.e.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)



#-------------------------------------------------------
### VII
ii.kaa.cnjtype.glmer.auto <- glmer(PV.kaa ~ auto.ni.3.actor + auto.vii.3 + auto.vii.1 + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/auto/kaa-cnj-ii-auto.txt')
print(summary(ii.kaa.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)


### VAI
ai.kaa.cnjtype.glmer.auto <- glmer(PV.kaa ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + auto.vai.2 + auto.vai.6 + auto.na.5.actor + auto.vai.11 + auto.nda.1.actor + auto.na.7.actor + auto.na.1.actor + Sg.actor + Pl.actor + Prox.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/auto/kaa-cnj-ai-auto.txt')
print(summary(ai.kaa.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.kaa.cnjtype.glmer.auto <- glmer(PV.kaa ~ PV.Time + PV.Position + PV.Discourse + PV.WantCan + PV.Qual + auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.ni.10.goal + auto.ni.2.goal + auto.ni.4.goal + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + Pl.goal + Prox.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/auto/kaa-cnj-ti-auto.txt')
print(summary(ti.kaa.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.kaa.cnjtype.glmer.auto <- glmer(PV.kaa ~ PV.Time + PV.Position + PV.Discourse + RdplS + auto.vta.6 + auto.vta.3 + auto.vta.1 + auto.vta.5 + auto.nda.1.goal + auto.vta.4 + auto.vta.2 + Sg.actor + D.actor + Prox.actor + actor.3 + actor.1 + actor.2 + goal.4 + goal.1 + goal.2 + Sg.goal + Prox.goal + Px1Sg.goal + Px3Sg.goal + Pers.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/auto/kaa-cnj-ta-auto.txt')
print(summary(ta.kaa.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

#-------------------------------------------------------
### VII
ii.other.cnjtype.glmer.auto <- glmer(OtherCnj ~ auto.ni.3.actor + auto.vii.3 + auto.vii.1 + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/auto/other-cnj-ii-auto.txt')
print(summary(ii.other.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.other.cnjtype.glmer.auto <- glmer(OtherCnj ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + auto.vai.2 + auto.vai.6 + auto.na.5.actor + auto.vai.11 + auto.nda.1.actor + auto.na.7.actor + auto.na.1.actor + Sg.actor + Pl.actor + Prox.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/auto/other-cnj-ai-auto.txt')
print(summary(ai.other.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.other.cnjtype.glmer.auto <- glmer(OtherCnj ~ PV.Time + PV.Position + PV.Discourse + PV.WantCan + PV.Qual + auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.ni.10.goal + auto.ni.2.goal + auto.ni.4.goal + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + Pl.goal + Prox.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/auto/other-cnj-ti-auto.txt')
print(summary(ti.other.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.other.cnjtype.glmer.auto <- glmer(OtherCnj ~ PV.Time + PV.Position + PV.Discourse + RdplS + auto.vta.6 + auto.vta.3 + auto.vta.1 + auto.vta.5 + auto.nda.1.goal + auto.vta.4 + auto.vta.2 + Sg.actor + D.actor + Prox.actor + actor.3 + actor.1 + actor.2 + goal.4 + goal.1 + goal.2 + Sg.goal + Prox.goal + Px1Sg.goal + Px3Sg.goal + Pers.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))


sink(file = 'SummaryTables/auto/other-cnj-ta-auto.txt')
print(summary(ta.other.cnjtype.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glmer.auto <- glmer(Ind ~ auto.vii.5 + (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/auto/ivc-ii-auto.txt')
print(summary(ii.indcnj.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.indcnj.glmer.auto <- glmer(Ind ~ PV.Move + PV.Time + PV.Position + PV.Qual + PV.Discourse + PV.StartFin + RdplW + auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.na.5.actor + auto.vai.11 + auto.vai.9 + auto.vai.3 + auto.vai.13 + auto.vai.4 + auto.vai.8 + auto.vai.7 + auto.vai.10 + Pl.actor + actor.1 + actor.2 + actor.4 + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/auto/ivc-ai-auto.txt')
print(summary(ai.indcnj.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.indcnj.glmer.auto <- glmer(Ind ~ PV.Time + PV.Discourse + PV.Qual + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.na.5.actor + auto.ni.10.goal + auto.ni.9.goal + auto.ni.2.goal + actor.3 + actor.1 + actor.2 + actor.4 + Sg.goal + Pl.goal + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/auto/ivc-ti-auto.txt')
print(summary(ti.indcnj.glmer.auto)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.indcnj.glmer.auto <- glmer(Ind ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.vta.4 + Sg.actor + actor.1 + actor.2 + actor.4 + goal.3 + goal.4 + goal.2 + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))


sink(file = 'SummaryTables/auto/ivc-ta-auto.txt')
print(summary(ta.indcnj.glmer.auto)$coefficients, max.print = NA)
sink(NULL)
