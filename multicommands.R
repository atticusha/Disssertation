
#============================================================================================================
### VAI
ii.ind.v.ecnj.glmer <- glmer(Ind ~ + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
### VAI
ai.ind.v.ecnj.glmer <- glmer(Ind ~  + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
### VTI
ti.ind.v.ecnj.glmer <- glmer(Ind ~  + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
### VTA
ta.ind.v.ecnj.glmer <- glmer(Ind ~  + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))









## eCnj vs all other Cnj types (use AWCnj data)
#=====================================================

### VII
####keep kîkway.actor ewvern at 49 
#------------------------------------------------------------------------------------------------
ii.ecnj.cnj.glmer <- glmer(PV.e ~ PV.ki + II.natural.land + I.actor + Sg.actor + N.actor + NI.object.actor + NI.object.actor +  auto.ni.3.actor + left.ACTOR.actor + ACTOR.right.actor + kîkway.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))
ii.kaa.cnj.glmer <- glmer(PV.kaa ~ Actor.3Sg + Actor.3Pl + PV.ki + II.natural.land + I.actor + Sg.actor + N.actor + NI.object.actor + NI.object.actor +  auto.ni.3.actor + left.ACTOR.actor + ACTOR.right.actor + kîkway.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#ii.other.cnj.glmer <- glmer(OtherCnj ~ Actor.3Sg + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#ii.other.cnj.glmer3 <- glmer(OtherCnj ~ Actor.3Sg + Actor.3Pl + Actor.4Sg + PV.ki + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#ii.other.cnj.glmer1 <- glmer(OtherCnj ~ Actor.3Sg + Actor.3Pl + Actor.4Sg + PV.ki + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

ii.other.cnj.glmer <- glmer(OtherCnj ~ Actor.3Sg + Actor.3Pl + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VAI (remove under 50)
#------------------------------------------------------------------------------------------------
ai.ecnj.cnj.glmer <- glmer(PV.e ~ Actor.2Sg + Actor.3Sg + Actor.1Pl + Actor.12Pl + PV.ki + PV.wi + PV.ati + PV.ohci + NA.persons.actor + Sg.actor + ACTOR.right.actor + auto.na.3.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
ai.kaa.cnj.glmer <- glmer(PV.kaa ~ Actor.12Pl + PV.ki + PV.pe + PV.wi + PV.ati + PV.ohci + AI.do + AI.cooking + NA.persons.actor + Sg.actor + ACTOR.right.actor + auto.na.7.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
ai.other.cnj.glmer <- glmer(OtherCnj ~ Actor.2Sg + Actor.3Sg + Actor.1Pl + Actor.12Pl + PV.ki + PV.wi + AI.do + AI.reflexive + AI.cooking + AI.health + AI.pray + NA.persons.actor + NDA.Relations.actor + auto.na.7.actor + auto.na.3.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))


### VTI(remove under 50)
#------------------------------------------------------------------------------------------------
ti.ecnj.cnj.glmer <- glmer(PV.e ~ Actor.2Sg + Actor.1Pl + Actor.12Pl + PV.ki + TI.cognitive + TI.speech + A.actor + Sg.goal + NI.object.goal + GOAL.right.goal + left.GOAL.goal + Prox.goal + Med.goal + NI.nominal.goal + auto.ni.4.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
ti.kaa.cnj.glmer <- glmer(PV.kaa ~ PV.ki + TI.speech + NA.persons.actor + A.actor + awa.actor + Sg.goal + N.goal + NI.object.goal + GOAL.right.goal + Prox.goal + Med.goal + NI.nominal.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#### below doesnt work, remove Actor.12Pl  as it is only 54 count
#### ti.other.cnj.glmer <- glmer(OtherCnj ~ Actor.2Sg + Actor.1Pl + Actor.12Pl + PV.ki + PV.wi + TI.cognitive + NI.nominal.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))
ti.other.cnj.glmer <- glmer(OtherCnj ~ Actor.2Sg + Actor.1Pl + PV.ki + PV.wi + TI.cognitive + NI.nominal.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA(remove under 50)
#------------------------------------------------------------------------------------------------
ta.kaa.cnj.glmer <- glmer(Ind ~ + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

ta.ecnj.cnj.glmer <- glmer(PV.e ~ Goal.1Sg + Actor.2Sg + Actor.3Sg + Goal.3Sg + Actor.1Pl + Actor.3Pl + Goal.4Sg.Pl + PV.ki + TA.speech + TA.cognitive + TA.food + NA.persons.goal + Sg.goal + Dem.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))
ta.kaa.cnj.glmer <- glmer(PV.kaa ~ Actor.1Sg + Actor.3Sg + Goal.3Sg + Actor.1Pl + Actor.3Pl + Goal.4Sg.Pl + PV.ki + PV.pe + PV.wi + TA.speech + TA.cognitive + NA.persons.goal + Sg.goal + Dem.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#### doesn't converge, both 2sg actor and ayisiwiniw.goal have 52, but removing the former helps while the latter doesn't converge
#### ta.other.cnj.glmer <- glmer(OtherCnj ~ Actor.1Sg + Goal.1Sg + Actor.2Sg + Actor.3Sg + Goal.3Sg + PV.ki + PV.pe + PV.wi + Px1Sg.goal + ayisiyiniw.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))
ta.other.cnj.glmer <- glmer(OtherCnj ~ Actor.1Sg + Goal.1Sg + Actor.3Sg + Goal.3Sg + PV.ki + PV.pe + PV.wi + Px1Sg.goal + ayisiyiniw.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

