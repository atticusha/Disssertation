# BiVariate Commands
#=======================================================================
## ind vs. eCnj
#=======================================================================
### VII
II.ive.Bivariate.initial <- nominal(. ~ PV.Time + II.sense + auto.ni.3.actor + auto.vai.6 + auto.vii.3 + auto.vii.4 + auto.vai.1 + Sg.actor + NI.object.actor + Dem.actor + Pron.actor + Med.actor, data = subset(AWive, II))
subset(II.ive.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
II.ive.Bivariate <- nominal(. ~ , data = subset(AWive, II))
#### 

### VAI
AI.ive.Bivariate.initial <- nominal(. ~ RdplW + PV.Time + PV.Move + PV.StartFinWantCan + PV.Qual + PV.StartFin + PV.Discourse + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.vai.11 + auto.na.5.actor + auto.vai.9 + auto.vai.3 + auto.vai.13 + auto.vai.4 + auto.vai.8 + auto.vai.7 + auto.na.3.actor + A.actor + N.actor + NA.persons.actor + ACTOR.right.actor + Pl.actor + NA.beast.of.burden.actor + Obv.actor + pn.1Pl.actor + NI.object.actor + actor.3 + actor.1 + actor.4 , data = subset(AWCnj, AI))
subset(AI.ive.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
AI.ive.Bivariate <- nominal(. ~ , data = subset(AWive, AI))
#### still colinearity between a bunch of semantic classes

### VTI
TI.ive.Bivariate.initial <- nominal(. ~ RdplS + PV.Time + PV.Discourse + TI.do + TI.cognitive + TI.money.count + auto.vti.4 + auto.vti.2 + auto.na.5.actor + auto.ni.9.goal + auto.ni.2.goal + auto.ni.10.goal + Pron.actor + Pers.actor + pn.1Sg.actor + pn.3Pl.actor + actor.3 + actor.1 + actor.2 + I.goal + N.goal + Pl.goal + NI.natural.force.goal + NI.place.goal, data = subset(AWive, TI))
subset(TI.ive.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TI.ive.Bivariate <- nominal(. ~ , data = subset(AWive, TI))
#### still colinearity between a bunch of semantic classes

### VTA
TA.ive.Bivariate.initial <- nominal(. ~ PV.Time + PV.Move + PV.Discourse + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + auto.vta.6 + auto.vta.3 + auto.vta.1 + auto.na.5.goal + auto.vta.4 + auto.vta.2 + auto.na.7.actor + A.actor + N.actor + Sg.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + Obv.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + A.goal + N.goal + Obv.goal + Px3Sg.goal + Px3Pl.goal, data = subset(AWive, TA))
subset(TA.ive.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TA.ive.Bivariate <- nominal(. ~ , data = subset(AWive, TA))

#### still colinearity between a bunch of semantic classes

#=======================================================================
## eCnj vs kaa vs other
#=======================================================================
### VII
II.ekaaother.Bivariate.initial <- nominal(. ~ PV.Time + II.sense + II.natural.land + II.weather + auto.vii.1 + auto.ni.3.actor + auto.vai.6 + I.actor + Sg.actor + NI.object.actor + N.actor + actor.3 + actor.4 , data = subset(AWCnj, II))
subset(II.ekaaother.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
II.ekaaother.Bivariate <- nominal(. ~ , data = subset(AWCnj, II))
#### still colinearity between a bunch of semantic classes


### VAI
AI.ekaaother.Bivariate.initial <- nominal(. ~ RdplW + PV.Time + PV.StartFinWantCan + PV.Position + PV.WantCan + AI.do + AI.reflexive + AI.cooking + AI.health + AI.pray + auto.vai.6 + auto.na.5.actor + auto.nda.1.actor + auto.na.7.actor + auto.vai.1 + auto.na.3.actor + auto.na.1.actor + A.actor + N.actor + NA.persons.actor + left.ACTOR.actor + Sg.actor + Pron.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + Dem.actor + Med.actor + actor.3 + actor.1 + actor.2, data = subset(AWCnj, AI))
subset(AI.ekaaother.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
AI.ekaaother.Bivariate <- nominal(. ~  , data = subset(AWCnj, AI))
#### still colinearity between a bunch of semantic classes

### VTI
TI.ekaaother.Bivariate.initial <- nominal(. ~ PV.Time + PV.StartFinWantCan + PV.Discourse + PV.Position + PV.WantCan + TI.do + TI.cognitive + TI.speech + auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + NA.persons.actor + A.actor + N.actor + Sg.actor + actor.1 + actor.2 + I.goal + Sg.goal + N.goal + NI.object.goal + Dem.goal + Pron.goal + Prox.goal + NI.nominal.goal, data = subset(AWCnj, TI))
subset(TI.ekaaother.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TI.ekaaother.Bivariate <- nominal(. ~  , data = subset(AWCnj, TI))
#### still colinearity between a bunch of semantic classes

### VTA
TA.ekaaother.Bivariate.initial <- nominal(. ~ PV.Time + PV.Position + TA.speech + TA.cognitive + TA.food + auto.vta.6 + auto.vta.5 + auto.na.5.goal + auto.vta.4 + auto.vta.2 + actor.3 + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + NA.persons.goal + Sg.goal + Px1Sg.goal + Dem.goal + awa.goal + Prox.goal, data = subset(AWCnj, TA))
subset(TA.ekaaother.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TA.ekaaother.Bivariate <- nominal(. ~  , data = subset(AWCnj, TA))
#### still colinearity between a bunch of semantic classes


#=======================================================================
## Ind vs Cnj as whole
#=======================================================================
### VII
II.ivc.Bivariate.initial <- nominal(. ~ PV.Time + II.sense + auto.ni.3.actor + auto.vai.6 + auto.vii.3 + auto.vii.4 + NI.object.actor + Dem.actor + Pron.actor, data = subset(AWCnj, II))
subset(II.ive.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
II.ivc.Bivariate <- nominal(. ~  , data = subset(AWCnj, II))
#### still colinearity between a bunch of semantic classes

### VAI
AI.ivc.Bivariate.initial <- nominal(. ~ RdplW + PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Position + PV.Discourse + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.vai.11 + auto.na.5.actor + auto.vai.9 + auto.vai.3 + auto.vai.4 + auto.vai.13 + auto.vai.8 + auto.vai.7 + auto.vai.10 + auto.na.3.actor + A.actor + N.actor + NA.persons.actor + Sg.actor + Pron.actor + Pl.actor + NA.beast.of.burden.actor + Obv.actor + pn.1Pl.actor + actor.3 + actor.1 + actor.2 + actor.4 , data = subset(AWCnj, AI))
subset(AI.ivc.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
AI.ivc.Bivariate <- nominal(. ~  , data = subset(AWCnj, AI))
#### still colinearity between a bunch of semantic classes

### VTI
TI.ivc.Bivariate.initial <- nominal(. ~ RdplS + PV.Time + PV.Discourse + TI.do + TI.cognitive + auto.vti.4 + auto.vti.2 + auto.na.5.actor + auto.ni.9.goal + auto.ni.2.goal + auto.ni.10.goal + Pron.actor + Pers.actor + pn.1Sg.actor + actor.3 + actor.1 + actor.2 + I.goal + Sg.goal + N.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + NI.place.goal, data = subset(AWCnj, TI))
subset(TI.ive.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TI.ivc.Bivariate <- nominal(. ~  , data = subset(AWCnj, TI))
#### still colinearity between a bunch of semantic classes

### VTA
TA.ivc.Bivariate.initial <- nominal(. ~ PV.Time + PV.Move + PV.StartFinWantCan + PV.Discourse + PV.Position + TA.speech + TA.do + TA.cognitive + TA.food + TA.money.count + auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.vta.4 + auto.nda.1.actor + auto.na.7.actor + auto.vta.7 + A.actor + N.actor + NA.persons.actor + Sg.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + Obv.actor + actor.3 + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + A.goal + N.goal + NA.persons.goal + Obv.goal + Px3Sg.goal , data = subset(AWCnj, TA))
subset(TA.ive.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TA.ivc.Bivariate <- nominal(. ~ , data = subset(AWCnj, TA))
#### still colinearity between a bunch of semantic classes


#=======================================================================
## Ind vs eCnj vs kaaCnj vs OtherCnj
#=======================================================================
### VII
II.all.Bivariate.initial <- nominal(. ~ PV.Time + II.sense + II.natural.land + II.weather + auto.vii.1 + auto.ni.3.actor + auto.vii.3 + auto.vii.4 + I.actor + Sg.actor + N.actor + NI.object.actor + Dem.actor + Pron.actor, data = subset(AWnimp, II))
subset(II.all.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
II.all.Bivariate <- nominal(. ~ , data = subset(AWnimp, II))
#### still colinearity between a bunch of semantic classes

### VAI
AI.all.Bivariate.initial <- nominal(. ~ RdplW + PV.Time + PV.Move + PV.StartFinWantCan + PV.Qual + PV.StartFin + PV.Position + PV.Discourse + PV.WantCan + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.vai.11 + auto.na.5.actor + auto.vai.9 + auto.vai.3 + auto.nda.1.actor + auto.vai.4 + auto.vai.13 + auto.na.7.actor + auto.vai.1 + auto.vai.8 + auto.vai.7 + auto.vai.10 + auto.na.1.actor + auto.na.3.actor + A.actor + N.actor + NA.persons.actor + Sg.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + Dem.actor + Med.actor + Obv.actor + pn.1Pl.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWnimp, AI))
subset(AI.all.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
AI.all.Bivariate <- nominal(. ~ , data = subset(AWnimp, AI))
#### still colinearity between a bunch of semantic classes

### VTI
TI.all.Bivariate.initial <- nominal(. ~ PV.Time + PV.Discourse + PV.WantCan + TI.do + TI.cognitive + TI.speech + auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.na.5.actor + auto.ni.9.goal + auto.ni.2.goal + auto.ni.10.goal + auto.ni.4.goal + auto.na.1.actor + NA.persons.actor + A.actor + N.actor + Sg.actor + Pers.actor + pn.1Sg.actor + Dem.actor + niya.actor + pn.3Pl.actor + Indef.actor + actor.3 + actor.1 + actor.2 + actor.4 + I.goal + Sg.goal + N.goal + NI.object.goal + Dem.goal + Pron.goal + Pl.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + Px12Pl.goa, data = subset(AWnimp, TI))
subset(TI.all.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TI.all.Bivariate <- nominal(. ~ , data = subset(AWnimp, TI))
#### still colinearity between a bunch of semantic classes

### VTA
TA.all.Bivariate.initial <- nominal(. ~ PV.Time + PV.Discourse + TA.speech + TA.do + TA.cognitive + TA.food + auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.vta.4 + auto.vta.2 + auto.nda.1.actor + auto.vta.7 + auto.na.1.goal + A.actor + N.actor + Sg.actor + left.ACTOR.actor + ACTOR.right.actor + D.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + A.goal + N.goal + NA.persons.goal + Sg.goal + Obv.goal + Dem.goal + awa.goal + Prox.goal + Px3Sg.goal + Px12Pl.goal, data = subset(AWnimp, TA))
subset(TA.all.Bivariate.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TA.all.Bivariate <- nominal(. ~ , data = subset(AWnimp, TA))
#### still colinearity between a bunch of semantic classes``