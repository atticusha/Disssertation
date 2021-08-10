# BiVariate Commands
#=======================================================================
## ind vs. eCnj
#=======================================================================
### VII
II.ive.Bivariate.man.initial <- nominal(. ~ II.sense + II.weather + Sg.actor + NI.object.actor, data = subset(AWive, II))
subset(II.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#category1       category2  N1 N2 N12     alpha.X2
#6  Sg.actor NI.object.actor 118 90  84 1.931598e-59
II.ive.Bivariate.man <- nominal(. ~ II.sense + II.weather + NI.object.actor, data = subset(AWive, II))
#### 

### VAI
AI.ive.Bivariate.man.initial <- nominal(. ~ PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Time + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.plural + RdplW + NA.persons.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Pers.actor + Px3Sg.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWCnj, AI))
subset(AI.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1           category2  N1  N2 N12      alpha.X2
#199    NA.persons.actor          Pron.actor 710 383 383             0
#212          Pron.actor          Pers.actor 383 172 172             0
#226             D.actor NDA.Relations.actor 201 200 200             0
#228             D.actor         Px3Sg.actor 201  27  27  1.89821e-133
#234 NDA.Relations.actor         Px3Sg.actor 200  27  27 3.904697e-134
#247         Px3Sg.actor             actor.4  27 173  27 1.123939e-155

AI.ive.Bivariate.man <- nominal(. ~ PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Time + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.plural + RdplW + NA.persons.actor + Pl.actor + NDA.Relations.actor + actor.1 + actor.2 + actor.4, data = subset(AWive, AI))
#### still colinearity between a bunch of semantic classes

### VTI
TI.ive.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Qual + PV.Discourse + TI.do + TI.cognitive + TI.speech + NA.persons.actor + Pron.actor + Pers.actor + actor.3 + actor.1 + actor.2 + actor.4 + Sg.goal + NI.object.goal + Pl.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal + Px1Sg.goal + Px3Pl.goal, data = subset(AWive, TI))
subset(TI.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#category1    category2   N1   N2 N12      alpha.X2
#61             TI.do TI.cognitive 1307 1047   0             0
#112 NA.persons.actor   Pron.actor  263  176 176             0 2.530823e-208
#113 NA.persons.actor   Pers.actor  263  137 137 4.119694e-275 2.384693e-155
#127       Pron.actor   Pers.actor  176  137 137             0  4.84976e-194
#154          actor.3      actor.1 1128 1086   0 4.446591e-323

TI.ive.Bivariate.man <- nominal(. ~ PV.Time + PV.Qual + PV.Discourse + TI.cognitive + TI.speech + NA.persons.actor + actor.1 + actor.2 + actor.4 + Sg.goal + NI.object.goal + Pl.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal + Px1Sg.goal + Px3Pl.goal, data = subset(AWive, TI))
#### still colinearity between a bunch of semantic classes

### VTA
TA.ive.Bivariate.man.initial <- nominal(. ~ PV.Move + PV.Discourse + PV.Position + PV.Qual + PV.Time + TA.speech + TA.do + TA.cognitive + TA.food +  Sg.actor + D.actor + Pers.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + NA.beast.of.burden.goal, data = subset(AWive, TA))
subset(TA.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1 category2   N1   N2 N12      alpha.X2
#178   actor.3    goal.3 1097 1212   0             0
#206    goal.4  Obv.goal  602  295 294 1.456653e-235 

TA.ive.Bivariate.man <- nominal(. ~ PV.Move + PV.Discourse + PV.Position + PV.Qual + PV.Time + TA.speech + TA.do + TA.cognitive + TA.food +  Sg.actor + D.actor + Pers.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + NA.beast.of.burden.goal, data = subset(AWive, TA))





#=======================================================================
## Ind vs Cnj as whole
#=======================================================================
### VII
II.ivc.Bivariate.man.initial <- nominal(. ~ auto.vii.5, data = subset(AWnimp, II))
subset(II.ivc.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
II.ivc.Bivariate.man <- nominal(. ~ auto.vii.5, data = subset(AWnimp, II))
#### still colinearity between a bunch of semantic classes

### VAI
AI.ivc.Bivariate.man.initial <- nominal(. ~ PV.Move + PV.Time + PV.Position + PV.Qual + PV.Discourse + PV.StartFin + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.pray + RdplW + NA.persons.actor + Pron.actor + Pl.actor + Pers.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWnimp, AI))
subset(AI.ivc.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
category1  category2   N1   N2 N12 alpha.X2   
#204 NA.persons.actor Pron.actor  961  528 528
#212       Pron.actor Pers.actor  528  238 238
#226          actor.3    actor.1 4052 2047   0


AI.ivc.Bivariate.man <- nominal(. ~ PV.Move + PV.Time + PV.Position + PV.Qual + PV.Discourse + PV.StartFin + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.pray + RdplW + NA.persons.actor + Pl.actor + Pers.actor + actor.1 + actor.2 + actor.4, data = subset(AWnimp, AI))

### VTI
TI.ivc.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Discourse + PV.Qual + TI.do + TI.cognitive + NA.persons.actor + Pron.actor + Pers.actor + actor.3 + actor.1 + actor.2 + actor.4 + Sg.goal + NI.object.goal + N.goal + Dem.goal + Pron.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal, data = subset(AWnimp, TI))
subset(TI.ivc.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#category1    category2   N1   N2 N12      alpha.X2      alpha.G2          beta power effect.size likelihood.ratio cramers.v lambda.12 lambda.21
#64             TI.do TI.cognitive 1806 1276   0             0             0             0     1   0.8038365         2802.079 0.8032351 0.7771011 0.7131661
#101 NA.persons.actor   Pron.actor  348  223 223             0 1.565294e-262             0     1   0.7848358         1198.117 0.7828785 0.6408046 0.4394619
#102 NA.persons.actor   Pers.actor  348  156 156 3.248376e-316 2.975923e-174 4.194134e-287     1   0.6497161         791.9899 0.6473999 0.4482759         0
#118       Pron.actor   Pers.actor  223  156 156             0 4.707001e-219             0     1    0.827837         998.0748 0.8250002 0.6995516 0.5705128
#210   NI.object.goal     Dem.goal  966  510 510             0             0    4.348e-304     1   0.6678386         1553.765  0.666929 0.5279503 0.1058824
#211   NI.object.goal    Pron.goal  966  510 510             0             0    4.348e-304     1   0.6678386         1553.765  0.666929 0.5279503 0.1058824
#226         Dem.goal    Pron.goal  510  510 510             0             0             0     1           1         2889.905 0.9988494         1         1

TI.ivc.Bivariate.man <- nominal(. ~ PV.Time + PV.Discourse + PV.Qual + TI.cognitive + NA.persons.actor + actor.3 + actor.1 + actor.2 + actor.4 + Sg.goal + NI.object.goal + N.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal, data = subset(AWnimp, TI))

### VTA
TA.ivc.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TA.speech + TA.do + TA.cognitive + TA.food + TA.money.count + Sg.actor + actor.1 + actor.2 + actor.4 + goal.3 + goal.4 + goal.2 + NA.persons.goal, data = subset(AWnimp, TA))
subset(TA.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1 category2   N1   N2 N12 alpha.X2 
#166   actor.3    goal.3 1097 1212   0        0

TA.ivc.Bivariate.man <- nominal(. ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TA.speech + TA.do + TA.cognitive + TA.food + TA.money.count + Sg.actor + actor.1 + actor.2 + actor.4 + goal.4 + goal.2 + NA.persons.goal, data = subset(AWnimp, TA))



#=======================================================================
## eCnj vs kaa vs other
#=======================================================================
### VII
II.ekaaother.Bivariate.man.initial <- nominal(. ~ II.sense + NI.object.actor + II.natural.land + II.weather + Dem.actor + Pron.actor + Sg.actor, data = subset(AWCnj, II))
subset(II.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#category1  category2 N1 N2 N12      alpha.X2
#9  NI.object.actor  Dem.actor 76 27  27  3.405996e-33  
#10 NI.object.actor Pron.actor 76 27  27  3.405996e-33  
#11 NI.object.actor   Sg.actor 76 89  71  5.941085e-73 
#19       Dem.actor Pron.actor 27 27  27 1.687882e-101 

II.ekaaother.Bivariate.man <- nominal(. ~ II.sense + NI.object.actor + II.natural.land + II.weather, data = subset(AWCnj, II))


### VAI
AI.ekaaother.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + Sg.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Dem.actor + Pers.actor + Px1Sg.actor + Prox.actor + Indef.actor + actor.3 + actor.1 + actor.2, data = subset(AWCnj, AI))
subset(AI.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#category1           category2  N1  N2 N12      alpha.X2      alpha.G2
#164    NA.persons.actor          Pron.actor 710 383 383             0 
#191          Pron.actor           Dem.actor 383 186 186             0
#192          Pron.actor          Pers.actor 383 172 172             0 
#194          Pron.actor          Prox.actor 383 100 100 2.526952e-258 
#209             D.actor NDA.Relations.actor 201 200 200             0  
#212             D.actor         Px1Sg.actor 201 151 150             0 
#220 NDA.Relations.actor         Px1Sg.actor 200 151 149             0 
#228           Dem.actor          Prox.actor 186 100 100             0


AI.ekaaother.Bivariate.man <- nominal(. ~  PV.Time + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + Pers.actor + Prox.actor + Indef.actor + actor.3 + actor.1 + actor.2, data = subset(AWCnj, AI))

### VTI
TI.ekaaother.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Position + PV.Discourse + PV.WantCan + PV.Qual + TI.do + TI.cognitive + TI.speech + NA.persons.actor + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + NI.object.goal + Dem.goal + Pron.goal + Pl.goal + Err.Orth.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal, data = subset(AWCnj, TI))
subset(TI.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#category1    category2   N1  N2 N12      alpha.X2 
#101          TI.do TI.cognitive 1430 720   0             0
#226 NI.object.goal     Dem.goal  720 377 377 1.300872e-229
#227 NI.object.goal    Pron.goal  720 377 377 1.300872e-229
#233       Dem.goal    Pron.goal  377 377 377             0
#236       Dem.goal    Prox.goal  377 176 176 1.813735e-223
#241      Pron.goal    Prox.goal  377 176 176 1.813735e-223


TI.ekaaother.Bivariate.man <- nominal(. ~ PV.Time + PV.Position + PV.Discourse + PV.WantCan + PV.Qual + TI.cognitive + TI.speech + NA.persons.actor + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + NI.object.goal + Pl.goal + Err.Orth.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal, data = subset(AWCnj, TI))
#### still colinearity between a bunch of semantic classes

### VTA
TA.ekaaother.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + RdplS + Sg.actor + D.actor + Prox.actor + actor.3 + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + D.goal + NDA.Relations.goal + Dem.goal + Prox.goal + NA.beast.of.burden.goal + Px1Sg.goal + Px3Sg.goal + Pers.goal + NA.food.goal, data = subset(AWCnj, TA))
subset(TA.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#category1          category2  N1   N2 N12      alpha.X2
#245            actor.3             goal.3 993 1123   0 5.230196e-316
#343             D.goal NDA.Relations.goal 175  173 173             0
#347             D.goal         Px1Sg.goal 175   83  83 2.212043e-239 
#348             D.goal         Px3Sg.goal 175   48  47 5.517454e-130
#354 NDA.Relations.goal         Px1Sg.goal 173   83  82 4.152687e-236
#355 NDA.Relations.goal         Px3Sg.goal 173   48  46 1.218992e-125
#358           Dem.goal          Prox.goal 161   96  96 1.760423e-304


TA.ekaaother.Bivariate.man <- nominal(. ~ PV.Time + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + RdplS + Sg.actor + D.actor + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + NDA.Relations.goal + Prox.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal, data = subset(AWCnj, TA))
#### still colinearity between a bunch of semantic classes
