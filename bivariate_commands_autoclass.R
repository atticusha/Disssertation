# BiVariate Commands
#=======================================================================
## ind vs. eCnj
#=======================================================================
### VII
II.ive.Bivariate.auto.initial <- nominal(. ~ auto.ni.3.actor + Sg.actor, data = subset(AWive, II))
subset(II.ive.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
II.ive.Bivariate.auto <- nominal(. ~ auto.ni.3.actor, data = subset(AWive, II))
#### 

### VAI
AI.ive.Bivariate.auto.initial <- nominal(. ~ PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Time + RdplW + auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.na.5.actor + auto.vai.11 + auto.vai.9 + auto.nda.1.actor + auto.vai.3 + auto.vai.13 + auto.vai.4 + auto.vai.8 + auto.vai.7 + auto.vai.10 + Pron.actor + Pl.actor + D.actor + Pers.actor + Px3Sg.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWCnj, AI))
subset(AI.ive.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1   category2  N1  N2 N12      alpha.X2      alpha.G2
#245  auto.na.5.actor  Pron.actor 512 383 358             0             0             0     
#248  auto.na.5.actor  Pers.actor 512 172 172             0  3.40166e-183 9.860087e-299     
#295 auto.nda.1.actor     D.actor 163 201 163             0 7.238125e-270             0     
#373       Pron.actor  Pers.actor 383 172 172             0 9.980987e-211             0     
#387          D.actor Px3Sg.actor 201  27  27  1.89821e-133  4.451862e-40 1.933627e-118     
#400      Px3Sg.actor     actor.4  27 173  27 1.123939e-155  5.466567e-42 8.745978e-140     

AI.ive.Bivariate.auto <- nominal(. ~ PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Time + RdplW + auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.na.5.actor + auto.vai.11 + auto.vai.9 + auto.nda.1.actor + auto.vai.3 + auto.vai.13 + auto.vai.4 + auto.vai.8 + auto.vai.7 + auto.vai.10 + Pl.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWive, AI))
#### actor 4 an Px3Sg colinear, obvz

### VTI
TI.ive.Bivariate.auto.initial <- nominal(. ~ PV.Time + PV.Qual + PV.Discourse + auto.vti.4 + auto.vti.2 + auto.na.5.actor + auto.ni.10.goal + auto.ni.9.goal + auto.ni.2.goal + Pron.actor + Pers.actor + actor.3 + actor.1 + actor.2 + actor.4 + Sg.goal + Pl.goal + Px1Sg.goal + Px3Pl.goal, data = subset(AWive, TI))
subset(TI.ive.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1  category2   N1   N2 N12      alpha.X2      alpha.G2
#84  auto.na.5.actor Pron.actor  211  176 170             0 1.514366e-217             0
#85  auto.na.5.actor Pers.actor  211  137 137             0 4.466774e-175             0
#127      Pron.actor Pers.actor  176  137 137             0  4.84976e-194             0
#144         actor.3    actor.1 1128 1086   0 4.446591e-323             0 3.945541e-292

TI.ive.Bivariate.auto <- nominal(. ~ PV.Time + PV.Qual + PV.Discourse + auto.vti.4 + auto.vti.2 + auto.na.5.actor + auto.ni.10.goal + auto.ni.9.goal + auto.ni.2.goal + actor.3 + actor.2 + actor.4 + Sg.goal + Pl.goal + Px1Sg.goal + Px3Pl.goal, data = subset(AWive, TI))
#### still colinearity between a bunch of semantic classes

### VTA
TA.ive.Bivariate.auto.initial <- nominal(. ~ PV.Move + PV.Discourse + PV.Position + PV.Qual + PV.Time + auto.vta.6 + auto.vta.3 + auto.vta.1 + auto.na.5.goal + auto.vta.4 + auto.vta.2 + auto.na.2.goal + Sg.actor + D.actor + Pers.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4, data = subset(AWive, TA))
subset(TA.ive.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1 category2   N1   N2 N12 alpha.X2 alpha.G2 beta
#199   actor.3    goal.3 1097 1212   0        0        0
TA.ive.Bivariate.auto <- nominal(. ~ PV.Move + PV.Discourse + PV.Position + PV.Qual + PV.Time + auto.vta.6 + auto.vta.3 + auto.vta.1 + auto.na.5.goal + auto.vta.4 + auto.vta.2 + auto.na.2.goal + Sg.actor + D.actor + Pers.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.4, data = subset(AWive, TA))





#=======================================================================
## Ind vs Cnj as whole
#=======================================================================
### VII
II.ivc.Bivariate.auto.initial <- nominal(. ~ auto.vii.5, data = subset(AWnimp, II))
subset(II.ivc.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
II.ivc.Bivariate.auto <- nominal(. ~ auto.vii.5, data = subset(AWnimp, II))

### VAI
AI.ivc.Bivariate.auto.initial <- nominal(. ~ PV.Move + PV.Time + PV.Position + PV.Qual + PV.Discourse + PV.StartFin + RdplW + auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.na.5.actor + auto.vai.11 + auto.vai.9 + auto.vai.3 + auto.vai.13 + auto.vai.4 + auto.vai.8 + auto.vai.7 + auto.vai.10 + Pron.actor + Pl.actor + Pers.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWnimp, AI))
subset(AI.ivc.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1  category2   N1   N2 N12 alpha.X2      alpha.G2 beta power effect.size likelihood.ratio cramers.v lambda.12 lambda.21
#240 auto.na.5.actor Pron.actor  694  528 497        0             0    0     1   0.8055369         2564.777 0.8046351 0.6714697 0.5681818
#242 auto.na.5.actor Pers.actor  694  238 238        0 9.852121e-264    0     1   0.5663215         1203.643 0.5650063 0.3429395         0
#332      Pron.actor Pers.actor  528  238 238        0 1.012439e-299    0     1   0.6574071         1369.246  0.655918 0.4507576         0
#346         actor.3    actor.1 4052 2047   0        0             0    0     1    0.701434         4415.246 0.7011263 0.6349256 0.4250122


AI.ivc.Bivariate.auto <- nominal(. ~ PV.Move + PV.Time + PV.Position + PV.Qual + PV.Discourse + PV.StartFin + RdplW + auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.na.5.actor + auto.vai.11 + auto.vai.9 + auto.vai.3 + auto.vai.13 + auto.vai.4 + auto.vai.8 + auto.vai.7 + auto.vai.10 + Pl.actor + actor.1 + actor.2 + actor.4, data = subset(AWnimp, AI))

### VTI
TI.ivc.Bivariate.auto.initial <- nominal(. ~ PV.Time + PV.Discourse + PV.Qual + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.na.5.actor + auto.ni.10.goal + auto.ni.9.goal + auto.ni.2.goal + Pron.actor + Pers.actor + actor.3 + actor.1 + actor.2 + actor.4 + Sg.goal + Dem.goal + Pron.goal + Pl.goal, data = subset(AWnimp, TI))
subset(TI.ivc.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1  category2  N1  N2 N12 alpha.X2      alpha.G2 beta power effect.size likelihood.ratio cramers.v lambda.12 lambda.21    tau.12
#83   auto.ni.3.goal   Dem.goal 883 510 510        0             0    0     1   0.7101049         1687.149 0.7091691 0.5775764 0.2686275  0.504249
#84   auto.ni.3.goal  Pron.goal 883 510 510        0             0    0     1   0.7101049         1687.149 0.7091691 0.5775764 0.2686275  0.504249
#103 auto.na.5.actor Pron.actor 270 223 209        0 1.647709e-259    0     1   0.8407084          1184.21 0.8385138 0.7222222 0.6636771 0.7067907
#104 auto.na.5.actor Pers.actor 270 156 156        0 2.241501e-198    0     1   0.7468398         902.9499 0.7442427 0.5777778 0.2692308 0.5577697
#146      Pron.actor Pers.actor 223 156 156        0 4.707001e-219    0     1    0.827837         998.0748 0.8250002 0.6995516 0.5705128  0.685314
#188        Dem.goal  Pron.goal 510 510 510        0             0    0     1           1         2889.905 0.9988494         1         1         1
TI.ivc.Bivariate.auto <- nominal(. ~ PV.Time + PV.Discourse + PV.Qual + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.na.5.actor + auto.ni.10.goal + auto.ni.9.goal + auto.ni.2.goal + actor.3 + actor.1 + actor.2 + actor.4 + Sg.goal + Pl.goal, data = subset(AWnimp, TI))

### VTA
TA.ivc.Bivariate.auto.initial <- nominal(. ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.vta.4 + Sg.actor + actor.1 + actor.2 + actor.4 + goal.3 + goal.4 + goal.2, data = subset(AWnimp, TA))
subset(TA.ivc.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1       category2  N1  N2 N12 alpha.X2      alpha.G2 beta power effect.size likelihood.ratio cramers.v lambda.12 lambda.21    tau.12    tau.21     uc.12
#296 auto.na.5.goal NA.persons.goal 424 600 413        0             0    0     1   0.7912219         1710.164 0.7900635 0.5330189      0.67  0.626032  0.626032 0.6578243
#307        A.actor         N.actor 349 273 272        0             0    0     1   0.8706059         1540.174 0.8688308 0.7765043 0.7142857 0.7579546 0.7579546 0.6744276
#308        A.actor        Sg.actor 349 235 233        0 1.007291e-274    0     1   0.7987819         1254.215 0.7968797 0.6618911 0.4978723 0.6380526 0.6380526 0.5492087


TA.ivc.Bivariate.auto <- nominal(. ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.vta.4 + Sg.actor + actor.1 + actor.2 + actor.4 + goal.3 + goal.4 + goal.2, data = subset(AWnimp, TA))



#=======================================================================
## eCnj vs kaa vs other
#=======================================================================
### VII
II.ekaaother.Bivariate.auto.initial <- nominal(. ~ auto.ni.3.actor + auto.vii.3 + Dem.actor + Pron.actor + Sg.actor + auto.vii.1, data = subset(AWCnj, II))
subset(II.ekaaother.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1  category2 N1 N2 N12      alpha.X2     alpha.G2         beta power effect.size
#2  auto.ni.3.actor  Dem.actor 69 27  27  5.050047e-37  7.75687e-27 1.312723e-28     1    0.595568
#3  auto.ni.3.actor Pron.actor 69 27  27  5.050047e-37  7.75687e-27 1.312723e-28     1    0.595568
#4  auto.ni.3.actor   Sg.actor 69 89  66  4.681033e-69 7.168615e-58 2.479061e-56     1   0.8126788
II.ekaaother.Bivariate.auto <- nominal(. ~ auto.ni.3.actor + auto.vii.3 + auto.vii.1, data = subset(AWCnj, II))


### VAI
AI.ekaaother.Bivariate.auto.initial <- nominal(. ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + auto.vai.2 + auto.vai.6 + auto.na.5.actor + auto.vai.11 + auto.nda.1.actor + auto.na.7.actor + auto.na.1.actor + Sg.actor + Pron.actor + Pl.actor + D.actor + Dem.actor + Pers.actor + Px1Sg.actor + Prox.actor + Indef.actor + actor.3 + actor.1 + actor.2, data = subset(AWCnj, AI))
subset(AI.ekaaother.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1   category2  N1  N2 N12      alpha.X2      alpha.G2          beta power
#123  auto.na.5.actor  Pron.actor 512 383 358             0             0             0     1
#126  auto.na.5.actor   Dem.actor 512 186 186             0 2.827445e-199             0     1
#127  auto.na.5.actor  Pers.actor 512 172 172             0  3.40166e-183 9.860087e-299     1
#154 auto.nda.1.actor     D.actor 163 201 163             0 7.238125e-270             0     1
#157 auto.nda.1.actor Px1Sg.actor 163 151 118             0 2.400151e-167             0     1
#184  auto.na.1.actor Indef.actor  48  25  25             0  1.271241e-55             0     1
#201       Pron.actor   Dem.actor 383 186 186             0 8.673908e-230             0     1
#202       Pron.actor  Pers.actor 383 172 172             0 9.980987e-211             0     1
#204       Pron.actor  Prox.actor 383 100 100 2.526952e-258  2.92951e-118 7.455015e-233     1
#220          D.actor Px1Sg.actor 201 151 150             0 6.548227e-241             0     1
#228        Dem.actor  Prox.actor 186 100 100             0 4.529236e-158             0     1

AI.ekaaother.Bivariate.auto <- nominal(. ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + auto.vai.2 + auto.vai.6 + auto.na.5.actor + auto.vai.11 + auto.nda.1.actor + auto.na.7.actor + auto.na.1.actor + Sg.actor + Pl.actor + Prox.actor + actor.3 + actor.1 + actor.2 , data = subset(AWCnj, AI))
#### still colinearity between a bunch of semantic classes

### VTI
TI.ekaaother.Bivariate.auto.initial <- nominal(. ~ PV.Time + PV.Position + PV.Discourse + PV.WantCan + PV.Qual + auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.ni.10.goal + auto.ni.2.goal + auto.ni.4.goal + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + Dem.goal + Pron.goal + Pl.goal + Prox.goal, data = subset(AWCnj, TI))
subset(TI.ekaaother.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1 category2  N1  N2 N12      alpha.X2      alpha.G2          beta power effect.size
#137 auto.ni.3.goal  Dem.goal 662 377 377 4.739096e-258 5.039201e-260 7.765108e-231     1   0.6998353
#138 auto.ni.3.goal Pron.goal 662 377 377 4.739096e-258 5.039201e-260 7.765108e-231     1   0.6998353
#226       Dem.goal Pron.goal 377 377 377             0             0             0     1           1
#228       Dem.goal Prox.goal 377 176 176 1.813735e-223 7.891722e-163 8.091239e-199     1   0.6518408
#230      Pron.goal Prox.goal 377 176 176 1.813735e-223 7.891722e-163 8.091239e-199     1   0.6518408

TI.ekaaother.Bivariate.auto <- nominal(. ~ PV.Time + PV.Position + PV.Discourse + PV.WantCan + PV.Qual + auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.ni.10.goal + auto.ni.2.goal + auto.ni.4.goal + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + Pl.goal + Prox.goal, data = subset(AWCnj, TI))

### VTA
TA.ekaaother.Bivariate.auto.initial <- nominal(. ~ PV.Time + PV.Position + PV.Discourse + RdplS + auto.vta.6 + auto.vta.3 + auto.vta.1 + auto.vta.5 + auto.nda.1.goal + auto.vta.4 + auto.vta.2 + Sg.actor + D.actor + Prox.actor + actor.3 + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + D.goal + Dem.goal + Prox.goal + Px1Sg.goal + Px3Sg.goal + Pers.goal, data = subset(AWCnj, TA))
subset(TA.ekaaother.Bivariate.auto.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1  category2  N1   N2 N12      alpha.X2      alpha.G2          beta power
#202 auto.nda.1.goal     D.goal 147  175 147             0 8.907055e-210             0     1
#290         actor.3     goal.3 993 1123   0 5.230196e-316             0  2.01951e-285     1
#366          D.goal Px1Sg.goal 175   83  83 2.212043e-239  1.09467e-106 2.703939e-215     1
#367          D.goal Px3Sg.goal 175   48  47 5.517454e-130  1.928854e-56  3.46229e-113     1
#369        Dem.goal  Prox.goal 161   96  96 1.760423e-304 1.417935e-130 4.559189e-277     1

TA.ekaaother.Bivariate.auto <- nominal(. ~ PV.Time + PV.Position + PV.Discourse + RdplS + auto.vta.6 + auto.vta.3 + auto.vta.1 + auto.vta.5 + auto.nda.1.goal + auto.vta.4 + auto.vta.2 + Sg.actor + D.actor + Prox.actor + actor.3 + actor.1 + actor.2 + goal.4 + goal.1 + goal.2 + Sg.goal + Prox.goal + Px1Sg.goal + Px3Sg.goal + Pers.goal, data = subset(AWCnj, TA))
#### still colinearity between a bunch of semantic classes
