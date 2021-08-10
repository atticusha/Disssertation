#MODEL STATISTICS

### Ind v. eCnj
ii.ive.observed.t.man <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.t.man <- ifelse(as.vector(fitted(ii.ind.v.ecnj.glmer.t.man))>0.5, "Ind", "eCnj")
table(ii.ive.observed.t.man,ii.ive.predicted.t.man)
ii.ive.p.values.t.man <- cbind(as.vector(fitted(ii.ind.v.ecnj.glmer.t.man)), 1-as.vector(fitted(ii.ind.v.ecnj.glmer.t.man)))
colnames(ii.ive.p.values.t.man) <- c("Ind","eCnj")
ii.ive.ModelStats.t.man<- model.statistics(ii.ive.observed.t.man, ii.ive.predicted.t.man, ii.ive.p.values.t.man)

ai.ive.observed.auto <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.auto <- ifelse(as.vector(fitted(ai.ive.glmer.auto))>0.5, "Ind", "eCnj")
table(ai.ive.observed.auto,ai.ive.predicted.auto)
ai.ive.p.values.auto <- cbind(as.vector(fitted(ai.ive.glmer.auto)), 1-as.vector(fitted(ai.ive.glmer.auto)))
colnames(ai.ive.p.values.auto) <- c("Ind","eCnj")
ai.ive.ModelStats.auto<- model.statistics(ai.ive.observed.auto, ai.ive.predicted.auto, ai.ive.p.values.auto)

ti.ive.observed.auto <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.auto <- ifelse(as.vector(fitted(ti.ive.glmer.auto))>0.5, "Ind", "eCnj")
table(ti.ive.observed.auto,ti.ive.predicted.auto)
ti.ive.p.values.auto <- cbind(as.vector(fitted(ti.ive.glmer.auto)), 1-as.vector(fitted(ti.ive.glmer.auto)))
colnames(ti.ive.p.values.auto) <- c("Ind","eCnj")
ti.ive.ModelStats.auto<- model.statistics(ti.ive.observed.auto, ti.ive.predicted.auto, ti.ive.p.values.auto)

ta.ive.observed.auto <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.auto <- ifelse(as.vector(fitted(ta.ive.glmer.auto))>0.5, "Ind", "eCnj")
table(ta.ive.observed.auto,ta.ive.predicted.auto)
ta.ive.p.values.auto <- cbind(as.vector(fitted(ta.ive.glmer.auto)), 1-as.vector(fitted(ta.ive.glmer.auto)))
colnames(ta.ive.p.values.auto) <- c("Ind","eCnj")
ta.ive.ModelStats.auto<- model.statistics(ta.ive.observed.auto, ta.ive.predicted.auto, ta.ive.p.values.auto)

## Save Model stats##
sink(file = 'Auto/ii-ive-ModelStats-auto.txt')
print(ii.ive.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ai-ive-ModelStats-auto.txt')
print(ai.ive.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ti-ive-ModelStats-auto.txt')
print(ti.ive.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ta-ive-ModelStats-auto.txt')
print(ta.ive.ModelStats.auto, max.print = NA)
sink(NULL)



#================================================
### eCnj v. kaaCnj v. OtherCnj
#================================================
ii.eCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ii.eCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ii.e.cnjtype.glmer.auto))>0.5, "eCnj", "otherCnj")
table(ii.eCnj.cnjtypes.observed.auto,ii.eCnj.cnjtypes.predicted.auto)
ii.eCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ii.e.cnjtype.glmer.auto)), 1-as.vector(fitted(ii.e.cnjtype.glmer.auto)))
colnames(ii.eCnj.cnjtypes.p.values.auto) <- c("eCnj","otherCnj")
ii.eCnj.cnjtypes.ModelStats.auto<- model.statistics(ii.eCnj.cnjtypes.observed.auto, ii.eCnj.cnjtypes.predicted.auto, ii.eCnj.cnjtypes.p.values.auto)

ii.kaaCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ii.kaaCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ii.kaa.cnjtype.glmer.auto))>0.5, "kaaCnj", "otherCnj")
table(ii.kaaCnj.cnjtypes.observed.auto,ii.kaaCnj.cnjtypes.predicted.auto)
ii.kaaCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ii.kaa.cnjtype.glmer.auto)), 1-as.vector(fitted(ii.kaa.cnjtype.glmer.auto)))
colnames(ii.kaaCnj.cnjtypes.p.values.auto) <- c("kaaCnj","otherCnj")
ii.kaaCnj.cnjtypes.ModelStats.auto<- model.statistics(ii.kaaCnj.cnjtypes.observed.auto, ii.kaaCnj.cnjtypes.predicted.auto, ii.kaaCnj.cnjtypes.p.values.auto)

ii.OtherCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ii.OtherCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ii.other.cnjtype.glmer.auto))>0.5, "OtherCnj", "otherCnj")
table(ii.OtherCnj.cnjtypes.observed.auto,ii.OtherCnj.cnjtypes.predicted.auto)
ii.OtherCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ii.other.cnjtype.glmer.auto)), 1-as.vector(fitted(ii.other.cnjtype.glmer.auto)))
colnames(ii.OtherCnj.cnjtypes.p.values.auto) <- c("OtherCnj","otherCnj")
ii.OtherCnj.cnjtypes.ModelStats.auto<- model.statistics(ii.OtherCnj.cnjtypes.observed.auto, ii.OtherCnj.cnjtypes.predicted.auto, ii.OtherCnj.cnjtypes.p.values.auto)

ai.eCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ai.eCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ai.e.cnjtype.glmer.auto))>0.5, "eCnj", "otherCnj")
table(ai.eCnj.cnjtypes.observed.auto,ai.eCnj.cnjtypes.predicted.auto)
ai.eCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ai.e.cnjtype.glmer.auto)), 1-as.vector(fitted(ai.e.cnjtype.glmer.auto)))
colnames(ai.eCnj.cnjtypes.p.values.auto) <- c("eCnj","otherCnj")
ai.eCnj.cnjtypes.ModelStats.auto<- model.statistics(ai.eCnj.cnjtypes.observed.auto, ai.eCnj.cnjtypes.predicted.auto, ai.eCnj.cnjtypes.p.values.auto)

ai.kaaCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ai.kaaCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ai.kaa.cnjtype.glmer.auto))>0.5, "kaaCnj", "otherCnj")
table(ai.kaaCnj.cnjtypes.observed.auto,ai.kaaCnj.cnjtypes.predicted.auto)
ai.kaaCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ai.kaa.cnjtype.glmer.auto)), 1-as.vector(fitted(ai.kaa.cnjtype.glmer.auto)))
colnames(ai.kaaCnj.cnjtypes.p.values.auto) <- c("kaaCnj","otherCnj")
ai.kaaCnj.cnjtypes.ModelStats.auto<- model.statistics(ai.kaaCnj.cnjtypes.observed.auto, ai.kaaCnj.cnjtypes.predicted.auto, ai.kaaCnj.cnjtypes.p.values.auto)

ai.OtherCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ai.OtherCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ai.other.cnjtype.glmer.auto))>0.5, "OtherCnj", "otherCnj")
table(ai.OtherCnj.cnjtypes.observed.auto,ai.OtherCnj.cnjtypes.predicted.auto)
ai.OtherCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ai.other.cnjtype.glmer.auto)), 1-as.vector(fitted(ai.other.cnjtype.glmer.auto)))
colnames(ai.OtherCnj.cnjtypes.p.values.auto) <- c("OtherCnj","otherCnj")
ai.OtherCnj.cnjtypes.ModelStats.auto<- model.statistics(ai.OtherCnj.cnjtypes.observed.auto, ai.OtherCnj.cnjtypes.predicted.auto, ai.OtherCnj.cnjtypes.p.values.auto)

ti.eCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ti.eCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ti.e.cnjtype.glmer.auto))>0.5, "eCnj", "otherCnj")
table(ti.eCnj.cnjtypes.observed.auto,ti.eCnj.cnjtypes.predicted.auto)
ti.eCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ti.e.cnjtype.glmer.auto)), 1-as.vector(fitted(ti.e.cnjtype.glmer.auto)))
colnames(ti.eCnj.cnjtypes.p.values.auto) <- c("eCnj","otherCnj")
ti.eCnj.cnjtypes.ModelStats.auto<- model.statistics(ti.eCnj.cnjtypes.observed.auto, ti.eCnj.cnjtypes.predicted.auto, ti.eCnj.cnjtypes.p.values.auto)

ti.kaaCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ti.kaaCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ti.kaa.cnjtype.glmer.auto))>0.5, "kaaCnj", "otherCnj")
table(ti.kaaCnj.cnjtypes.observed.auto,ti.kaaCnj.cnjtypes.predicted.auto)
ti.kaaCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ti.kaa.cnjtype.glmer.auto)), 1-as.vector(fitted(ti.kaa.cnjtype.glmer.auto)))
colnames(ti.kaaCnj.cnjtypes.p.values.auto) <- c("kaaCnj","otherCnj")
ti.kaaCnj.cnjtypes.ModelStats.auto<- model.statistics(ti.kaaCnj.cnjtypes.observed.auto, ti.kaaCnj.cnjtypes.predicted.auto, ti.kaaCnj.cnjtypes.p.values.auto)

ti.OtherCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ti.OtherCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ti.other.cnjtype.glmer.auto))>0.5, "OtherCnj", "otherCnj")
table(ti.OtherCnj.cnjtypes.observed.auto,ti.OtherCnj.cnjtypes.predicted.auto)
ti.OtherCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ti.other.cnjtype.glmer.auto)), 1-as.vector(fitted(ti.other.cnjtype.glmer.auto)))
colnames(ti.OtherCnj.cnjtypes.p.values.auto) <- c("OtherCnj","otherCnj")
ti.OtherCnj.cnjtypes.ModelStats.auto<- model.statistics(ti.OtherCnj.cnjtypes.observed.auto, ti.OtherCnj.cnjtypes.predicted.auto, ti.OtherCnj.cnjtypes.p.values.auto)

ta.eCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ta.eCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ta.e.cnjtype.glmer.auto))>0.5, "eCnj", "otherCnj")
table(ta.eCnj.cnjtypes.observed.auto,ta.eCnj.cnjtypes.predicted.auto)
ta.eCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ta.e.cnjtype.glmer.auto)), 1-as.vector(fitted(ta.e.cnjtype.glmer.auto)))
colnames(ta.eCnj.cnjtypes.p.values.auto) <- c("eCnj","otherCnj")
ta.eCnj.cnjtypes.ModelStats.auto<- model.statistics(ta.eCnj.cnjtypes.observed.auto, ta.eCnj.cnjtypes.predicted.auto, ta.eCnj.cnjtypes.p.values.auto)

ta.kaaCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ta.kaaCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ta.kaa.cnjtype.glmer.auto))>0.5, "kaaCnj", "otherCnj")
table(ta.kaaCnj.cnjtypes.observed.auto,ta.kaaCnj.cnjtypes.predicted.auto)
ta.kaaCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ta.kaa.cnjtype.glmer.auto)), 1-as.vector(fitted(ta.kaa.cnjtype.glmer.auto)))
colnames(ta.kaaCnj.cnjtypes.p.values.auto) <- c("kaaCnj","otherCnj")
ta.kaaCnj.cnjtypes.ModelStats.auto<- model.statistics(ta.kaaCnj.cnjtypes.observed.auto, ta.kaaCnj.cnjtypes.predicted.auto, ta.kaaCnj.cnjtypes.p.values.auto)

ta.OtherCnj.cnjtypes.observed.auto <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ta.OtherCnj.cnjtypes.predicted.auto <- ifelse(as.vector(fitted(ta.other.cnjtype.glmer.auto))>0.5, "OtherCnj", "otherCnj")
table(ta.OtherCnj.cnjtypes.observed.auto,ta.OtherCnj.cnjtypes.predicted.auto)
ta.OtherCnj.cnjtypes.p.values.auto <- cbind(as.vector(fitted(ta.other.cnjtype.glmer.auto)), 1-as.vector(fitted(ta.other.cnjtype.glmer.auto)))
colnames(ta.OtherCnj.cnjtypes.p.values.auto) <- c("OtherCnj","otherCnj")
ta.OtherCnj.cnjtypes.ModelStats.auto<- model.statistics(ta.OtherCnj.cnjtypes.observed.auto, ta.OtherCnj.cnjtypes.predicted.auto, ta.OtherCnj.cnjtypes.p.values.auto)






## Save Model stats##
sink(file = 'Auto/ii-ecnj-cnjtypes-ModelStats-auto.txt')
print(ii.eCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ai-ecnj-cnjtypes-ModelStats-auto.txt')
print(ai.eCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ti-ecnj-cnjtypes-ModelStats-auto.txt')
print(ti.eCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ta-ecnj-cnjtypes-ModelStats-auto.txt')
print(ta.eCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)

sink(file = 'Auto/ii-kaacnj-cnjtypes-ModelStats-auto.txt')
print(ii.kaaCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ai-kaacnj-cnjtypes-ModelStats-auto.txt')
print(ai.kaaCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ti-kaacnj-cnjtypes-ModelStats-auto.txt')
print(ti.kaaCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ta-kaacnj-cnjtypes-ModelStats-auto.txt')
print(ta.kaaCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)

sink(file = 'Auto/ii-othercnj-cnjtypes-ModelStats-auto.txt')
print(ii.OtherCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ai-othercnj-cnjtypes-ModelStats-auto.txt')
print(ai.OtherCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ti-othercnj-cnjtypes-ModelStats-auto.txt')
print(ti.OtherCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ta-othercnj-cnjtypes-ModelStats-auto.txt')
print(ta.OtherCnj.cnjtypes.ModelStats.auto, max.print = NA)
sink(NULL)



#================================================
### Ind v. Cnj
#================================================

ii.indcnj.observed.auto <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.auto <- ifelse(as.vector(fitted(ii.indcnj.glmer.auto))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.auto,ii.indcnj.predicted.auto)
ii.indcnj.p.values.auto <- cbind(as.vector(fitted(ii.indcnj.glmer.auto)), 1-as.vector(fitted(ii.indcnj.glmer.auto)))
colnames(ii.indcnj.p.values.auto) <- c("Ind","eCnj")
ii.indcnj.ModelStats.auto<- model.statistics(ii.indcnj.observed.auto, ii.indcnj.predicted.auto, ii.indcnj.p.values.auto)

ai.indcnj.observed.auto <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.auto <- ifelse(as.vector(fitted(ai.indcnj.glmer.auto))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.auto,ai.indcnj.predicted.auto)
ai.indcnj.p.values.auto <- cbind(as.vector(fitted(ai.indcnj.glmer.auto)), 1-as.vector(fitted(ai.indcnj.glmer.auto)))
colnames(ai.indcnj.p.values.auto) <- c("Ind","eCnj")
ai.indcnj.ModelStats.auto<- model.statistics(ai.indcnj.observed.auto, ai.indcnj.predicted.auto, ai.indcnj.p.values.auto)

ti.indcnj.observed.auto <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.auto <- ifelse(as.vector(fitted(ti.indcnj.glmer.auto))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.auto,ti.indcnj.predicted.auto)
ti.indcnj.p.values.auto <- cbind(as.vector(fitted(ti.indcnj.glmer.auto)), 1-as.vector(fitted(ti.indcnj.glmer.auto)))
colnames(ti.indcnj.p.values.auto) <- c("Ind","eCnj")
ti.indcnj.ModelStats.auto<- model.statistics(ti.indcnj.observed.auto, ti.indcnj.predicted.auto, ti.indcnj.p.values.auto)

ta.indcnj.observed.auto <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.auto <- ifelse(as.vector(fitted(ta.indcnj.glmer.auto))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.auto,ta.indcnj.predicted.auto)
ta.indcnj.p.values.auto <- cbind(as.vector(fitted(ta.indcnj.glmer.auto)), 1-as.vector(fitted(ta.indcnj.glmer.auto)))
colnames(ta.indcnj.p.values.auto) <- c("Ind","eCnj")
ta.indcnj.ModelStats.auto<- model.statistics(ta.indcnj.observed.auto, ta.indcnj.predicted.auto, ta.indcnj.p.values.auto)

## Save Model stats##
sink(file = 'Auto/ii-indcnj-ModelStats-auto.txt')
print(ii.indcnj.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ai-indcnj-ModelStats-auto.txt')
print(ai.indcnj.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ti-indcnj-ModelStats-auto.txt')
print(ti.indcnj.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ta-indcnj-ModelStats-auto.txt')
print(ta.indcnj.ModelStats.auto, max.print = NA)
sink(NULL)

#================================================
###all
#================================================

ii.Ind.all.observed.auto <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "otherCnj")
ii.Ind.all.predicted.auto <- ifelse(as.vector(fitted(ii.ind.all.glmer.auto))>0.5, "Ind", "otherCnj")
table(ii.Ind.all.observed.auto,ii.Ind.all.predicted.auto)
ii.Ind.all.p.values.auto <- cbind(as.vector(fitted(ii.ind.all.glmer.auto)), 1-as.vector(fitted(ii.ind.all.glmer.auto)))
colnames(ii.Ind.all.p.values.auto) <- c("Ind","otherCnj")
ii.Ind.all.ModelStats.auto<- model.statistics(ii.Ind.all.observed.auto, ii.Ind.all.predicted.auto, ii.Ind.all.p.values.auto)

ai.Ind.all.observed.auto <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "otherCnj")
ai.Ind.all.predicted.auto <- ifelse(as.vector(fitted(ai.ind.all.glmer.auto))>0.5, "Ind", "otherCnj")
table(ai.Ind.all.observed.auto,ai.Ind.all.predicted.auto)
ai.Ind.all.p.values.auto <- cbind(as.vector(fitted(ai.ind.all.glmer.auto)), 1-as.vector(fitted(ai.ind.all.glmer.auto)))
colnames(ai.Ind.all.p.values.auto) <- c("Ind","otherCnj")
ai.Ind.all.ModelStats.auto<- model.statistics(ai.Ind.all.observed.auto, ai.Ind.all.predicted.auto, ai.Ind.all.p.values.auto)

ti.Ind.all.observed.auto <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "otherCnj")
ti.Ind.all.predicted.auto <- ifelse(as.vector(fitted(ti.ind.all.glmer.auto))>0.5, "Ind", "otherCnj")
table(ti.Ind.all.observed.auto,ti.Ind.all.predicted.auto)
ti.Ind.all.p.values.auto <- cbind(as.vector(fitted(ti.ind.all.glmer.auto)), 1-as.vector(fitted(ti.ind.all.glmer.auto)))
colnames(ti.Ind.all.p.values.auto) <- c("Ind","otherCnj")
ti.Ind.all.ModelStats.auto<- model.statistics(ti.Ind.all.observed.auto, ti.Ind.all.predicted.auto, ti.Ind.all.p.values.auto)

ta.Ind.all.observed.auto <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "otherCnj")
ta.Ind.all.predicted.auto <- ifelse(as.vector(fitted(ta.ind.all.glmer.auto))>0.5, "Ind", "otherCnj")
table(ta.Ind.all.observed.auto,ta.Ind.all.predicted.auto)
ta.Ind.all.p.values.auto <- cbind(as.vector(fitted(ta.ind.all.glmer.auto)), 1-as.vector(fitted(ta.ind.all.glmer.auto)))
colnames(ta.Ind.all.p.values.auto) <- c("Ind","otherCnj")
ta.Ind.all.ModelStats.auto<- model.statistics(ta.Ind.all.observed.auto, ta.Ind.all.predicted.auto, ta.Ind.all.p.values.auto)


ii.eCnj.all.observed.auto <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ii.eCnj.all.predicted.auto <- ifelse(as.vector(fitted(ii.e.all.glmer.auto))>0.5, "eCnj", "otherCnj")
table(ii.eCnj.all.observed.auto,ii.eCnj.all.predicted.auto)
ii.eCnj.all.p.values.auto <- cbind(as.vector(fitted(ii.e.all.glmer.auto)), 1-as.vector(fitted(ii.e.all.glmer.auto)))
colnames(ii.eCnj.all.p.values.auto) <- c("eCnj","otherCnj")
ii.eCnj.all.ModelStats.auto<- model.statistics(ii.eCnj.all.observed.auto, ii.eCnj.all.predicted.auto, ii.eCnj.all.p.values.auto)

ai.eCnj.all.observed.auto <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ai.eCnj.all.predicted.auto <- ifelse(as.vector(fitted(ai.e.all.glmer.auto))>0.5, "eCnj", "otherCnj")
table(ai.eCnj.all.observed.auto,ai.eCnj.all.predicted.auto)
ai.eCnj.all.p.values.auto <- cbind(as.vector(fitted(ai.e.all.glmer.auto)), 1-as.vector(fitted(ai.e.all.glmer.auto)))
colnames(ai.eCnj.all.p.values.auto) <- c("eCnj","otherCnj")
ai.eCnj.all.ModelStats.auto<- model.statistics(ai.eCnj.all.observed.auto, ai.eCnj.all.predicted.auto, ai.eCnj.all.p.values.auto)

ti.eCnj.all.observed.auto <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ti.eCnj.all.predicted.auto <- ifelse(as.vector(fitted(ti.e.all.glmer.auto))>0.5, "eCnj", "otherCnj")
table(ti.eCnj.all.observed.auto,ti.eCnj.all.predicted.auto)
ti.eCnj.all.p.values.auto <- cbind(as.vector(fitted(ti.e.all.glmer.auto)), 1-as.vector(fitted(ti.e.all.glmer.auto)))
colnames(ti.eCnj.all.p.values.auto) <- c("eCnj","otherCnj")
ti.eCnj.all.ModelStats.auto<- model.statistics(ti.eCnj.all.observed.auto, ti.eCnj.all.predicted.auto, ti.eCnj.all.p.values.auto)

ta.eCnj.all.observed.auto <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ta.eCnj.all.predicted.auto <- ifelse(as.vector(fitted(ta.e.all.glmer.auto))>0.5, "eCnj", "otherCnj")
table(ta.eCnj.all.observed.auto,ta.eCnj.all.predicted.auto)
ta.eCnj.all.p.values.auto <- cbind(as.vector(fitted(ta.e.all.glmer.auto)), 1-as.vector(fitted(ta.e.all.glmer.auto)))
colnames(ta.eCnj.all.p.values.auto) <- c("eCnj","otherCnj")
ta.eCnj.all.ModelStats.auto<- model.statistics(ta.eCnj.all.observed.auto, ta.eCnj.all.predicted.auto, ta.eCnj.all.p.values.auto)

ii.kaaCnj.all.observed.auto <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ii.kaaCnj.all.predicted.auto <- ifelse(as.vector(fitted(ii.kaa.all.glmer.auto))>0.5, "kaaCnj", "otherCnj")
table(ii.kaaCnj.all.observed.auto,ii.kaaCnj.all.predicted.auto)
ii.kaaCnj.all.p.values.auto <- cbind(as.vector(fitted(ii.kaa.all.glmer.auto)), 1-as.vector(fitted(ii.kaa.all.glmer.auto)))
colnames(ii.kaaCnj.all.p.values.auto) <- c("kaaCnj","otherCnj")
ii.kaaCnj.all.ModelStats.auto<- model.statistics(ii.kaaCnj.all.observed.auto, ii.kaaCnj.all.predicted.auto, ii.kaaCnj.all.p.values.auto)

ai.kaaCnj.all.observed.auto <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ai.kaaCnj.all.predicted.auto <- ifelse(as.vector(fitted(ai.kaa.all.glmer.auto))>0.5, "kaaCnj", "otherCnj")
table(ai.kaaCnj.all.observed.auto,ai.kaaCnj.all.predicted.auto)
ai.kaaCnj.all.p.values.auto <- cbind(as.vector(fitted(ai.kaa.all.glmer.auto)), 1-as.vector(fitted(ai.kaa.all.glmer.auto)))
colnames(ai.kaaCnj.all.p.values.auto) <- c("kaaCnj","otherCnj")
ai.kaaCnj.all.ModelStats.auto<- model.statistics(ai.kaaCnj.all.observed.auto, ai.kaaCnj.all.predicted.auto, ai.kaaCnj.all.p.values.auto)

ti.kaaCnj.all.observed.auto <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ti.kaaCnj.all.predicted.auto <- ifelse(as.vector(fitted(ti.kaa.all.glmer.auto))>0.5, "kaaCnj", "otherCnj")
table(ti.kaaCnj.all.observed.auto,ti.kaaCnj.all.predicted.auto)
ti.kaaCnj.all.p.values.auto <- cbind(as.vector(fitted(ti.kaa.all.glmer.auto)), 1-as.vector(fitted(ti.kaa.all.glmer.auto)))
colnames(ti.kaaCnj.all.p.values.auto) <- c("kaaCnj","otherCnj")
ti.kaaCnj.all.ModelStats.auto<- model.statistics(ti.kaaCnj.all.observed.auto, ti.kaaCnj.all.predicted.auto, ti.kaaCnj.all.p.values.auto)

ta.kaaCnj.all.observed.auto <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ta.kaaCnj.all.predicted.auto <- ifelse(as.vector(fitted(ta.kaa.all.glmer.auto))>0.5, "kaaCnj", "otherCnj")
table(ta.kaaCnj.all.observed.auto,ta.kaaCnj.all.predicted.auto)
ta.kaaCnj.all.p.values.auto <- cbind(as.vector(fitted(ta.kaa.all.glmer.auto)), 1-as.vector(fitted(ta.kaa.all.glmer.auto)))
colnames(ta.kaaCnj.all.p.values.auto) <- c("kaaCnj","otherCnj")
ta.kaaCnj.all.ModelStats.auto<- model.statistics(ta.kaaCnj.all.observed.auto, ta.kaaCnj.all.predicted.auto, ta.kaaCnj.all.p.values.auto)


ii.OtherCnj.all.observed.auto <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "OtherCnj", "OtherCnj", "otherCnj")
ii.OtherCnj.all.predicted.auto <- ifelse(as.vector(fitted(ii.other.all.glmer.auto))>0.5, "OtherCnj", "otherCnj")
table(ii.OtherCnj.all.observed.auto,ii.OtherCnj.all.predicted.auto)
ii.OtherCnj.all.p.values.auto <- cbind(as.vector(fitted(ii.other.all.glmer.auto)), 1-as.vector(fitted(ii.other.all.glmer.auto)))
colnames(ii.OtherCnj.all.p.values.auto) <- c("OtherCnj","otherCnj")
ii.OtherCnj.all.ModelStats.auto<- model.statistics(ii.OtherCnj.all.observed.auto, ii.OtherCnj.all.predicted.auto, ii.OtherCnj.all.p.values.auto)

ai.OtherCnj.all.observed.auto <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "OtherCnj", "OtherCnj", "otherCnj")
ai.OtherCnj.all.predicted.auto <- ifelse(as.vector(fitted(ai.other.all.glmer.auto))>0.5, "OtherCnj", "otherCnj")
table(ai.OtherCnj.all.observed.auto,ai.OtherCnj.all.predicted.auto)
ai.OtherCnj.all.p.values.auto <- cbind(as.vector(fitted(ai.other.all.glmer.auto)), 1-as.vector(fitted(ai.other.all.glmer.auto)))
colnames(ai.OtherCnj.all.p.values.auto) <- c("OtherCnj","otherCnj")
ai.OtherCnj.all.ModelStats.auto<- model.statistics(ai.OtherCnj.all.observed.auto, ai.OtherCnj.all.predicted.auto, ai.OtherCnj.all.p.values.auto)

ti.OtherCnj.all.observed.auto <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "OtherCnj", "OtherCnj", "otherCnj")
ti.OtherCnj.all.predicted.auto <- ifelse(as.vector(fitted(ti.other.all.glmer.auto))>0.5, "OtherCnj", "otherCnj")
table(ti.OtherCnj.all.observed.auto,ti.OtherCnj.all.predicted.auto)
ti.OtherCnj.all.p.values.auto <- cbind(as.vector(fitted(ti.other.all.glmer.auto)), 1-as.vector(fitted(ti.other.all.glmer.auto)))
colnames(ti.OtherCnj.all.p.values.auto) <- c("OtherCnj","otherCnj")
ti.OtherCnj.all.ModelStats.auto<- model.statistics(ti.OtherCnj.all.observed.auto, ti.OtherCnj.all.predicted.auto, ti.OtherCnj.all.p.values.auto)

ta.OtherCnj.all.observed.auto <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "OtherCnj", "OtherCnj", "otherCnj")
ta.OtherCnj.all.predicted.auto <- ifelse(as.vector(fitted(ta.other.all.glmer.auto))>0.5, "OtherCnj", "otherCnj")
table(ta.OtherCnj.all.observed.auto,ta.OtherCnj.all.predicted.auto)
ta.OtherCnj.all.p.values.auto <- cbind(as.vector(fitted(ta.other.all.glmer.auto)), 1-as.vector(fitted(ta.other.all.glmer.auto)))
colnames(ta.OtherCnj.all.p.values.auto) <- c("OtherCnj","otherCnj")
ta.OtherCnj.all.ModelStats.auto<- model.statistics(ta.OtherCnj.all.observed.auto, ta.OtherCnj.all.predicted.auto, ta.OtherCnj.all.p.values.auto)



ii.all.auto<-t(apply(cbind(fitted(ii.e.all.glmer.auto),fitted(ii.kaa.all.glmer.auto), fitted(ii.other.all.glmer.auto), fitted(ii.ind.all.glmer.auto)), 1, function(x) x/sum(x)))
colnames(ii.all.auto)<-c("eCnj", "kaaCnj", "OtherCnj", "Ind")
ii.all.auto.observed<-subset(AWnimp, II, OrderType, drop=TRUE)
ii.all.auto.predicted<-colnames(ii.all.auto)[apply(ii.all.auto, 1, which.max)]
ii.all.modelstats.auto<-model.statistics(ii.all.auto.observed, ii.all.auto.predicted, ii.all.auto)

ai.all.auto<-t(apply(cbind(fitted(ai.e.all.glmer.auto),fitted(ai.kaa.all.glmer.auto), fitted(ai.other.all.glmer.auto), fitted(ai.ind.all.glmer.auto)), 1, function(x) x/sum(x)))
colnames(ai.all.auto)<-c("eCnj", "kaaCnj", "OtherCnj", "Ind")
ai.all.auto.observed<-subset(AWnimp, AI, OrderType, drop=TRUE)
ai.all.auto.predicted<-colnames(ai.all.auto)[apply(ai.all.auto, 1, which.max)]
ai.all.modelstats.auto<-model.statistics(ai.all.auto.observed, ai.all.auto.predicted, ai.all.auto)

ti.all.auto<-t(apply(cbind(fitted(ti.e.all.glmer.auto),fitted(ti.kaa.all.glmer.auto), fitted(ti.other.all.glmer.auto), fitted(ti.ind.all.glmer.auto)), 1, function(x) x/sum(x)))
colnames(ti.all.auto)<-c("eCnj", "kaaCnj", "OtherCnj", "Ind")
ti.all.auto.observed<-subset(AWnimp, TI, OrderType, drop=TRUE)
ti.all.auto.predicted<-colnames(ti.all.auto)[apply(ti.all.auto, 1, which.max)]
ti.all.modelstats.auto<-model.statistics(ti.all.auto.observed, ti.all.auto.predicted, ti.all.auto)

ta.all.auto<-t(apply(cbind(fitted(ta.e.all.glmer.auto),fitted(ta.kaa.all.glmer.auto), fitted(ta.other.all.glmer.auto), fitted(ta.ind.all.glmer.auto)), 1, function(x) x/sum(x)))
colnames(ta.all.auto)<-c("eCnj", "kaaCnj", "OtherCnj", "Ind")
ta.all.auto.observed<-subset(AWnimp, TA, OrderType, drop=TRUE)
ta.all.auto.predicted<-colnames(ta.all.auto)[apply(ta.all.auto, 1, which.max)]
ta.all.modelstats.auto<-model.statistics(ta.all.auto.observed, ta.all.auto.predicted, ta.all.auto)

## Save Model stats##
sink(file = 'Auto/ii.all.modelstats.auto.txt')
print(ii.Ind.all.ModelStats.auto, max.print = NA)
sink(NULL)

sink(file = 'Auto/ai.all.modelstats.auto.txt')
print(ai.Ind.all.ModelStats.auto, max.print = NA)
sink(NULL)

sink(file = 'Auto/ti.all.modelstats.auto.txt')
print(ti.Ind.all.ModelStats.auto, max.print = NA)
sink(NULL)

sink(file = 'Auto/ta.all.modelstats.auto.txt')
print(ta.Ind.all.ModelStats.auto, max.print = NA)
sink(NULL)

sink(file = 'Auto/ii-ind-all-ModelStats-auto.txt')
print(ii.Ind.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ai-ind-all-ModelStats-auto.txt')
print(ai.Ind.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ti-ind-all-ModelStats-auto.txt')
print(ti.Ind.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ta-ind-all-ModelStats-auto.txt')
print(ta.Ind.all.ModelStats.auto, max.print = NA)
sink(NULL)

sink(file = 'Auto/ii-ecnj-all-ModelStats-auto.txt')
print(ii.eCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ai-ecnj-all-ModelStats-auto.txt')
print(ai.eCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ti-ecnj-all-ModelStats-auto.txt')
print(ti.eCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ta-ecnj-all-ModelStats-auto.txt')
print(ta.eCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)

sink(file = 'Auto/ii-kaacnj-all-ModelStats-auto.txt')
print(ii.kaaCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ai-kaacnj-all-ModelStats-auto.txt')
print(ai.kaaCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ti-kaacnj-all-ModelStats-auto.txt')
print(ti.kaaCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ta-kaacnj-all-ModelStats-auto.txt')
print(ta.kaaCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)

sink(file = 'Auto/ii-othercnj-all-ModelStats-auto.txt')
print(ii.OtherCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ai-othercnj-all-ModelStats-auto.txt')
print(ai.OtherCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ti-othercnj-all-ModelStats-auto.txt')
print(ti.OtherCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)
sink(file = 'Auto/ta-othercnj-all-ModelStats-auto.txt')
print(ta.OtherCnj.all.ModelStats.auto, max.print = NA)
sink(NULL)


