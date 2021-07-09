### This script produces 3 files: ./corpus.p which is a pickled version of the final corpus, ./final-corpus.csv which is a csv version of the previous, and ./noautoclass.csv which is a csv of items not accounted for in the automatic classification--these are not included in the previous files.

# Read in files
import csv
from collections import Counter
import pickle

with open('Corpus-no-semclass.csv', 'r+') as fin:
    disambig=fin.readlines()

no_newlines=[]

for string in disambig:
    no_newlines.append(string.replace('\n',''))

words=[]

for entry in no_newlines:
    words.append(entry.split('\t'))

# Make corpus

## Make list of list structure
corpus = [[y.split(' ') for y in x] for x in words]

## Make order correct:
for i in corpus:
    for j in i:
        if j[0] == 'IC':
            j.insert(-1, j.pop(0))

for i in corpus:
    for j in i:
        if j[0] == 'RdplS':
            j.insert(-1, j.pop(0))

for i in corpus:
    for j in i:
        if j[0] == 'RdplW':
            j.insert(-1, j.pop(0))

for i in corpus:
    for j in i:
        if 'PV' in j[0]:
            j.insert(-1, j.pop(0))

## Read in clusters and make dictionary of {word:cluster}:
def readin_cluster(i):
    with open(i, 'r+') as fin:
        b=fin.read().splitlines()
        b_cl=[]
        for i in b:
            b_cl.append(i.split(','))
    return b_cl

na_cl=readin_cluster('/Users/atticus/crk/AtticusClustering/AEWComparisons/autos/AEW_NA_Cluster_auto_fix')
nda_cl=readin_cluster('/Users/atticus/crk/AtticusClustering/AEWComparisons/autos/AEW_NDA_Cluster_auto_fix')
ndi_cl=readin_cluster('/Users/atticus/crk/AtticusClustering/AEWComparisons/autos/AEW_NDI_Cluster_auto_fix')
ni_cl=readin_cluster('/Users/atticus/crk/AtticusClustering/AEWComparisons/autos/AEW_NI_Cluster_auto_fix')
vii_cl=readin_cluster('/Users/atticus/crk/AtticusClustering/AEWComparisons/autos/AEW_VII_auto_fix')
vai_cl=readin_cluster('/Users/atticus/crk/AtticusClustering/AEWComparisons/autos/AEW_VAI_auto_fix')
vti_cl=readin_cluster('/Users/atticus/crk/AtticusClustering/AEWComparisons/autos/AEW_VTI_auto_fix')
vta_cl=readin_cluster('/Users/atticus/crk/AtticusClustering/AEWComparisons/autos/AEW_VTA_auto_fix')
all_list = na_cl+nda_cl+ndi_cl+ni_cl+vii_cl+vai_cl+vti_cl+vta_cl

clu_d={i[0]:i[1] for i in all_list}

key_set = set(clu_d.keys())


# Deal with fact that my things have fewer than the purely auto one... for... reasons:
def readin_cluster2(i):
    with open(i, 'r+') as fin:
        b=fin.read().splitlines()
        b_cl=[]
        for i in b:
            b_cl.append(i.split('@'))
    return b_cl

na_cl_man=readin_cluster2('na-class-final.txt')
nda_cl_man=readin_cluster2('nda-class-final.txt')
ndi_cl_man=readin_cluster2('ndi-class-final.txt')
ni_cl_man=readin_cluster2('ni-class-final.txt')
vii_cl_man=readin_cluster2('vii-class-final.txt')
vai_cl_man=readin_cluster2('vai-class-final.txt')
vti_cl_man=readin_cluster2('vti-class-final.txt')
vta_cl_man=readin_cluster2('vta-class-final.txt')
all_list_man = na_cl_man+nda_cl_man+ndi_cl_man+ni_cl_man+vii_cl_man+vai_cl_man+vti_cl_man+vta_cl_man
clu_d_man={i[1]:i[0] for i in all_list_man}



# Get rid of words not in my classification

incorp=[]
for i in corpus:
    for j in i:
        incorp.append(j[0])



corpinmine=[]
for i in incorp:
    if i in list(clu_d_man.keys()):
        corpinmine.append(i)


# Create new corpus with only verbs (with their nested arguments) if those verb lemma occur in my classification scheme
newcorp=[]
for i in corpus:
    if i[0][0] in corpinmine:
            newcorp.append(i)

# Add cluster to entry if entry is in clu_d; if not, move on. I will deal with this in modelling. 
for i in newcorp:
    for j in i:
            if j[0] in clu_d.keys():
                j.insert(1, clu_d[j[0]])

for i in newcorp:
    for j in i:
            if j[0] in clu_d_man.keys():
                j.insert(1, clu_d_man[j[0]])

# Add actor and goal to all relevant tags
for clause in newcorp:
    for words_with_tags in clause:
            for tags in words_with_tags:
                if 'GOAL' in tags:
                    for i in range(len(words_with_tags)):
                        words_with_tags[i] += '.goal'

for clause in newcorp:
    for words_with_tags in clause:
            for tags in words_with_tags:
                if 'ACTOR' in tags:
                    for i in range(len(words_with_tags)):
                        words_with_tags[i] += '.actor'

# Find items without an autoclass 
dontneedtodo=[]
needtodo=[]

for clause in newcorp:
    for entry in clause:
        if any("auto" in tag for tag in entry):
            dontneedtodo.append(clause)
        else:
            needtodo.append(clause)

## Save corpus
with open('corpus.p', 'wb') as f:
    pickle.dump(dontneedtodo, f)

with open("final-corpus.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerows(dontneedtodo)

with open("noautoclass.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerows(needtodo)