import copy
import pickle
import csv

#Read in noun,def files
with open('/Users/atticus/NA.tsv', 'r+') as fin:
    NA_def=fin.read().splitlines() 
with open('/Users/atticus/NI.tsv', 'r+') as fin:
    NI_def=fin.read().splitlines()
with open('/Users/atticus/NDA.tsv', 'r+') as fin:
    NDA_def=fin.read().splitlines () 
with open('/Users/atticus/NDI.tsv', 'r+') as fin:
    NDI_def=fin.read().splitlines() 

#Read in Noun,clust files:
with open('/Users/Atticus/crk/AtticusClustering/AEWComparisons/AEW_NA_Cluster', 'r+') as fin:
    NAclust=fin.read().splitlines() 
NAclust= [i.replace('"', '') for i in NAclust]
NaCl=[]
for i in NAclust:
    NaCl.append(i.split(','))
del NaCl[0]
NaDef=[]
for i in NA_def:
    NaDef.append(i.split(','))
NaCl_dict={words[0]:words[1:] for words in NaCl}
NaDef_dict={words[0]:words[1:] for words in NaDef}


NA_dicts=NaCl_dict, NaDef_dict
NA_cluster_defined={k:[d.get(k) for d in NA_dicts] for k in {k for d in NA_dicts for k in d}}


with open('NA_defined.csv', 'w') as csv_file:
    writer = csv.writer(csv_file)
    for key, value in NA_cluster_defined.items():
        writer.writerow([key, value])
##########################################################################################################################

with open('/Users/Atticus/crk/AtticusClustering/AEWComparisons/AEW_NI_Cluster', 'r+') as fin:
    NIclust=fin.read().splitlines() 
NIclust= [i.replace('"', '') for i in NIclust]
NiCl=[]
for i in NIclust: 
    NiCl.append(i.split(','))
del NiCl[0]
NiDef=[]
for i in NI_def:
    NiDef.append(i.split(','))

NiCl_dict={words[0]:words[1:] for words in NiCl}
NiDef_dict={words[0]:words[1:] for words in NiDef}


NI_dicts=NiCl_dict, NiDef_dict
NI_cluster_defined={k:[d.get(k) for d in NI_dicts] for k in {k for d in NI_dicts for k in d}}


with open('NI_defined.csv', 'w') as csv_file:
    writer = csv.writer(csv_file)
    for key, value in NI_cluster_defined.items():
        writer.writerow([key, value])

########################################################################
with open('/Users/Atticus/crk/AtticusClustering/AEWComparisons/AEW_NDA_Cluster', 'r+') as fin:
    NDAclust=fin.read().splitlines() 
NDAclust= [i.replace('"', '') for i in NDAclust]

NdaCl=[]
for i in NDAclust:
    NdaCl.append(i.split(','))

del NdaCl[0]
NdaDef=[]
for i in NDA_def:
    NdaDef.append(i.split(','))

NdaCl_dict={words[0]:words[1:] for words in NdaCl}
NdaDef_dict={words[0]:words[1:] for words in NdaDef}


NDA_dicts=NdaCl_dict, NdaDef_dict
NDA_cluster_defined={k:[d.get(k) for d in NDA_dicts] for k in {k for d in NDA_dicts for k in d}}


with open('NDA_defined.csv', 'w') as csv_file:
    writer = csv.writer(csv_file)
    for key, value in NDA_cluster_defined.items():
        writer.writerow([key, value])

############################################################################################
with open('/Users/Atticus/crk/AtticusClustering/AEWComparisons/AEW_NDI_Cluster', 'r+') as fin:
    NDIclust=fin.read().splitlines() 
NDIclust= [i.replace('"', '') for i in NDIclust]

NdiCl=[]
for i in NDIclust:
    NdiCl.append(i.split(','))

del NdiCl[0]
NdiDef=[]
for i in NDI_def:
    NdiDef.append(i.split(','))

NdiCl_dict={words[0]:words[1:] for words in NdiCl}
NdiDef_dict={words[0]:words[1:] for words in NdiDef}


NDI_dicts=NdiCl_dict, NdiDef_dict
NDI_cluster_defined={k:[d.get(k) for d in NDI_dicts] for k in {k for d in NDI_dicts for k in d}}


with open('NDI_defined.csv', 'w') as csv_file:
    writer = csv.writer(csv_file)
    for key, value in NDI_cluster_defined.items():
        writer.writerow([key, value])