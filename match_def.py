import copy
import pickle
import csv

#open cluster
def dictcomp(file):

    with open(file, 'r+') as fin:
        clustering=fin.read().splitlines()       

    #read into list
    conjclass=[]

    for i in clustering:
        conjclass.append(i.split(','))


    clusters = [[x[0].replace('"', ''), x[1]] for x in conjclass]



    #Open AEW defs, read into dict
    with open('/Users/Atticus/svn/crk/AtticusClustering/AEW_lemma_def.csv', 'r+') as fin:
        AEW_lemma_def=fin.read().splitlines()


    aew=[]
    for i in AEW_lemma_def:
        aew.append(i.split('  '))

    lemmas=[]
    for i in clusters:
        lemmas.append(i[0]) #len 781


    aew_dict={words[0]:words[1:] for words in aew} #len 16161


    #pickle aew dict
    with open('aew_dict.pickle','wb') as f:
        pickle.dump(aew_dict, f)

    #make new list so that I'm not iterating over what I modify
    aew_keys=[]
    for i in aew_dict.keys():
        aew_keys.append(i)

    #make dict of only conjclass lemmas
    for i in aew_keys:
        if i not in lemmas:
            del aew_dict[i]
    not_def=[]
    for i in lemmas:
        if i not in list(aew_dict):
            not_def.append(i)
    not_defined={word:0 for word in not_def}
    #add cluster to aew_dict and not_def
    cluster_dict={words[0]:words[1:] for words in clusters} #len 781

    clust_keys=[]
    for i in cluster_dict:
        clust_keys.append(i)

    for i in clust_keys:
        if i not in aew_dict:
            del cluster_dict[i]
    ## is cluster_dict is same len as aew_dict?

    ##make not_def_clust

    cluster_dict2={words[0]:words[1:] for words in clusters}
    not_def_clust=[]
    for i in clust_keys:
        if i in aew_dict:
            del cluster_dict2[i]

    ## is cluster_dict2 is same len as not_Def
    if len(cluster_dict2) != len(not_def):
        print('somebody set us up the bomb. cluster_dict2 and not_def are not the same length.')
    ##merge two dictionaries:

    ###Not defined dict is just clusterdict2:
    
    ###definitions dict creating
    dicts=cluster_dict, aew_dict
    cluster_defined={k:[d.get(k) for d in dicts] for k in {k for d in dicts for k in d}}

    ##writing the files
    with open(str(file + '_notdefined.csv'), 'w') as csv_file:
        writer = csv.writer(csv_file)
        for key, value in cluster_dict2.items():
            writer.writerow([key, value])

    with open(str(file + '_defined.csv'), 'w') as csv_file:
        writer = csv.writer(csv_file)
        for key, value in cluster_defined.items():
            writer.writerow([key, value])

if __name__ == "__main__":
    import sys
    fib(int(sys.argv[1]))
