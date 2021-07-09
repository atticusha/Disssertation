#!/usr/bin/python3

# by Atticus G. Harrigan, May 25, 2020
# a.ttic.us
# galvin@ualberta.ca

# This script takes 3 arguments:
#    1. A set of pretrained vectors of English as a binary 
#    2. An InDict argument (which is a .csv file with the target-language word first, followed by the English definition, with every word comma seperated, and target-language word seperated by a new line):
#        blug, nice, dog
#        ughn, delicious, apple
#        shmar, stinky, cheese
#    NOTE: stop words should be removed before runing this script if your pre-trained vector model removes them (it probably does)
#    3. An Output argument (a string that names the final result, you probably a .csv file)

# This script will write 3 files to the folder you run this in:
#    1. InDict.pickle (which is a pickle of the Python dictionary object converted form your source csv)
#    2. Averaged_InDict_array.pickle (which is a pickled Python dictionary object with each word defined as a sentence vector)
#    3. Output (which is a file [probably in a csv format, but you can choose what you want] with a word and its sentence vector seperated by a comma)
import sys
import gensim
from gensim.models import KeyedVectors
import pandas as pd
import pickle
import copy
import csv
import numpy as np
from collections import OrderedDict
from itertools import chain
from pathlib import Path

def sentvec(vectors, InDict, Output):
    p = Path(InDict)
    wv = KeyedVectors.load_word2vec_format(vectors, binary=True)
    df = pd.read_csv(InDict, header=None)
    df=df.set_index(0, inplace=False)
    dct = df[1].str.split().agg(set).to_dict()
    
    ## check to see if indict definition words are in your w2v model ###
    defs=list(dct.values())
    deflist=[]
    for group in defs:
        if len(group) == 1:
            deflist.append(''.join(group))
        else:
            for i in group:
                deflist.append(i)
    wordsnotin=[]
    for word in deflist:
        if not word in wv.vocab:
            wordsnotin.append(word)
    with open('words_not_in_model.txt', 'w') as f:
        for i in set(wordsnotin):
            f.write(str(i) + '\n')
    if len(wordsnotin) > 0:
        sys.exit('aaaah! you have definitions in your Indict that are not in your vector model! A list of these has been printed at ./words_not_in_model.txt')
    ## Continue if wordsnotin is empty
    
    else:
        for key in dct.keys():
            words = dct[key]
            subdict = { word : wv[word] for word in words}
            dct[key] = subdict
        for key in dct.keys():
            words = dct[key]
            subdict = { word : wv[word] for word in words}
            dct[key] = subdict
        pickle.dump(dct,open(str(p.stem)+'.pickle', 'wb'))
        tcd=copy.deepcopy(dct)
        for word in dct:
            tcd[word]=sum(dct[word].values()) / len(dct[word])
        pickle.dump(tcd,open('Averaged_array.pickle', 'wb'))
        final = {k:list(v) for (k,v) in tcd.items()}
        with open(Output, 'w') as f:
            csv_writer = csv.writer(f)
            for row in [[k] + v for k, v in final.items()]:
                csv_writer.writerow(row)


    # The following code cas be used if you have a line seperated list of words for a particular part of speech.
    # In this case, I used verbs.

    #    with open('averagedarray.pickle', 'rb') as f:
    #        new_dict = pickle.load(f)
    #    verbs=[]
    #    with open('aew_verbs.txt', 'r') as f:
    #        for i in f.readlines():
    #            verbs.append(i.strip('\n'))
    #    verbonly = {k:v for (k,v) in new_dict.items() if k in verbs}
    #    final = {k:list(v) for (k,v) in verbonly.items()}

if __name__ == "__main__":
    vectors = sys.argv[1]
    InDict =  sys.argv[2]
    Output =  sys.argv[3]
    sentvec(vectors, InDict, Output)
