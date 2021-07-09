import pickle
import csv
def otherdef(l,cc):
    aewdict = pickle.load(open('/Users/atticus/crk/AtticusClustering/aew_dict.pickle', 'rb'))
    other_dict={}
    for i in l:
        other_dict[i]=aewdict[i]
    with open('/Users/atticus/crk/AtticusClustering/AEWComparisons/' + str(cc) + '_other.csv', 'w') as csv_file:
        writer = csv.writer(csv_file)
        for key, value in other_dict.items():
            writer.writerow([key, value])   

## sed "s/\[//g" vta_other.csv | sed "s/\]//g" | sed "s/\'//g" | sed "s/\,\"/; /g" | sed "s/\"//g" | tr -s ',' >  Desktop/vta_other.csv