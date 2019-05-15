import json
import numpy as np

SIZE = 57

def main():
    from fuzzi.generated import iris as kmeans

    part = kmeans.parts

    g = open('testdata/iris.json')
    train = json.load(g)
    X = train['db_test']

    print ('our results (0):' + str(len(part[0])))
    print ('our results (1):' + str(len(part[1])))
    print ('our results (2):' + str(len(part[2])))

    total_examples = len(part[0])  + len(part[1]) + len(part[2])

    #Calculating accuracy
    err = 0
    for j in range(0,3):
        labs = [part[j][i][4] for i in range(len(part[j]))]
        lab0 = labs.count(0)
        lab1 = labs.count(1)
        lab2 = labs.count(2)
        m = max(lab0, lab1, lab2)
        assert(len(part[j]) - lab0 - lab1 - lab2 == 0)
        err = err + len(part[j]) - m

    print ("errors: " + str(err))

    print ("Total accuracy: " + str(1-float(err)/total_examples))
