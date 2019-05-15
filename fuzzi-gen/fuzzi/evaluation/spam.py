import json
import numpy as np

SIZE = 57

def main():
    from fuzzi.generated import nb as naiveBayes

    w = naiveBayes.w
    b = naiveBayes.b

    g = open('testdata/spam.json')
    train = json.load(g)
    X = train['db_test']

    X_no_labels = np.array([X[i][0:SIZE] for i in range(len(X))])
    labels = [(X[i][SIZE]*2.0-1.0) for i in range(len(X))]

    results = [np.dot(w, X_no_labels[i])+b for i in range(len(X_no_labels))]

    sign = [1 if (results[i] > 0) else -1 for i in range(len(results))]
    # print ("Our prediction " + str(sign))
    # print ("Truth: " + str(labels))
    print ("w: " + str(np.mean(w)))
    print ("w var: " + str(np.var(w)))
    print ("b: " + str(b))

    correct = 0
    for i in range(len(labels)):
        if (labels[i] * results[i] > 0 ):
            correct = correct + 1


    pos_labels = len([1 for x in labels if x > 0])
    print ('Pos label percent: %f' % (float(pos_labels) / len(labels)))
    print ('Neg label percent: %f' % (1 - float(pos_labels) / len(labels)))
    print ("Accuracy: %f" % (float(correct)/len(labels)))
