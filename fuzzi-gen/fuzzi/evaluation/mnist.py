import json
import pkg_resources
import numpy as np

SIZE = 784

def main():
    from fuzzi.generated import mnist784

    w = mnist784.w

    g = open(pkg_resources.resource_filename('fuzzi', 'data/MNIST/mnist784.json'))
    train = json.load(g)
    X = train['db_test']

    X_no_labels = np.array([X[i][0:SIZE] for i in range(len(X))])
    labels = [X[i][SIZE] for i in range(len(X))]

    results = [np.dot(w[1:], X_no_labels[i])+w[0] for i in range(len(X_no_labels))]

    sign = [1 if (results[i] > 0) else -1 for i in range(len(results))]

    correct = 0
    for i in range(len(labels)):
        if (labels[i] * results[i] > 0 ):
            correct = correct + 1

    print ("labels: " + str(labels))
    print ("results: " + str(results))
    print ("w[0]: " + str(w[0]))
    #print "w: " + str(w)
    #print "graident: " + str(mnist784.dws)

    print("Accuracy: %f\n" % (float(correct)/len(labels)))
