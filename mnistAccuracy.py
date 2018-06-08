import json
import numpy as np
import mnist784

SIZE = 784

w = mnist784.w

g = open('testdata/mnist.json')
train = json.load(g)
X = train['db']

X_no_labels = np.array([X[i][0:SIZE] for i in range(len(X))])
labels = [X[i][SIZE] for i in range(len(X))]

results = [np.dot(w[1:], X_no_labels[i])+w[0] for i in range(len(X_no_labels))]

sign = [1 if (results[i] > 0) else -1 for i in range(len(results))]

correct = 0
for i in range(len(labels)):
        if (labels[i] * results[i] > 0 ):
                correct = correct + 1

print("Accuracy: %f\n" % (float(correct)/len(labels)))
