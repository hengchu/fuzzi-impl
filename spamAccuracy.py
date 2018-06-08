import json
import numpy as np

SIZE = 57

f = open('output.json')
data = json.load(f)
w = data['w']
b = data['b']

g = open('testdata/mnist.json')
train = json.load(g)
X = train['db']

X_no_labels = np.array([X[i][0:SIZE] for i in range(len(X))])
labels = [X[i][SIZE] for i in range(len(X))]

results = [np.dot(w, X_no_labels[i])+b for i in range(len(X_no_labels))]

sign = [1 if (results[i] > 0) else -1 for i in range(len(results))]
print "Our prediction " + str(sign)
print "Truth: " + str(labels)
print "w: " + str(np.mean(w))
print "w var: " + str(np.var(w))

correct = 0
for i in range(len(labels)):
	if (labels[i] * results[i] > 0 ):
		correct = correct + 1

print "Accuracy: %f\n" % (float(correct)/len(labels))