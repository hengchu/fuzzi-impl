from mnist import MNIST
import json
import numpy as np
import scipy.misc as m
import random

ORIG_SIZE = 28;
NEW_SIZE = 10;

#Total number of available samples, can set to less if we don't need all of them
TRAINING_EXAMPLES = 12665;

# Resizes images down given a single vector for image
def resize(imageVector):
	vec = np.array(imageVector)
	a = np.resize(vec, (ORIG_SIZE, ORIG_SIZE))
	new_a = m.imresize(a, (NEW_SIZE, NEW_SIZE))
	new_vec = new_a.flatten()
	return new_vec.tolist()

mndata = MNIST('./MNIST')

#Each row of images is 28x28 represented as one vector
#60,000 total examples
images, labels = mndata.load_training()

#5924 zero labels
zero_indices = [i for i, x in enumerate(labels) if x == 0]
#6724 one labels
one_indices = [i for i, x in enumerate(labels) if x == 1]

#Reconstructing the data (total 12665 samples now) - random assortment of 1's and 0's
all_indices = zero_indices + one_indices
random.shuffle(all_indices)
binary_images = [images[i] for i in all_indices]
binary_labels = [labels[i] for i in all_indices]
#binary_images = [images[i] for i in zero_indices] + [images[i] for i in one_indices]
#binary_labels = [0]*len(zero_indices) + [1]*len(one_indices)

#Number of samples we will take (leq 12665)
N = TRAINING_EXAMPLES

smaller_images = []

for i in range(N):
	smaller_images.append(resize(binary_images[i]))

data = {}

with open('testdata/mnist.json','w') as outfile:
	outfile.write('{\n')
	for i in range(N):
		data['x' + str(i)] = smaller_images[i]
		data['y' + str(i)] = binary_labels[i]
		outfile.write('\t\"x' + str(i) + '\": ' + str(smaller_images[i]) + ',\n')
		outfile.write('\t\"y' + str(i) + '\": ' + str(binary_labels[i]) + ',\n')
	outfile.write('}')

	#json.dump(data, outfile, indent=4, sort_keys=True)
	# (scrapped this since I couldn't figure out a pretty printing)