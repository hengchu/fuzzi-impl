from mnist import MNIST
import json
import numpy as np
import scipy.misc as m
import random
import re
#import matplotlib as mpl
#from matplotlib import pyplot as plt

ORIG_SIZE = 28;
NEW_SIZE = 28;
NUM_PARTITIONS = 10;

#Total number of available samples, can set to less if we don't need all of them
#TRAINING_EXAMPLES = 12665;
TRAINING_EXAMPLES = 1000;
TEST_EXAMPLES = 100

# Resizes images down given a single vector for image
# Also scales values from -1 to 1
def resize(imageVector):
        vec = np.array(imageVector)
        a = np.resize(vec, (ORIG_SIZE, ORIG_SIZE))
        new_a = m.imresize(a, (NEW_SIZE, NEW_SIZE))
        new_vec = new_a.flatten()
        l = new_vec.tolist()
        scaled = [(l[i] -128) /128.0 for i in range(len(l))]
        return scaled

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

binary_labels = [binary_labels[i]*2.0 - 1 for i in range(len(binary_labels))]
#binary_images = [images[i] for i in zero_indices] + [images[i] for i in one_indices]
#binary_labels = [0]*len(zero_indices) + [1]*len(one_indices)

#Number of samples we will take (leq 12665)
N = TRAINING_EXAMPLES

smaller_images = []
rand = []

def preview(img):
    """
    Render a image list into visible image
    """
    img_data = np.array(img)
    img_data = np.reshape(img_data, newshape=((NEW_SIZE, NEW_SIZE)))
    plt.imshow(img_data, cmap=mpl.cm.Greys)
    plt.show()

for i in range(N + TEST_EXAMPLES):
        smaller_images.append(resize(binary_images[i]))
        rand.append(random.randint(0, NUM_PARTITIONS-1))

# Uncomment this line to look at the pixels
# preview(smaller_images[101])

data = {}

#Create one array with images and labels
images_with_labels = [smaller_images[i] + [binary_labels[i]] for i in range(N)]
images_with_labels_test = [smaller_images[i] + [binary_labels[i]] for i in range(N, N+TEST_EXAMPLES)]

data['db'] = images_with_labels
data['db_test'] = images_with_labels_test
data['trow1'] = [1.0] + (NEW_SIZE * NEW_SIZE + 1)*[0.0]
data['db1'] = []
data['wout'] = (NEW_SIZE * NEW_SIZE + 1)*[0.0]
data['dws'] = []
data['dws_j'] = []
data['w'] = (NEW_SIZE * NEW_SIZE + 1)*[0.0]
data['w_total'] = []
data['j_sum'] = 0.0

data_json_str = json.dumps(data, indent=4)

data_json_str = re.sub(r'(\d),\s+', r'\1, ', data_json_str)

with open('testdata/mnist.json','w') as outfile:
        outfile.write(data_json_str)
