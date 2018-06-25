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

for i in range(N):
        smaller_images.append(resize(binary_images[i]))
        rand.append(random.randint(0, NUM_PARTITIONS-1))

# Uncomment this line to look at the pixels
# preview(smaller_images[101])

data = {}

#Create one array with images and labels
images_with_labels = [smaller_images[i] + [binary_labels[i]] for i in range(N)]

# with open('testdata/mnist.json','w') as outfile:
#       outfile.write('{\n')
#       for i in range(N):
#               data['x' + str(i)] = smaller_images[i]
#               data['y' + str(i)] = binary_labels[i]
#               outfile.write('\t\"x' + str(i) + '\": ' + str(smaller_images[i]) + ',\n')
#               outfile.write('\t\"y' + str(i) + '\": ' + str(binary_labels[i]) + ',\n')
#               outfile.write('\t\"r' + str(i) + '\": ' + str(rand[i]) + ',\n')
#       outfile.write('}')

data['db'] = images_with_labels
data['trow1'] = [1.0] + 785*[0.0]
data['db1'] = []
data['wout'] = 785*[0.0]
data['dws'] = []
data['dws_j'] = []
data['w'] = 785*[0.0]
data['w_total'] = []
data['j_sum'] = 0.0

data_json_str = json.dumps(data, indent=4)

data_json_str = re.sub(r'(\d),\s+', r'\1, ', data_json_str)

with open('testdata/mnist.json','w') as outfile:
        # outfile.write('{\n')
        # outfile.write('\t\"db": [\n')
        # for i in range(0,N):
        #       data['x' + str(i)] = smaller_images[i]
        #       data['y' + str(i)] = binary_labels[i]
        #       outfile.write('\t\t' +  str(images_with_labels[i]))
        #       if (i != N-1):
        #                outfile.write(',\n')
        # outfile.write('\n')
        # outfile.write('\t],\n')
        # outfile.write('       \"trow1\": [1.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 4.000001, 38.000001, 47.000001, 5.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 4.000001, 99.000001, 201.000001, 212.000001, 67.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 48.000001, 196.000001, 95.000001, 153.000001, 122.000001, 0.000001, 0.000001, 0.000001, 0.000001, 8.000001, 138.000001, 122.000001, 4.000001, 108.000001, 121.000001, 0.000001, 0.000001, 0.000001, 0.000001, 39.000001, 181.000001, 36.000001, 8.000001, 143.000001, 83.000001, 0.000001, 0.000001, 0.000001, 0.000001, 92.000001, 135.000001, 6.000001, 72.000001, 164.000001, 22.000001, 0.000001, 0.000001, 0.000001, 0.000001, 121.000001, 124.000001, 96.000001, 181.000001, 82.000001, 1.000001, 0.000001, 0.000001, 0.000001, 0.000001, 71.000001, 180.000001, 161.000001, 68.000001, 6.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 3.000001, 20.000001, 12.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001],\n\t\"db1\": [],\n\t      \"wout\": [0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001, 0.0000000000000001],\n\t     \n\t\"dws\": [],\"dws_j\": [], \n\t \"w\": [0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 4.000001, 38.000001, 47.000001, 5.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 4.000001, 99.000001, 201.000001, 212.000001, 67.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 48.000001, 196.000001, 95.000001, 153.000001, 122.000001, 0.000001, 0.000001, 0.000001, 0.000001, 8.000001, 138.000001, 122.000001, 4.000001, 108.000001, 121.000001, 0.000001, 0.000001, 0.000001, 0.000001, 39.000001, 181.000001, 36.000001, 8.000001, 143.000001, 83.000001, 0.000001, 0.000001, 0.000001, 0.000001, 92.000001, 135.000001, 6.000001, 72.000001, 164.000001, 22.000001, 0.000001, 0.000001, 0.000001, 0.000001, 121.000001, 124.000001, 96.000001, 181.000001, 82.000001, 1.000001, 0.000001, 0.000001, 0.000001, 0.000001, 71.000001, 180.000001, 161.000001, 68.000001, 6.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 3.000001, 20.000001, 12.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001],\n\t "w_total": [],\n\t\"j_sum\": 0.00000001')
        # outfile.write('}')

        outfile.write(data_json_str)
        # json.dump(data, outfile, indent=4, sort_keys=True)
        # (scrapped this since I couldn't figure out a pretty printing)
