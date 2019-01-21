from mnist import MNIST
from copy import deepcopy
import json
import numpy as np
import scipy.misc as m
import random
import re

SIZE              = 28
PARTITIONS        = 5
TRAINING_EXAMPLES = 5000
TEST_EXAMPLES     = 100

mndata = MNIST('./MNIST')

images, labels = mndata.load_training()

#5924 zero labels
zero_indices = [i for i, x in enumerate(labels) if x == 0]
#6724 one labels
one_indices = [i for i, x in enumerate(labels) if x == 1]

all_indices = zero_indices + one_indices
random.shuffle(all_indices)
binary_images = [images[i] for i in all_indices]
binary_labels = [labels[i] for i in all_indices]
binary_labels = [binary_labels[i]*2.0 - 1 for i in range(len(binary_labels))]
images_with_labels = [binary_images[i] + [binary_labels[i]] for i in range(TRAINING_EXAMPLES + TEST_EXAMPLES)]

partitions = []
for i in range(PARTITIONS):
    partitions.append([])

for i in range(TRAINING_EXAMPLES):
    img = images_with_labels[i]
    p_idx = random.randrange(PARTITIONS)
    partitions[p_idx].append(img)

data = {}

data['db_partitions'] = partitions

data_json_str = json.dumps(data, indent=2)
with open('testdata/pate.json', 'w') as outfile:
    outfile.write(data_json_str)
