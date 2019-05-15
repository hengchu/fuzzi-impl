import json
import numpy as np
import pkg_resources
import random
import re
import scipy.misc as m

from mnist import MNIST
from copy import deepcopy

SIZE              = 28
PARTITIONS        = 5
TRAINING_EXAMPLES = 5000
TEST_EXAMPLES     = 100

def main():
    mndata = MNIST(pkg_resources.resource_filename('fuzzi', 'data/MNIST'))

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

    # print(len(images_with_labels))

    partitions = []
    w = [0.0] * 785
    for i in range(PARTITIONS):
        partitions.append([[], w])

    for i in range(TRAINING_EXAMPLES):
        img = images_with_labels[i]
        p_idx = random.randrange(PARTITIONS)
        partitions[p_idx][0].append(img)

    models = []

    trained_pate_file = open(pkg_resources.resource_filename('fuzzi', 'data/pate/pate.json'), 'r')
    trained_data = json.load(trained_pate_file)
    for p in trained_data['db_partitions']:
        models.append(p[1])
    trained_pate_file.close()

    data_test = {}
    data_test['db_test'] = images_with_labels[TRAINING_EXAMPLES:TRAINING_EXAMPLES + TEST_EXAMPLES]
    data_test['models'] = models
    with open(pkg_resources.resource_filename('fuzzi', 'data/pate/pate_test.json'), 'w') as outfile:
        outfile.write(json.dumps(data_test, indent=2))

    data = {}
    data['db_partitions'] = partitions
    data_json_str = json.dumps(data, indent=2)
    with open(pkg_resources.resource_filename('fuzzi', 'data/pate/pate.json'), 'w') as outfile:
        outfile.write(data_json_str)
