import numpy as np
import json
import copy


def resize_bag(arr, new_len, v):
  if new_len > len(arr):
    return arr + [v] * (new_len - len(arr))
  else:
    return arr[0:new_len]

def init_with_json(data, name):
  return data[name]



db = []
all_labels = []
label_sum = 0.0
db_size = 0.0
positive_features = []
negative_features = []
positive_feature_sum_j = 0.0
negative_feature_sum_j = 0.0
positive_fraction = 0.0
negative_fraction = 0.0
theta_positive = np.array([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,])
theta_negative = np.array([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,])
w = np.array([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,])
b_collection = np.array([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,])
b = 0.0
t_in = np.array([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,])
i = 0
t_out_label = 0.0
t_label_sum = 0.0
j = 0
t_feature = 0.0
t_sum = 0.0


input_data = json.load(open('fuzzi-gen/fuzzi/data/spambase/spam.json'))
try:
  db = init_with_json(input_data, 'db')
except KeyError:
  pass
try:
  all_labels = init_with_json(input_data, 'all_labels')
except KeyError:
  pass
try:
  label_sum = init_with_json(input_data, 'label_sum')
except KeyError:
  pass
try:
  db_size = init_with_json(input_data, 'db_size')
except KeyError:
  pass
try:
  positive_features = init_with_json(input_data, 'positive_features')
except KeyError:
  pass
try:
  negative_features = init_with_json(input_data, 'negative_features')
except KeyError:
  pass
try:
  positive_feature_sum_j = init_with_json(input_data, 'positive_feature_sum_j')
except KeyError:
  pass
try:
  negative_feature_sum_j = init_with_json(input_data, 'negative_feature_sum_j')
except KeyError:
  pass
try:
  positive_fraction = init_with_json(input_data, 'positive_fraction')
except KeyError:
  pass
try:
  negative_fraction = init_with_json(input_data, 'negative_fraction')
except KeyError:
  pass
try:
  theta_positive = np.array(init_with_json(input_data, 'theta_positive'))
except KeyError:
  pass
try:
  theta_negative = np.array(init_with_json(input_data, 'theta_negative'))
except KeyError:
  pass
try:
  w = np.array(init_with_json(input_data, 'w'))
except KeyError:
  pass
try:
  b_collection = np.array(init_with_json(input_data, 'b_collection'))
except KeyError:
  pass
try:
  b = init_with_json(input_data, 'b')
except KeyError:
  pass
try:
  t_in = np.array(init_with_json(input_data, 't_in'))
except KeyError:
  pass
try:
  i = init_with_json(input_data, 'i')
except KeyError:
  pass
try:
  t_out_label = init_with_json(input_data, 't_out_label')
except KeyError:
  pass
try:
  t_label_sum = init_with_json(input_data, 't_label_sum')
except KeyError:
  pass
try:
  j = init_with_json(input_data, 'j')
except KeyError:
  pass
try:
  t_feature = init_with_json(input_data, 't_feature')
except KeyError:
  pass
try:
  t_sum = init_with_json(input_data, 't_sum')
except KeyError:
  pass


db_size = np.random.laplace(float(len(db)), 1.0)
"""begin extension hint: bmap"""
i = 0
all_labels = copy.deepcopy([])
all_labels = resize_bag(all_labels, len(db), 0.0)
while i < len(db):
  t_in = np.array(db[i])
  if t_in[57] == 1.0:
    t_out_label = 1.0
  else:
    t_out_label = 0.0
  all_labels[i] = t_out_label
  i = i + 1
"""end extension hint: bmap"""
i = 0
label_sum = 0.0
t_label_sum = 0.0
"""begin extension hint: bsum"""
i = 0
label_sum = 0.0
while i < len(all_labels):
  t_label_sum = all_labels[i]
  if t_label_sum < -1.0 * 1.0:
    label_sum = label_sum - 1.0
  else:
    if t_label_sum > 1.0:
      label_sum = label_sum + 1.0
    else:
      label_sum = label_sum + t_label_sum
  i = i + 1
"""end extension hint: bsum"""
label_sum = np.random.laplace(label_sum, 1.0)
positive_fraction = label_sum / db_size
if positive_fraction < 0.0:
  positive_fraction = 1.0e-3
else:
  if positive_fraction > 1.0:
    positive_fraction = 0.999
  else:
    pass
negative_fraction = 1.0 - positive_fraction
"""begin extension hint: repeat"""
j = 0
while j < 57:
  """begin extension hint: bmap"""
  i = 0
  positive_features = copy.deepcopy([])
  positive_features = resize_bag(positive_features, len(db), 0.0)
  while i < len(db):
    t_in = np.array(db[i])
    if t_in[57] == 0.0:
      t_feature = t_in[j]
    else:
      t_feature = 0.0
    positive_features[i] = t_feature
    i = i + 1
  """end extension hint: bmap"""
  """begin extension hint: bmap"""
  i = 0
  negative_features = copy.deepcopy([])
  negative_features = resize_bag(negative_features, len(db), 0.0)
  while i < len(db):
    t_in = np.array(db[i])
    if t_in[57] == 1.0:
      t_feature = t_in[j]
    else:
      t_feature = 0.0
    negative_features[i] = t_feature
    i = i + 1
  """end extension hint: bmap"""
  positive_feature_sum_j = 0.0
  negative_feature_sum_j = 0.0
  t_sum = 0.0
  """begin extension hint: bsum"""
  i = 0
  positive_feature_sum_j = 0.0
  while i < len(positive_features):
    t_sum = positive_features[i]
    if t_sum < -1.0 * 1.0:
      positive_feature_sum_j = positive_feature_sum_j - 1.0
    else:
      if t_sum > 1.0:
        positive_feature_sum_j = positive_feature_sum_j + 1.0
      else:
        positive_feature_sum_j = positive_feature_sum_j + t_sum
    i = i + 1
  """end extension hint: bsum"""
  """begin extension hint: bsum"""
  i = 0
  positive_feature_sum_j = 0.0
  while i < len(negative_features):
    t_sum = negative_features[i]
    if t_sum < -1.0 * 1.0:
      positive_feature_sum_j = positive_feature_sum_j - 1.0
    else:
      if t_sum > 1.0:
        positive_feature_sum_j = positive_feature_sum_j + 1.0
      else:
        positive_feature_sum_j = positive_feature_sum_j + t_sum
    i = i + 1
  """end extension hint: bsum"""
  positive_feature_sum_j = np.random.laplace(positive_feature_sum_j, 10.0)
  negative_feature_sum_j = np.random.laplace(negative_feature_sum_j, 10.0)
  theta_positive[j] = positive_feature_sum_j / label_sum
  theta_negative[j] = negative_feature_sum_j / label_sum
  if theta_positive[j] < 0.0:
    theta_positive[j] = 1.0e-3
  else:
    if theta_positive[j] > 1.0:
      theta_positive[j] = 0.999
    else:
      pass
  if theta_negative[j] < 0.0:
    theta_negative[j] = 1.0e-3
  else:
    if theta_negative[j] > 1.0:
      theta_negative[j] = 0.999
    else:
      pass
  w[j] = np.log(theta_positive[j] / theta_negative[j]) - np.log((1.0 - theta_positive[j]) / (1.0 - theta_negative[j]))
  b_collection[j] = np.log((1.0 - theta_positive[j]) / (1.0 - theta_negative[j])) - np.log(negative_fraction / positive_fraction)
  j = j + 1
"""end extension hint: repeat"""
b = 0.0
i = 0
while i < len(b_collection):
  b = b + b_collection[i]
  i = i + 1


