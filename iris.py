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
cs1 = np.array([0.0, 0.0, 0.0, 0.0, 0.0,])
cs2 = np.array([0.0, 0.0, 0.0, 0.0, 0.0,])
cs3 = np.array([0.0, 0.0, 0.0, 0.0, 0.0,])
parts = []
t_in = np.array([0.0, 0.0, 0.0, 0.0, 0.0,])
i = 0
t_out = 0
t_idx = 0
out_idx = []
t_part = []
min_dist = 0.0
this_dist = 0.0
j = 0
k = 0
k_INFINITY = 0.0
k = 0
cs_j = np.array([0.0, 0.0, 0.0, 0.0, 0.0,])
this_partition = []
this_partition_k = []
t_coord_k = 0.0
t_coord_k_sum = 0.0
part_size = 0.0
epoch = 0
zero_5 = np.array([0.0, 0.0, 0.0, 0.0, 0.0,])


input_data = json.load(open('testdata/iris.json'))
try:
  db = init_with_json(input_data, 'db')
except KeyError:
  pass
try:
  cs1 = np.array(init_with_json(input_data, 'cs1'))
except KeyError:
  pass
try:
  cs2 = np.array(init_with_json(input_data, 'cs2'))
except KeyError:
  pass
try:
  cs3 = np.array(init_with_json(input_data, 'cs3'))
except KeyError:
  pass
try:
  parts = init_with_json(input_data, 'parts')
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
  t_out = init_with_json(input_data, 't_out')
except KeyError:
  pass
try:
  t_idx = init_with_json(input_data, 't_idx')
except KeyError:
  pass
try:
  out_idx = init_with_json(input_data, 'out_idx')
except KeyError:
  pass
try:
  t_part = init_with_json(input_data, 't_part')
except KeyError:
  pass
try:
  min_dist = init_with_json(input_data, 'min_dist')
except KeyError:
  pass
try:
  this_dist = init_with_json(input_data, 'this_dist')
except KeyError:
  pass
try:
  j = init_with_json(input_data, 'j')
except KeyError:
  pass
try:
  k = init_with_json(input_data, 'k')
except KeyError:
  pass
try:
  k_INFINITY = init_with_json(input_data, 'k_INFINITY')
except KeyError:
  pass
try:
  k = init_with_json(input_data, 'k')
except KeyError:
  pass
try:
  cs_j = np.array(init_with_json(input_data, 'cs_j'))
except KeyError:
  pass
try:
  this_partition = init_with_json(input_data, 'this_partition')
except KeyError:
  pass
try:
  this_partition_k = init_with_json(input_data, 'this_partition_k')
except KeyError:
  pass
try:
  t_coord_k = init_with_json(input_data, 't_coord_k')
except KeyError:
  pass
try:
  t_coord_k_sum = init_with_json(input_data, 't_coord_k_sum')
except KeyError:
  pass
try:
  part_size = init_with_json(input_data, 'part_size')
except KeyError:
  pass
try:
  epoch = init_with_json(input_data, 'epoch')
except KeyError:
  pass
try:
  zero_5 = np.array(init_with_json(input_data, 'zero_5'))
except KeyError:
  pass


epoch = 0
k_INFINITY = 1.0e8
pass
"""begin extension hint: repeat"""
epoch = 0
while epoch < 5:
  """begin extension hint: partition"""
  i = 0
  parts = resize_bag(parts, 3, [])
  while i < 3:
    t_part = copy.deepcopy(parts[i])
    t_part = resize_bag(t_part, 0, np.array([0.0, 0.0, 0.0, 0.0, 0.0,]))
    parts[i] = copy.deepcopy(t_part)
    i = i + 1
  """begin extension hint: bmap"""
  i = 0
  out_idx = copy.deepcopy([])
  out_idx = resize_bag(out_idx, len(db), 0)
  while i < len(db):
    t_in = np.array(db[i])
    min_dist = k_INFINITY
    this_dist = 0.0
    t_out = 0
    """begin extension hint: compute_dist"""
    this_dist = (cs1[0] - t_in[0]) * (cs1[0] - t_in[0]) + (cs1[1] - t_in[1]) * (cs1[1] - t_in[1]) + (cs1[2] - t_in[2]) * (cs1[2] - t_in[2]) + (cs1[3] - t_in[3]) * (cs1[3] - t_in[3])
    """end extension hint: compute_dist"""
    if this_dist < min_dist:
      min_dist = this_dist
      t_out = 0
    else:
      pass
    """begin extension hint: compute_dist"""
    this_dist = (cs2[0] - t_in[0]) * (cs2[0] - t_in[0]) + (cs2[1] - t_in[1]) * (cs2[1] - t_in[1]) + (cs2[2] - t_in[2]) * (cs2[2] - t_in[2]) + (cs2[3] - t_in[3]) * (cs2[3] - t_in[3])
    """end extension hint: compute_dist"""
    if this_dist < min_dist:
      min_dist = this_dist
      t_out = 1
    else:
      pass
    """begin extension hint: compute_dist"""
    this_dist = (cs3[0] - t_in[0]) * (cs3[0] - t_in[0]) + (cs3[1] - t_in[1]) * (cs3[1] - t_in[1]) + (cs3[2] - t_in[2]) * (cs3[2] - t_in[2]) + (cs3[3] - t_in[3]) * (cs3[3] - t_in[3])
    """end extension hint: compute_dist"""
    if this_dist < min_dist:
      min_dist = this_dist
      t_out = 2
    else:
      pass
    min_dist = 0.0
    this_dist = 0.0
    out_idx[i] = t_out
    i = i + 1
  """end extension hint: bmap"""
  i = 0
  while i < len(out_idx):
    t_idx = out_idx[i]
    if 0 <= t_idx and t_idx < len(parts):
      t_part = copy.deepcopy(parts[t_idx])
      t_part = resize_bag(t_part, len(t_part) + 1, np.array([0.0, 0.0, 0.0, 0.0, 0.0,]))
      t_part[len(t_part) - 1] = np.array(db[i])
      parts[t_idx] = copy.deepcopy(t_part)
    else:
      pass
    i = i + 1
  """end extension hint: partition"""
  pass
  pass
  """begin extension hint: repeat"""
  j = 0
  while j < 3:
    this_partition = copy.deepcopy(parts[j])
    #part_size = np.random.laplace(float(len(this_partition)), 1.0)
    part_size = float(len(this_partition))
    """begin extension hint: repeat"""
    k = 0
    while k < 4:
      """begin extension hint: bmap"""
      i = 0
      this_partition_k = copy.deepcopy([])
      this_partition_k = resize_bag(this_partition_k, len(this_partition), 0.0)
      while i < len(this_partition):
        t_in = np.array(this_partition[i])
        t_coord_k = t_in[k]
        this_partition_k[i] = t_coord_k
        i = i + 1
      """end extension hint: bmap"""
      """begin extension hint: bsum"""
      i = 0
      t_coord_k_sum = 0.0
      while i < len(this_partition_k):
        t_coord_k = this_partition_k[i]
        if t_coord_k < -1.0 * 1.0:
          t_coord_k_sum = t_coord_k_sum - 1.0
        else:
          if t_coord_k > 1.0:
            t_coord_k_sum = t_coord_k_sum + 1.0
          else:
            t_coord_k_sum = t_coord_k_sum + t_coord_k
        i = i + 1
      """end extension hint: bsum"""
      #t_coord_k_sum = np.random.laplace(t_coord_k_sum, 10.0)
      t_coord_k_sum = t_coord_k_sum / part_size
      """begin extension hint: get_cs"""
      if j == 0:
        cs_j = np.array(cs1)
      else:
        if j == 1:
          cs_j = np.array(cs2)
        else:
          cs_j = np.array(cs3)
      """end extension hint: get_cs"""
      cs_j[k] = t_coord_k_sum
      k = k + 1
    """end extension hint: repeat"""
    """begin extension hint: set_cs"""
    if j == 0:
      cs1 = np.array(cs_j)
    else:
      if j == 1:
        cs2 = np.array(cs_j)
      else:
        cs3 = np.array(cs_j)
    """end extension hint: set_cs"""
    j = j + 1
  """end extension hint: repeat"""
  epoch = epoch + 1
"""end extension hint: repeat"""
