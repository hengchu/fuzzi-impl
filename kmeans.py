"""
Fuzzi source code: 
k_INFINITY = 1.0e8;
i = 0;
length(parts) = 3;
while i < 3 do
  t_part = (parts)[i];
  length(t_part) = 0;
  (parts)[i] = t_part;
  i = i + 1;
end;
i = 0;
out_idx = {};
length(out_idx) = length(db);
while i < length(db) do
  t_in = (db)[i];
  { { min_dist = k_INFINITY;
      this_dist = 0.0;
      t_out = 0;
      clear_idx = 0;
      while clear_idx < 3 do
        { this_dist = (((cs)[clear_idx])[0] - (t_in)[0]) * (((cs)[clear_idx])[0] - (t_in)[0]) + (((cs)[clear_idx])[1] - (t_in)[1]) * (((cs)[clear_idx])[1] - (t_in)[1]) + (((cs)[clear_idx])[2] - (t_in)[2]) * (((cs)[clear_idx])[2] - (t_in)[2]) + (((cs)[clear_idx])[3] - (t_in)[3]) * (((cs)[clear_idx])[3] - (t_in)[3]);
          if this_dist < min_dist then
            min_dist = this_dist;
            t_out = clear_idx;
          else
            skip;
          end
        };
        clear_idx = clear_idx + 1;
      end;
      min_dist = 0.0;
      this_dist = 0.0
    }
  };
  (out_idx)[i] = t_out;
  i = i + 1;
end;
i = 0;
while i < length(out_idx) do
  t_idx = (out_idx)[i];
  if 0 <= t_idx && t_idx < length(out_idx) then
    t_part = (parts)[t_idx];
    length(t_part) = length(t_part) + 1;
    (t_part)[length(t_part) - 1] = (db)[i];
    (parts)[t_idx] = t_part;
  else
    skip;
  end;
  i = i + 1;
end;
clear_idx = 0;
while clear_idx < 3 do
  { this_partition = (parts)[clear_idx];
    k = 0;
    while k < 5 do
      { i = 0;
        i = 0;
        this_partition_k = {};
        length(this_partition_k) = length(this_partition);
        while i < length(this_partition) do
          t_in = (this_partition)[i];
          { { t_coord_k = (t_in)[clear_idx]
            }
          };
          (this_partition_k)[i] = t_coord_k;
          i = i + 1;
        end;
        t_coord_k_sum = 0.0;
        t_coord_k = 0.0;
        i = 0;
        i = 0;
        t_coord_k_sum = 0.0;
        while i < length(this_partition_k) do
          t_coord_k = (this_partition_k)[i];
          if t_coord_k < -1.0 * 8.0 then
            t_coord_k_sum = t_coord_k_sum - 8.0;
          else
            if t_coord_k > 8.0 then
              t_coord_k_sum = t_coord_k_sum + 8.0;
            else
              t_coord_k_sum = t_coord_k_sum + t_coord_k;
            end;
          end;
          i = i + 1;
        end;
        part_size $= lap(1.0, fc(length(this_partition_k)));
        t_coord_k_sum $= lap(1.0, t_coord_k_sum);
        t_coord_k_sum = t_coord_k_sum / part_size;
        cs_j = (cs)[clear_idx];
        (cs_j)[k] = t_coord_k_sum;
        (cs)[clear_idx] = cs_j
      };
      k = k + 1;
    end
  };
  clear_idx = clear_idx + 1;
end
"""


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
cs = np.array([])
parts = []
t_in = np.array([0.0, 0.0, 0.0, 0.0, 0.0,])
i = 0
t_out = 0
t_idx = 0
out_idx = []
t_part = []
min_dist = 0.0
this_dist = 0.0
clear_idx = 0
k_INFINITY = 0.0
k = 0
cs_j = np.array([0.0, 0.0, 0.0, 0.0, 0.0,])
this_partition = []
this_partition_k = []
t_coord_k = 0.0
t_coord_k_sum = 0.0
part_size = 0.0


input_data = json.load(open('testdata/iris.json'))
try:
  db = init_with_json(input_data, 'db')
except KeyError:
  pass
try:
  cs = np.array(init_with_json(input_data, 'cs'))
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
  clear_idx = init_with_json(input_data, 'clear_idx')
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


k_INFINITY = 1.0e8
i = 0
parts = resize_bag(parts, 3, [])
while i < 3:
  t_part = copy.deepcopy(parts[i])
  t_part = resize_bag(t_part, 0, np.array([0.0, 0.0, 0.0, 0.0, 0.0,]))
  parts[i] = copy.deepcopy(t_part)
  i = i + 1
i = 0
out_idx = copy.deepcopy([])
out_idx = resize_bag(out_idx, len(db), 0)
while i < len(db):
  t_in = np.array(db[i])
  min_dist = k_INFINITY
  this_dist = 0.0
  t_out = 0
  clear_idx = 0
  while clear_idx < 3:
    this_dist = (cs[clear_idx][0] - t_in[0]) * (cs[clear_idx][0] - t_in[0]) + (cs[clear_idx][1] - t_in[1]) * (cs[clear_idx][1] - t_in[1]) + (cs[clear_idx][2] - t_in[2]) * (cs[clear_idx][2] - t_in[2]) + (cs[clear_idx][3] - t_in[3]) * (cs[clear_idx][3] - t_in[3])
    if this_dist < min_dist:
      min_dist = this_dist
      t_out = clear_idx
    else:
      pass
    clear_idx = clear_idx + 1
  min_dist = 0.0
  this_dist = 0.0
  out_idx[i] = t_out
  i = i + 1
i = 0
while i < len(out_idx):
  t_idx = out_idx[i]
  if 0 <= t_idx and t_idx < len(out_idx):
    t_part = copy.deepcopy(parts[t_idx])
    t_part = resize_bag(t_part, len(t_part) + 1, np.array([0.0, 0.0, 0.0, 0.0, 0.0,]))
    t_part[len(t_part) - 1] = np.array(db[i])
    parts[t_idx] = copy.deepcopy(t_part)
  else:
    pass
  i = i + 1
clear_idx = 0
while clear_idx < 3:
  this_partition = copy.deepcopy(parts[clear_idx])
  k = 0
  while k < 5:
    i = 0
    i = 0
    this_partition_k = copy.deepcopy([])
    this_partition_k = resize_bag(this_partition_k, len(this_partition), 0.0)
    while i < len(this_partition):
      t_in = np.array(this_partition[i])
      t_coord_k = t_in[clear_idx]
      this_partition_k[i] = t_coord_k
      i = i + 1
    t_coord_k_sum = 0.0
    t_coord_k = 0.0
    i = 0
    i = 0
    t_coord_k_sum = 0.0
    while i < len(this_partition_k):
      t_coord_k = this_partition_k[i]
      if t_coord_k < -1.0 * 8.0:
        t_coord_k_sum = t_coord_k_sum - 8.0
      else:
        if t_coord_k > 8.0:
          t_coord_k_sum = t_coord_k_sum + 8.0
        else:
          t_coord_k_sum = t_coord_k_sum + t_coord_k
      i = i + 1
    part_size = np.random.laplace(float(len(this_partition_k)), 1.0)
    t_coord_k_sum = np.random.laplace(t_coord_k_sum, 1.0)
    t_coord_k_sum = t_coord_k_sum / part_size
    cs_j = np.array(cs[clear_idx])
    cs_j[k] = t_coord_k_sum
    cs[clear_idx] = np.array(cs_j)
    k = k + 1
  clear_idx = clear_idx + 1


