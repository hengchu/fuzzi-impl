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



x = 0.0
y = 0.0
z = 0.0


input_data = json.load(open('fuzzi-gen/fuzzi/data/other/arithmetic.json'))
try:
  x = init_with_json(input_data, 'x')
except KeyError:
  pass
try:
  y = init_with_json(input_data, 'y')
except KeyError:
  pass
try:
  z = init_with_json(input_data, 'z')
except KeyError:
  pass


z = 2.0 * x
z = z + y


