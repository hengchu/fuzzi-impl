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



inputs = []
outputs = np.array([])
private_sum = 0.0
public_sum = 0.0
i = 0
tin = 0.0
adv_comp_iter = 0


input_data = json.load(open('fuzzi-gen/fuzzi/data/other/kitchensink.json'))
try:
  inputs = init_with_json(input_data, 'inputs')
except KeyError:
  pass
try:
  outputs = np.array(init_with_json(input_data, 'outputs'))
except KeyError:
  pass
try:
  private_sum = init_with_json(input_data, 'private_sum')
except KeyError:
  pass
try:
  public_sum = init_with_json(input_data, 'public_sum')
except KeyError:
  pass
try:
  i = init_with_json(input_data, 'i')
except KeyError:
  pass
try:
  tin = init_with_json(input_data, 'tin')
except KeyError:
  pass
try:
  adv_comp_iter = init_with_json(input_data, 'adv_comp_iter')
except KeyError:
  pass


outputs.resize((100,)+outputs.shape[1:])
"""begin extension hint: ac"""
adv_comp_iter = 0
while adv_comp_iter < 100:
  """begin extension hint: bsum"""
  i = 0
  private_sum = 0.0
  while i < len(inputs):
    tin = inputs[i]
    if tin < -1.0 * 50.0:
      private_sum = private_sum - 50.0
    else:
      if tin > 50.0:
        private_sum = private_sum + 50.0
      else:
        private_sum = private_sum + tin
    i = i + 1
  """end extension hint: bsum"""
  public_sum = np.random.laplace(private_sum, 200.0)
  outputs[adv_comp_iter] = public_sum
  private_sum = 0.0
  tin = 0.0
  i = 0
  adv_comp_iter = adv_comp_iter + 1
if 1.0e-6 >= 0.0:
  pass
else:
  pass
"""end extension hint: ac"""


