import importlib
# execute first time
import pate784
import json
import numpy as np

class NumpyEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        return json.JSONEncoder.default(self, obj)

N = 10

epoch = 0

while epoch < N:
    print('current epoch = %d' % epoch)
    ws_partitions = pate784.ws_partitions
    db_partitions = pate784.db_partitions

    for i in range(len(ws_partitions)):
        db_partitions[i][1] = ws_partitions[i]

    data = {}
    data['db_partitions'] = db_partitions
    data_json_str = json.dumps(data, indent=2, cls=NumpyEncoder)
    with open('testdata/pate.json', 'w') as outfile:
        outfile.write(data_json_str)
    importlib.reload(pate784)
    epoch += 1

# write the data one last time
data = {}
data['db_partitions'] = db_partitions
data_json_str = json.dumps(data, indent=2, cls=NumpyEncoder)
with open('testdata/pate.json', 'w') as outfile:
    outfile.write(data_json_str)