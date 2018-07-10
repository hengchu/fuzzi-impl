import csv 
import json
import re
import random

with open('Iris/iris.data', 'rb') as f:
	reader = csv.reader(f)
	l = list(reader)



# Converting to floats for Fuzzi purposes and assigning cluster labels
for i in range(len(l)):
	y = len(l[0])-1
	for j in range(len(l[0])-1):
		l[i][j] = float(l[i][j])
	if (l[i][y] == 'Iris-versicolor'):
		l[i][y] = 2.0
	elif (l[i][y] == 'Iris-virginica'):
		l[i][y] = 1.0
	elif (l[i][y] == 'Iris-setosa'):
		l[i][y] = 0.0

data = {}

data['db'] = l

#Initial cluster centers (can be played with)
x0 = random.randint(0, len(l)-1)
x1 = random.randint(0, len(l)-1)
x2 = random.randint(0, len(l)-1)
data['cs'] = [l[x0], l[x1], l[x2]]

data_json_str = json.dumps(data, indent=4)

with open('testdata/iris.json','w') as outfile:
	outfile.write(data_json_str)