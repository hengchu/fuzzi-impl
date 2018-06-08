import csv 
import json
import re

with open('spambase/spambase.data', 'rb') as f:
	reader = csv.reader(f)
	l = list(reader)

# Converting to floats for Fuzzi purposes
for i in range(len(l)):
	for j in range(len(l[0])):
		l[i][j] = float(l[i][j])

data = {}

data['db'] = l

data_json_str = json.dumps(data, indent=4)
data_json_str = re.sub(r'(\d),\s+', r'\1, ', data_json_str)

with open('testdata/spam.json','w') as outfile:
	outfile.write(data_json_str)


print len(l[0])