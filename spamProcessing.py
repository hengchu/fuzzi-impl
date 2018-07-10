import csv 
import json
import re

with open('spambase/spambase.data', 'rb') as f:
	reader = csv.reader(f)
	l = list(reader)

#
# Indices 0-46 are continuous (convert to 0 or 1)
# 47-53 continuous (convert to 0 or 1)
# 54-56 are integers, will use average (0 or 1 threshold)

sum_54 = 0.0
sum_55 = 0.0
sum_56 = 0.0
for i in range(len(l)):
	sum_54 += float(l[i][54])
	sum_55 += float(l[i][55])
	sum_56 += float(l[i][56])
avg_54 = sum_54 / float(len(l))
avg_55 = sum_55 / float(len(l))
avg_56 = sum_56 / float(len(l))

# Converting to floats for Fuzzi purposes
for i in range(len(l)):
	for j in range(len(l[0])):
		l[i][j] = float(l[i][j])
		if (j == 54):
			if (l[i][j] <= avg_54):
				l[i][j] = 0.0
		if (j == 55):
			if (l[i][j] <= avg_55):
				l[i][j] = 0.0
		if (j == 56):
			if (l[i][j] <= avg_56):
				l[i][j] = 0.0
		if (l[i][j] != 0.0):
			l[i][j] = 1.0


data = {}

data['db'] = l

data_json_str = json.dumps(data, indent=4)
data_json_str = re.sub(r'(\d),\s+', r'\1, ', data_json_str)

with open('testdata/spam.json','w') as outfile:
	outfile.write(data_json_str)


print len(l[0])