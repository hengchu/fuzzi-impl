import numpy as np
import pate_label

predictions = pate_label.outputs
truth = [x[-1] for x in pate_label.db_test]

print('PATE accuracy = %f' % (np.mean(predictions == truth)))
