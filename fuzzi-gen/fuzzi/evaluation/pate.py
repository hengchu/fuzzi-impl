import numpy as np

def main():
    from fuzzi.evaluation import pate_train
    from fuzzi.generated import pate_label

    predictions = pate_label.outputs
    truth = [x[-1] for x in pate_label.db_test]
    print(predictions)
    print(truth)
    print('PATE accuracy = %f' % (np.mean(predictions == truth)))
