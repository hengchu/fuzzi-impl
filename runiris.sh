for i in `seq 100`; do python irisProcessing.py && python irisAccuracy.py; done | grep accuracy
