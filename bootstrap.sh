#!/bin/bash

set -x
set -e

echo 'creating python3 virtual environment...'
cd fuzzi-gen
virtualenv -p `which python3.7` ./venv
source venv/bin/activate
pip3 install -r requirements.txt
pip3 install --editable .
cd ..

make fuzzi
make preprocess
make typecheck
make transpile
