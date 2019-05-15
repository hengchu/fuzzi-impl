#!/bin/bash

set -x
set -e

echo 'creating python3 virtual environment...'
cd fuzzi-gen
python3 -m venv ./venv
source venv/bin/activate
pip3 install --editable .
cd ..

make fuzzi
make preprocess
make transpile
