#!/bin/bash

set -x
set -e

mkdir -p doc
id=$(docker create fuzzi-impl)
docker cp $id:/tmp/fuzzi-impl/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/doc ./
docker rm -v $id
