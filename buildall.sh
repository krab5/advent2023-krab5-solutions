#!/usr/bin/bash

for i in day*; do
    echo "Building $i"
    cd $i
    ./build.sh
    cd ..
done


