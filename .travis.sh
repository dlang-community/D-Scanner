#!/bin/bash

set -e

if [[ $BUILD == dub ]]; then
    dub test
elif [[ $DC == ldc2 ]]; then
    git submodule update --init --recursive
    make test DC=ldmd2
else
    git submodule update --init --recursive
    make test
    make lint
    git clone https://www.github.com/dlang/phobos.git --depth=1
    # just check that it doesn't crash
    cd phobos/std && ../../bin/dscanner -S --config=../.dscanner.ini || true
fi
