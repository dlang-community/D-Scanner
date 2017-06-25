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
fi
