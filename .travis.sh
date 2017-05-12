#!/bin/bash

set -e

dub build --build=release
git submodule update --init --recursive

if [ $DC = ldc2 ]; then
    make test DC=ldmd2
else
    make test
fi
