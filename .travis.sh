#!/bin/bash

set -e

if [[ $BUILD == dub ]]; then
    if [[ -n $LIBDPARSE_VERSION ]]; then
        rdmd ./d-test-utils/test_with_package.d $LIBDPARSE_VERSION libdparse -- dub test
    elif [[ -n $DSYMBOL_VERSION ]]; then
        rdmd ./d-test-utils/test_with_package.d $DSYMBOL_VERSION dsymbol -- dub test
    else
        echo 'Cannot run test without LIBDPARSE_VERSION nor DSYMBOL_VERSION environment variable'
        exit 1
    fi
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
