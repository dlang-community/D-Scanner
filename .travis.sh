#!/bin/bash

set -e

if [[ $DC == dmd ]]; then
	wget http://downloads.dlang.org/releases/LATEST
	latest=`cat LATEST`
	actual=`dmd --version | grep -Po -m1 v[.0-9\-a-zA-Z]*`
	actual=${actual:1}
	nightly=`echo $actual | grep -Po master`
	if [[ $actual == $latest* ]] && [[ $actual != $latest ]] && [[ -z "$nightly" ]]; then
		old_beta=1
	fi
fi

if [[ -z "$old_beta" ]]; then
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
fi
