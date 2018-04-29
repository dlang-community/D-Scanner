#!/usr/bin/env bash
# Build the Windows binaries under Linux (requires wine)
set -eux -o pipefail
VERSION=$(git describe --abbrev=0 --tags)
OS=windows
if [ "${ARCH:-32}" == "64" ] ; then
	ARCH_SUFFIX="x86_64"
	export DFLAGS=-mtriple=x86_64-windows-msvc
else
	ARCH_SUFFIX="x86"
	export DFLAGS=-mtriple=x86_32-windows-msvc
fi

# for the install.sh script only
LDC_PATH="$(dirname $(dirname $(which ldc2)))"

# Allow the script to be run from anywhere
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

# Step 1: download the LDC windows binaries
if [ ! -d ldc2-1.13.0-windows-x64 ] ; then
	wget https://github.com/ldc-developers/ldc/releases/download/v1.13.0/ldc2-1.13.0-windows-x64.7z
	7z x ldc2-1.13.0-windows-x64.7z > /dev/null
fi

# Step 2: Add LDC windows binaries to LDC Linux
if [ ! -d "${LDC_PATH}/lib-win64" ] ; then
	cp -r ldc2-1.13.0-windows-x64/lib "${LDC_PATH}/lib-win64"

	cat >> "$LDC_PATH"/etc/ldc2.conf <<EOF
"x86_64-.*-windows-msvc":
{
	switches = [
		"-defaultlib=phobos2-ldc,druntime-ldc",
		"-link-defaultlib-shared=false",
	];
	lib-dirs = [
		"%%ldcbinarypath%%/../lib-win64",
	];
};
EOF

fi

# Step 3: Run LDC with cross-compilation
archiveName="dscanner-$VERSION-$OS-$ARCH_SUFFIX.zip"
echo "Building $archiveName"
mkdir -p bin
export DC=ldmd2
make

cd bin
zip "$archiveName" dscanner.exe
