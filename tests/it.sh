#!/bin/bash

set -eu -o pipefail

function section {
	e=$'\e'
	if [ ! -z "${GITHUB_ACTION:-}" ]; then
		echo "::endgroup::"
	fi
	if [ ! -z "${GITHUB_ACTION:-}" ]; then
		echo "::group::$@"
	fi
	echo "$e[1m$@$e[m"
}

function error {
	echo $'\e[31;1mTests have failed.\e[m'
	exit 1
}

function cleanup {
	if [ ! -z "${GITHUB_ACTION:-}" ]; then
		echo "::endgroup::"
	fi
}

DSCANNER_DIR="$(dirname -- $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd ))"

if [ ! -z "${GITHUB_ACTION:-}" ]; then
	echo "::group::Building d-scanner"
fi

trap cleanup EXIT
trap error ERR

if [ -z "${CI:-}" ]; then
	dub build --root="$DSCANNER_DIR"
fi

cd "$DSCANNER_DIR/tests"

# IDE APIs
# --------
# checking that reporting format stays consistent or only gets extended
diff <(../bin/dscanner --report it/autofix_ide/source_autofix.d | jq -S .) <(jq -S . it/autofix_ide/source_autofix.report.json)
diff <(../bin/dscanner --resolveMessage b16 it/autofix_ide/source_autofix.d | jq -S .) <(jq -S . it/autofix_ide/source_autofix.autofix.json)

# CLI tests
# ---------
# check that `dscanner fix` works as expected
section '1. test no changes if EOFing'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
printf "" | ../bin/dscanner fix it/autofix_cli/test.d
diff it/autofix_cli/test.d it/autofix_cli/source.d
section '2. test no changes for simple enter pressing'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
printf "\n" | ../bin/dscanner fix it/autofix_cli/test.d
diff it/autofix_cli/test.d it/autofix_cli/source.d
section '2.1. test no changes entering 0'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
printf "0\n" | ../bin/dscanner fix it/autofix_cli/test.d
diff it/autofix_cli/test.d it/autofix_cli/source.d
section '3. test change applies automatically with --applySingle'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
../bin/dscanner fix --applySingle it/autofix_cli/test.d | grep -F 'Writing changes to it/autofix_cli/test.d'
diff it/autofix_cli/test.d it/autofix_cli/fixed.d
section '4. test change apply when entering "1"'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
printf "1\n" | ../bin/dscanner fix it/autofix_cli/test.d | grep -F 'Writing changes to it/autofix_cli/test.d'
diff it/autofix_cli/test.d it/autofix_cli/fixed.d
section '5. test invalid selection reasks what to apply'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
printf "2\n-1\n1000\na\n1\n" | ../bin/dscanner fix it/autofix_cli/test.d | grep -F 'Writing changes to it/autofix_cli/test.d'
diff it/autofix_cli/test.d it/autofix_cli/fixed.d

# check that `dscanner @myargs.rst` reads arguments from file
section "Test @myargs.rst"
echo "-f" > "myargs.rst"
echo "github" >> "myargs.rst"
echo "lint" >> "myargs.rst"
echo "it/singleissue.d" >> "myargs.rst"
diff it/singleissue_github.txt <(../bin/dscanner "@myargs.rst")
rm "myargs.rst"
