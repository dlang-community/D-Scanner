#!/bin/bash

set -eu -o pipefail

DSCANNER_DIR="$(dirname -- $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd ))"

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
echo '1. test no changes if EOFing'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
printf "" | ../bin/dscanner fix it/autofix_cli/test.d
diff it/autofix_cli/test.d it/autofix_cli/source.d
echo '2. test no changes for simple enter pressing'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
printf "\n" | ../bin/dscanner fix it/autofix_cli/test.d
diff it/autofix_cli/test.d it/autofix_cli/source.d
echo '2.1. test no changes entering 0'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
printf "0\n" | ../bin/dscanner fix it/autofix_cli/test.d
diff it/autofix_cli/test.d it/autofix_cli/source.d
echo '3. test change applies automatically with --applySingle'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
../bin/dscanner fix --applySingle it/autofix_cli/test.d | grep -F 'Writing changes to it/autofix_cli/test.d'
diff it/autofix_cli/test.d it/autofix_cli/fixed.d
echo '4. test change apply when entering "1"'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
printf "1\n" | ../bin/dscanner fix it/autofix_cli/test.d | grep -F 'Writing changes to it/autofix_cli/test.d'
diff it/autofix_cli/test.d it/autofix_cli/fixed.d
echo '5. test invalid selection reasks what to apply'
cp -v it/autofix_cli/source.d it/autofix_cli/test.d
printf "2\n-1\n1000\na\n1\n" | ../bin/dscanner fix it/autofix_cli/test.d | grep -F 'Writing changes to it/autofix_cli/test.d'
diff it/autofix_cli/test.d it/autofix_cli/fixed.d

