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
diff <(../bin/dscanner --report it/source_autofix.d | jq -S .) <(jq -S . it/source_autofix.report.json)
diff <(../bin/dscanner --resolveMessage b16 it/source_autofix.d | jq -S .) <(jq -S . it/source_autofix.autofix.json)


