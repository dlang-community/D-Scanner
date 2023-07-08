#!/bin/bash

set -eu -o pipefail

DSCANNER_DIR="$(dirname -- $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd ))"
dub build --root="$DSCANNER_DIR"

cd "$DSCANNER_DIR/tests"

# IDE APIs
# --------
# checking that reporting format stays consistent or only gets extended
diff <(jq -S . <(../bin/dscanner --report it/source_autofix.d)) <(jq -S . it/source_autofix.report.json)
diff <(jq -S . <(../bin/dscanner --resolveMessage b16 it/source_autofix.d)) <(jq -S . it/source_autofix.autofix.json)


