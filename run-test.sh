#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"

# Accept test selector from argument or fallback to all (t)
TEST_SELECTOR="${1:-t}"

echo "Running test selector: $TEST_SELECTOR"

emacs -Q --batch \
      -l erl-trace.el \
      -l erl-trace-test.el \
      --eval "(ert-run-tests-batch-and-exit (quote $TEST_SELECTOR))"
