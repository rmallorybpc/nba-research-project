#!/usr/bin/env bash
set -euo pipefail

if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript not found. Install R to run fixture tests." >&2
  exit 1
fi

Rscript tests/run_contract_classification_fixture.R
