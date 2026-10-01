#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

command -v python3 >/dev/null
command -v Rscript >/dev/null
Rscript -e 'library(survival)' >/dev/null
shasum -a 256 -c SHA256SUMS.txt >/dev/null

RUN="$ROOT/results_reproduced"
if [ -e "$RUN" ]; then
  echo "Output exists; no files replaced: $RUN" >&2
  exit 1
fi
mkdir "$RUN"

python3 "$ROOT/code/run_primary.py" \
  --project-root "$ROOT" \
  --correction-zip "$ROOT/data/provenance/claude_exclusions.zip" \
  --audit-zip "$ROOT/data/provenance/review_event_audit.zip" \
  --output-parent "$RUN"

REFIT_ZIPS=("$RUN"/trust_corrected_rq1_rq2_refit_*.zip)
if [ "${#REFIT_ZIPS[@]}" -ne 1 ] || [ ! -f "${REFIT_ZIPS[0]}" ]; then
  echo "Expected exactly one new primary ZIP" >&2
  exit 1
fi

python3 "$ROOT/code/run_additional.py" \
  --refit-zip "${REFIT_ZIPS[0]}" \
  --output-parent "$RUN"

CLOSURE_ZIPS=("$RUN"/trust_analysis_closure_*.zip)
if [ "${#CLOSURE_ZIPS[@]}" -ne 1 ] || [ ! -f "${CLOSURE_ZIPS[0]}" ]; then
  echo "Expected exactly one new additional ZIP" >&2
  exit 1
fi

python3 "$ROOT/verify_results.py" \
  --reference-refit "$ROOT/results_reference/archives/primary_models.zip" \
  --new-refit "${REFIT_ZIPS[0]}" \
  --reference-closure "$ROOT/results_reference/archives/additional_models.zip" \
  --new-closure "${CLOSURE_ZIPS[0]}" \
  --report "$RUN/comparison_report.json"

echo "Verified model results: $RUN/comparison_report.json"
