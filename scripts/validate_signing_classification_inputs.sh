#!/usr/bin/env bash
set -euo pipefail

base_dir="${1:-data/processed}"

events_csv="${base_dir}/signing_events.csv"
thresholds_csv="${base_dir}/cba_thresholds.csv"
awards_csv="${base_dir}/nba_awards.csv"
min_scale_csv="${base_dir}/nba_minimum_scale.csv"

for f in "$events_csv" "$thresholds_csv" "$awards_csv" "$min_scale_csv"; do
  if [[ ! -f "$f" ]]; then
    echo "ERROR: Required input not found: $f" >&2
    exit 1
  fi
done

check_header_exact() {
  local file="$1"
  local expected="$2"
  local actual

  actual="$(head -n 1 "$file")"
  if [[ "$actual" != "$expected" ]]; then
    echo "ERROR: Unexpected header in $file" >&2
    echo "Expected: $expected" >&2
    echo "Found:    $actual" >&2
    exit 1
  fi
}

check_header_contains() {
  local file="$1"
  shift
  local required=("$@")
  local header

  header="$(head -n 1 "$file")"

  for col in "${required[@]}"; do
    if [[ ",${header}," != *",${col},"* ]]; then
      echo "ERROR: Missing required column '$col' in $file" >&2
      echo "Header: $header" >&2
      exit 1
    fi
  done
}

check_header_contains "$events_csv" \
  event_id player_name season signing_team prior_team \
  contract_start_season years_of_service average_annual_value cap_percentage_at_signing

check_header_contains "$thresholds_csv" \
  season salary_cap max_25pct max_30pct max_35pct \
  mle_nontaxpayer mle_taxpayer mle_room bae

check_header_exact "$awards_csv" "season,award,team_level,player"
check_header_contains "$min_scale_csv" season years_of_service minimum_salary

# Duplicate-key guards for join-critical tables.
threshold_dupes="$(awk -F, 'NR>1 {c[$1]++} END {for (k in c) if (c[k] > 1) print k FS c[k]}' "$thresholds_csv" || true)"
if [[ -n "$threshold_dupes" ]]; then
  echo "ERROR: Duplicate seasons in $thresholds_csv (season,count):" >&2
  echo "$threshold_dupes" >&2
  exit 1
fi

min_dupes="$(awk -F, 'NR>1 {k=$1 FS $2; c[k]++} END {for (k in c) if (c[k] > 1) print k FS c[k]}' "$min_scale_csv" || true)"
if [[ -n "$min_dupes" ]]; then
  echo "ERROR: Duplicate (season,years_of_service) rows in $min_scale_csv:" >&2
  echo "$min_dupes" >&2
  exit 1
fi

event_id_dupes="$(awk -F, 'NR>1 {c[$1]++} END {for (k in c) if (c[k] > 1) print k FS c[k]}' "$events_csv" || true)"
if [[ -n "$event_id_dupes" ]]; then
  echo "ERROR: Duplicate event_id values in $events_csv (event_id,count):" >&2
  echo "$event_id_dupes" >&2
  exit 1
fi

echo "Validation passed for classifier inputs in $base_dir"
