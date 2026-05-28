#!/usr/bin/env bash
set -euo pipefail

csv_path="${1:-nba_awards.csv}"

if [[ ! -f "$csv_path" ]]; then
  echo "ERROR: File not found: $csv_path" >&2
  exit 1
fi

expected_seasons=(
  "2013-14" "2014-15" "2015-16" "2016-17" "2017-18" "2018-19"
  "2019-20" "2020-21" "2021-22" "2022-23" "2023-24" "2024-25"
)

required_header="season,award,team_level,player"
header_line="$(head -n 1 "$csv_path")"
if [[ "$header_line" != "$required_header" ]]; then
  echo "ERROR: Unexpected header in $csv_path" >&2
  echo "Expected: $required_header" >&2
  echo "Found:    $header_line" >&2
  exit 1
fi

errors=0

# Guard against accidental duplicate inserts for the same logical row.
duplicate_rows="$(awk -F, 'NR>1 {k=$1 FS $2 FS $3 FS $4; c[k]++} END {for (k in c) if (c[k] > 1) print c[k] FS k}' "$csv_path" | sort -t, -k2,2 -k3,3 -k4,4 -k5,5 || true)"
if [[ -n "$duplicate_rows" ]]; then
  echo "ERROR: Duplicate rows found (count,season,award,team_level,player):" >&2
  echo "$duplicate_rows" >&2
  errors=1
fi

for season in "${expected_seasons[@]}"; do
  all_nba_count="$(awk -F, -v s="$season" 'NR>1 && $1==s && $2=="ALL_NBA" {c++} END{print c+0}' "$csv_path")"
  mvp_count="$(awk -F, -v s="$season" 'NR>1 && $1==s && $2=="MVP" {c++} END{print c+0}' "$csv_path")"
  dpoy_count="$(awk -F, -v s="$season" 'NR>1 && $1==s && $2=="DPOY" {c++} END{print c+0}' "$csv_path")"

  if [[ "$all_nba_count" -ne 15 ]]; then
    echo "ERROR: $season has ALL_NBA=$all_nba_count (expected 15)" >&2
    errors=1
  fi
  if [[ "$mvp_count" -ne 1 ]]; then
    echo "ERROR: $season has MVP=$mvp_count (expected 1)" >&2
    errors=1
  fi
  if [[ "$dpoy_count" -ne 1 ]]; then
    echo "ERROR: $season has DPOY=$dpoy_count (expected 1)" >&2
    errors=1
  fi

  for team_level in 1 2 3; do
    team_count="$(awk -F, -v s="$season" -v t="$team_level" 'NR>1 && $1==s && $2=="ALL_NBA" && $3==t {c++} END{print c+0}' "$csv_path")"
    if [[ "$team_count" -ne 5 ]]; then
      echo "ERROR: $season ALL_NBA team_level=$team_level has $team_count rows (expected 5)" >&2
      errors=1
    fi
  done
done

if [[ "$errors" -ne 0 ]]; then
  echo "Validation failed." >&2
  exit 1
fi

total_rows="$(awk 'END{print NR-1}' "$csv_path")"
if [[ "$total_rows" -ne 204 ]]; then
  echo "ERROR: Total data rows are $total_rows (expected 204)" >&2
  exit 1
fi

echo "Validation passed: $csv_path is complete for 2013-14 through 2024-25."
