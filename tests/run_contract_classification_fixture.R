#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[grep(file_arg, args)][1])
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

source("scripts/02_classify_contract_types.R")

fixture_base <- "tests/fixtures/contract_classification"
out_csv <- tempfile(pattern = "signing_events_classified_", fileext = ".csv")

# Backward-compat for older fixture input snapshots that predate
# signing_offseason_year being required by the classifier.
events_path <- file.path(fixture_base, "signing_events.csv")
events_df <- readr::read_csv(events_path, show_col_types = FALSE)
if (!"signing_offseason_year" %in% names(events_df)) {
  events_df <- events_df |>
    dplyr::mutate(
      signing_offseason_year = as.integer(substr(contract_start_season, 1, 4))
    )
  events_path <- tempfile(pattern = "signing_events_with_year_", fileext = ".csv")
  readr::write_csv(events_df, events_path)
}

fixture_paths <- list(
  events = events_path,
  thresholds = file.path(fixture_base, "cba_thresholds.csv"),
  awards = file.path(fixture_base, "nba_awards.csv"),
  min_scale = file.path(fixture_base, "nba_minimum_scale.csv"),
  out = out_csv
)

actual <- main(fixture_paths)
expected <- readr::read_csv(
  file.path(fixture_base, "expected_signing_events_classified.csv"),
  show_col_types = FALSE
)

actual <- dplyr::arrange(actual, event_id)
expected <- dplyr::arrange(expected, event_id)

compare_cols <- c(
  "event_id", "player_name", "season", "contract_start_season",
  "signing_team", "prior_team", "incumbent", "years_of_service", "yos_band",
  "average_annual_value", "cap_percentage_at_signing", "using_aav_proxy",
  "minimum_salary", "supermax_eligible", "eligibility_uncertain",
  "contract_type", "treatment_category", "classification_flag"
)

actual_cmp <- actual[, compare_cols]
expected_cmp <- expected[, compare_cols]

if (!isTRUE(all.equal(actual_cmp, expected_cmp, check.attributes = FALSE))) {
  mismatch <- dplyr::bind_rows(
    dplyr::mutate(expected_cmp, .source = "expected"),
    dplyr::mutate(actual_cmp, .source = "actual")
  )
  print(mismatch)
  stop("Fixture comparison failed", call. = FALSE)
}

cat("Fixture test passed: classifier output matches expected snapshot.\n")
