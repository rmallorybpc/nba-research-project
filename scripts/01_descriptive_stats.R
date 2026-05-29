# ==============================================================================
# 01_descriptive_stats.R
#
# Purpose : Descriptive statistics on the classified signing-event population.
#           First-look numbers: counts by treatment category, by contract type,
#           by season, by team; mean MIS by group; distribution shape. Produces
#           CSVs that feed the site's overview and findings pages, plus a
#           console report for the analyst.
#
# Inputs  : data/processed/signing_events_classified.csv
#           data/processed/signing_events_mis.csv
#
# Outputs : data/processed/desc_population_counts.csv
#           data/processed/desc_mis_by_group.csv
#           data/processed/desc_mis_by_season.csv
#
# Depends : dplyr, tidyr, readr, stringr, purrr
#
# Notes   : All MIS-based summaries restrict to mis_data_quality == "complete"
#           so partial-coverage events do not pollute the means. Counts (which
#           are not MIS-based) include the full population.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
})

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

paths <- list(
  events       = "data/processed/signing_events_classified.csv",
  mis          = "data/processed/signing_events_mis.csv",
  out_counts   = "data/processed/desc_population_counts.csv",
  out_groups   = "data/processed/desc_mis_by_group.csv",
  out_seasons  = "data/processed/desc_mis_by_season.csv"
)

# ------------------------------------------------------------------------------
# Load
# ------------------------------------------------------------------------------

load_inputs <- function(paths) {
  walk(paths[c("events", "mis")], function(p) {
    if (!file.exists(p)) stop("Required input not found: ", p, call. = FALSE)
  })
  events <- read_csv(paths$events, show_col_types = FALSE)
  mis    <- read_csv(paths$mis,    show_col_types = FALSE)

  # Join MIS into events on event_id. left_join keeps all events even when MIS
  # is missing (e.g. classified but pre-signing impact data not yet ingested), which is
  # correct for population counts.
  events %>% left_join(mis, by = "event_id", suffix = c("", ".mis"))
}

# ------------------------------------------------------------------------------
# Tables
# ------------------------------------------------------------------------------

# Population counts — covers the full classified event set, not just complete-
# data events. This is the "how many signings of what kind happened" table.
table_population_counts <- function(df) {

  by_treatment <- df %>%
    count(treatment_category, name = "n_events") %>%
    mutate(grouping = "treatment_category") %>%
    rename(group_value = treatment_category)

  by_contract_type <- df %>%
    count(contract_type, name = "n_events") %>%
    mutate(grouping = "contract_type") %>%
    rename(group_value = contract_type)

  by_yos_band <- df %>%
    count(yos_band, name = "n_events") %>%
    mutate(grouping = "yos_band") %>%
    rename(group_value = yos_band)

  by_season <- df %>%
    count(contract_start_season, name = "n_events") %>%
    mutate(grouping = "contract_start_season") %>%
    rename(group_value = contract_start_season)

  bind_rows(by_treatment, by_contract_type, by_yos_band, by_season) %>%
    mutate(group_value = as.character(group_value)) %>%
    select(grouping, group_value, n_events)
}

# Mean MIS by treatment category. Complete-data only. This is the table that
# directly feeds the findings page's headline numbers.
table_mis_by_group <- function(df) {
  df %>%
    filter(mis_data_quality == "complete") %>%
    group_by(treatment_category) %>%
    summarise(
      n_events                  = n(),
      mean_mis_overall          = mean(mis_overall, na.rm = TRUE),
      median_mis_overall        = median(mis_overall, na.rm = TRUE),
      sd_mis_overall            = sd(mis_overall, na.rm = TRUE),
      mean_mis_offense          = mean(mis_offense, na.rm = TRUE),
      mean_mis_defense          = mean(mis_defense, na.rm = TRUE),
      mean_pre_impact_overall      = mean(pre_impact_overall_mw, na.rm = TRUE),
      mean_post_impact_overall     = mean(post_impact_overall_mw, na.rm = TRUE),
      pct_with_negative_mis     = mean(mis_overall < 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(treatment_category)
}

# Mean MIS by season AND treatment — for the explorer page's season-over-season
# views. Surfaces whether the cohort effects are stable across seasons or
# driven by one or two outlier offseasons.
table_mis_by_season <- function(df) {
  df %>%
    filter(mis_data_quality == "complete") %>%
    group_by(contract_start_season, treatment_category) %>%
    summarise(
      n_events         = n(),
      mean_mis_overall = mean(mis_overall, na.rm = TRUE),
      mean_mis_offense = mean(mis_offense, na.rm = TRUE),
      mean_mis_defense = mean(mis_defense, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(contract_start_season, treatment_category)
}

# ------------------------------------------------------------------------------
# Console report
# ------------------------------------------------------------------------------

print_report <- function(counts, groups, by_season) {
  message("\n--- DESCRIPTIVE STATS ---")
  message("\nPopulation by treatment:")
  counts %>% filter(grouping == "treatment_category") %>%
    pwalk(function(grouping, group_value, n_events) {
      message(sprintf("  %-22s %d", group_value, n_events))
    })

  message("\nPopulation by contract type:")
  counts %>% filter(grouping == "contract_type") %>%
    arrange(desc(n_events)) %>%
    pwalk(function(grouping, group_value, n_events) {
      message(sprintf("  %-22s %d", group_value, n_events))
    })

  message("\nMean MIS by treatment (complete-data events only):")
  message(sprintf("  %-22s %-6s %-10s %-10s %-10s %s",
                  "group", "n", "mean", "median", "sd", "pct_neg"))
  pwalk(groups, function(treatment_category, n_events,
                         mean_mis_overall, median_mis_overall, sd_mis_overall,
                         mean_mis_offense, mean_mis_defense,
                         mean_pre_impact_overall, mean_post_impact_overall,
                         pct_with_negative_mis) {
    message(sprintf("  %-22s %-6d %+9.3f %+10.3f %10.3f %.1f%%",
                    treatment_category, n_events,
                    mean_mis_overall, median_mis_overall, sd_mis_overall,
                    100 * pct_with_negative_mis))
  })

  # Headline check: is re_signed_supermax the most negative?
  if ("re_signed_supermax" %in% groups$treatment_category) {
    sm <- groups %>% filter(treatment_category == "re_signed_supermax") %>%
      pull(mean_mis_overall)
    others <- groups %>% filter(treatment_category != "re_signed_supermax")
    if (all(sm < others$mean_mis_overall, na.rm = TRUE)) {
      message("\n  Headline check: supermax cohort has the MOST NEGATIVE ",
              "mean MIS — consistent with the supermax-reset hypothesis.")
    } else {
      message("\n  Headline check: supermax cohort is NOT the most negative — ",
              "the simple-means picture does not support the hypothesis.")
    }
  }
  message("--- end ---\n")
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main <- function(paths) {
  df <- load_inputs(paths)

  counts    <- table_population_counts(df)
  groups    <- table_mis_by_group(df)
  by_season <- table_mis_by_season(df)

  dir.create(dirname(paths$out_counts), showWarnings = FALSE, recursive = TRUE)
  write_csv(counts,    paths$out_counts)
  write_csv(groups,    paths$out_groups)
  write_csv(by_season, paths$out_seasons)
  message("wrote descriptive tables: ",
          basename(paths$out_counts),  ", ",
          basename(paths$out_groups),  ", ",
          basename(paths$out_seasons))

  print_report(counts, groups, by_season)
  invisible(list(counts = counts, groups = groups, by_season = by_season))
}

if (sys.nframe() == 0) {
  main(paths)
}
