# ==============================================================================
# 04_team_portfolio_model.R
#
# Purpose : Team-level rollup of signing MIS. For each of the 30 NBA teams,
#           summarises how their UFA signings and supermax extensions
#           collectively performed over the study window. Feeds the site's
#           team-detail page directly.
#
#           The team-portfolio framing answers a different question from the
#           supermax-reset model: not "are supermax deals worse on average"
#           (script 03), but "which teams systematically got value from their
#           signing decisions, and which didn't". Both are interesting; the
#           portfolio view is more legible to a casual reader.
#
# Inputs  : data/processed/signing_events_classified.csv
#           data/processed/signing_events_mis.csv
#
# Outputs : data/processed/team_portfolio.csv
#           data/processed/team_signings_detail.csv
#
# Notes   : Restricts headline rankings to mis_data_quality == "complete" but
#           reports counts for the full population so a team with all-incomplete
#           data is visibly NA-ranked rather than silently absent.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
})

paths <- list(
  events       = "data/processed/signing_events_classified.csv",
  mis          = "data/processed/signing_events_mis.csv",
  out_team     = "data/processed/team_portfolio.csv",
  out_detail   = "data/processed/team_signings_detail.csv"
)

# ------------------------------------------------------------------------------
# Load
# ------------------------------------------------------------------------------

load_inputs <- function(paths) {
  events <- read_csv(paths$events, show_col_types = FALSE)
  mis    <- read_csv(paths$mis,    show_col_types = FALSE)
  events %>% left_join(mis, by = "event_id", suffix = c("", ".mis"))
}

# ------------------------------------------------------------------------------
# Team-level aggregate
# ------------------------------------------------------------------------------

team_portfolio <- function(df) {

  # Counts include all events; MIS aggregates filter to complete-data.
  counts <- df %>%
    group_by(signing_team) %>%
    summarise(
      n_signings_total        = n(),
      n_supermax              = sum(treatment_category == "re_signed_supermax",
                                     na.rm = TRUE),
      n_re_signed_standard    = sum(treatment_category == "re_signed_standard",
                                     na.rm = TRUE),
      n_new_team              = sum(treatment_category == "new_team",
                                     na.rm = TRUE),
      total_value_signed      = sum(total_value, na.rm = TRUE),
      .groups = "drop"
    )

  mis_agg <- df %>%
    filter(mis_data_quality == "complete") %>%
    group_by(signing_team) %>%
    summarise(
      n_complete           = n(),
      mean_mis_overall     = mean(mis_overall, na.rm = TRUE),
      sum_mis_overall      = sum(mis_overall,  na.rm = TRUE),
      mean_mis_offense     = mean(mis_offense, na.rm = TRUE),
      mean_mis_defense     = mean(mis_defense, na.rm = TRUE),
      pct_positive_mis     = mean(mis_overall > 0, na.rm = TRUE),
      best_mis             = max(mis_overall, na.rm = TRUE),
      worst_mis            = min(mis_overall, na.rm = TRUE),
      .groups = "drop"
    )

  counts %>%
    left_join(mis_agg, by = "signing_team") %>%
    arrange(desc(mean_mis_overall))
}

# ------------------------------------------------------------------------------
# Per-event detail for team-page drill-down
# Keeps the columns the site's team-detail page needs in one flat file,
# pre-joined and sorted.
# ------------------------------------------------------------------------------

team_detail <- function(df) {
  df %>%
    transmute(
      event_id,
      signing_team,
      season = contract_start_season,
      player_name,
      treatment_category,
      contract_type,
      contract_years,
      total_value,
      mis_overall,
      mis_data_quality
    ) %>%
    arrange(signing_team, season, desc(mis_overall))
}

# ------------------------------------------------------------------------------
# Report
# ------------------------------------------------------------------------------

print_report <- function(portfolio) {

  ranked <- portfolio %>%
    filter(!is.na(mean_mis_overall)) %>%
    arrange(desc(mean_mis_overall))

  if (nrow(ranked) == 0) {
    message("\n--- TEAM PORTFOLIO ---")
    message("No complete-data team rows yet. Pipeline needs an end-to-end ",
            "run against real data before this report has content.")
    message("--- end ---\n")
    return(invisible(NULL))
  }

  message("\n--- TEAM PORTFOLIO ---")
  message(sprintf("Teams with complete-data signings: %d", nrow(ranked)))

  message("\nTop 5 teams by mean MIS (best signing decisions):")
  message(sprintf("  %-5s %-6s %-10s %-10s %s",
                  "team", "n", "mean_MIS", "sum_MIS", "pct_positive"))
  ranked %>% head(5) %>%
    pwalk(function(signing_team, n_signings_total, n_supermax,
                   n_re_signed_standard, n_new_team, total_value_signed,
                   n_complete, mean_mis_overall, sum_mis_overall,
                   mean_mis_offense, mean_mis_defense, pct_positive_mis,
                   best_mis, worst_mis) {
      message(sprintf("  %-5s %-6d %+9.3f %+10.3f %.1f%%",
                      signing_team, n_complete,
                      mean_mis_overall, sum_mis_overall,
                      100 * pct_positive_mis))
    })

  message("\nBottom 5 teams by mean MIS (worst signing decisions):")
  message(sprintf("  %-5s %-6s %-10s %-10s %s",
                  "team", "n", "mean_MIS", "sum_MIS", "pct_positive"))
  ranked %>% tail(5) %>% arrange(mean_mis_overall) %>%
    pwalk(function(signing_team, n_signings_total, n_supermax,
                   n_re_signed_standard, n_new_team, total_value_signed,
                   n_complete, mean_mis_overall, sum_mis_overall,
                   mean_mis_offense, mean_mis_defense, pct_positive_mis,
                   best_mis, worst_mis) {
      message(sprintf("  %-5s %-6d %+9.3f %+10.3f %.1f%%",
                      signing_team, n_complete,
                      mean_mis_overall, sum_mis_overall,
                      100 * pct_positive_mis))
    })

  # Teams with the most supermax exposure
  sm_heavy <- ranked %>% filter(n_supermax > 0) %>%
    arrange(desc(n_supermax), mean_mis_overall)
  if (nrow(sm_heavy) > 0) {
    message("\nTeams with supermax exposure:")
    message(sprintf("  %-5s %-10s %s", "team", "n_supermax", "mean_MIS"))
    sm_heavy %>%
      pwalk(function(signing_team, n_signings_total, n_supermax,
                     n_re_signed_standard, n_new_team, total_value_signed,
                     n_complete, mean_mis_overall, sum_mis_overall,
                     mean_mis_offense, mean_mis_defense, pct_positive_mis,
                     best_mis, worst_mis) {
        message(sprintf("  %-5s %-10d %+.3f",
                        signing_team, n_supermax, mean_mis_overall))
      })
  }
  message("--- end ---\n")
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main <- function(paths) {
  df         <- load_inputs(paths)
  portfolio  <- team_portfolio(df)
  detail     <- team_detail(df)

  dir.create(dirname(paths$out_team), showWarnings = FALSE, recursive = TRUE)
  write_csv(portfolio, paths$out_team)
  write_csv(detail,    paths$out_detail)
  message("wrote: ", basename(paths$out_team), ", ", basename(paths$out_detail))

  print_report(portfolio)
  invisible(list(portfolio = portfolio, detail = detail))
}

if (sys.nframe() == 0) {
  main(paths)
}