# ==============================================================================
# 02_mean_reversion_model.R
#
# Purpose : Test whether high pre-signing performance predicts post-signing
#           decline (and vice versa) - the mean-reversion baseline carried
#           over from the NHL study. This is the project's SECONDARY finding,
#           not the headline. The supermax-reset result (script 03) is the
#           headline. Mean reversion is a stable, well-replicated pattern in
#           sports analytics; confirming it here is a sanity check that the
#           pipeline produces plausible results before we trust the
#           supermax-specific finding.
#
# Hypothesis : Players signing on top of an exceptional pre-window (top
#              quartile of pre_epm_overall) show systematically larger
#              negative MIS than players signing on top of an average pre-
#              window. Direction: top quartile -> most negative MIS, bottom
#              quartile -> least negative (or positive) MIS.
#
# Specification : Pre-signing performance quartile assigned within each
#                 contract_start_season (so era effects don't drive the
#                 quartile cuts). Mean MIS reported per quartile, with a
#                 simple regression test of pre_epm -> MIS slope.
#
# Inputs  : data/processed/signing_events_classified.csv
#           data/processed/signing_events_mis.csv
#
# Outputs : data/processed/reversion_by_quartile.csv
#           data/processed/reversion_regression.csv
#
# Depends : dplyr, tidyr, readr, purrr, broom
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(broom)
})

paths <- list(
  events       = "data/processed/signing_events_classified.csv",
  mis          = "data/processed/signing_events_mis.csv",
  out_quartile = "data/processed/reversion_by_quartile.csv",
  out_reg      = "data/processed/reversion_regression.csv"
)

# ------------------------------------------------------------------------------
# Load
# ------------------------------------------------------------------------------

load_complete_events <- function(paths) {
  events <- read_csv(paths$events, show_col_types = FALSE)
  mis    <- read_csv(paths$mis,    show_col_types = FALSE)
  events %>%
    left_join(mis, by = "event_id", suffix = c("", ".mis")) %>%
    filter(mis_data_quality == "complete",
           !is.na(pre_epm_overall_mw),
           !is.na(mis_overall))
}

# ------------------------------------------------------------------------------
# Quartile analysis
# Assign quartiles within each season so the cuts reflect that season's
# population rather than an absolute EPM threshold. Then mean MIS per quartile.
# ------------------------------------------------------------------------------

within_season_quartile <- function(df) {
  df %>%
    group_by(contract_start_season) %>%
    mutate(
      pre_epm_quartile = ntile(pre_epm_overall_mw, 4)
    ) %>%
    ungroup() %>%
    filter(!is.na(pre_epm_quartile)) %>%
    mutate(pre_epm_quartile_label = paste0("Q", pre_epm_quartile))
}

quartile_summary <- function(df) {
  df %>%
    group_by(pre_epm_quartile, pre_epm_quartile_label) %>%
    summarise(
      n                       = n(),
      mean_pre_epm_overall    = mean(pre_epm_overall_mw, na.rm = TRUE),
      mean_post_epm_overall   = mean(post_epm_overall_mw, na.rm = TRUE),
      mean_mis_overall        = mean(mis_overall,        na.rm = TRUE),
      median_mis_overall      = median(mis_overall,      na.rm = TRUE),
      pct_negative            = mean(mis_overall < 0,    na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(pre_epm_quartile)
}

# ------------------------------------------------------------------------------
# Regression
# Simple OLS of MIS on pre-EPM, with season fixed effects to absorb era
# effects. If mean reversion is present, the slope of pre_epm on mis_overall
# should be NEGATIVE: higher pre-signing performance predicts a larger drop.
# ------------------------------------------------------------------------------

regression_summary <- function(df) {
  fit <- lm(mis_overall ~ pre_epm_overall_mw + factor(contract_start_season),
            data = df)
  ts <- tidy(fit, conf.int = TRUE)
  ts %>%
    filter(term == "pre_epm_overall_mw") %>%
    mutate(model = "mis_overall ~ pre_epm + season_FE",
           n_obs = nobs(fit),
           r_squared = summary(fit)$r.squared) %>%
    select(model, term, estimate, std.error, statistic, p.value,
           conf.low, conf.high, n_obs, r_squared)
}

# ------------------------------------------------------------------------------
# Report
# ------------------------------------------------------------------------------

print_report <- function(qsum, rsum) {
  message("\n--- MEAN REVERSION ---")
  message("\nMean MIS by pre-signing EPM quartile:")
  message(sprintf("  %-4s %-6s %-12s %-12s %-12s %s",
                  "Q", "n", "pre_EPM", "post_EPM", "MIS", "pct_neg"))
  pwalk(qsum, function(pre_epm_quartile, pre_epm_quartile_label, n,
                       mean_pre_epm_overall, mean_post_epm_overall,
                       mean_mis_overall, median_mis_overall, pct_negative) {
    message(sprintf("  %-4s %-6d %+11.3f %+12.3f %+12.3f %.1f%%",
                    pre_epm_quartile_label, n,
                    mean_pre_epm_overall, mean_post_epm_overall,
                    mean_mis_overall, 100 * pct_negative))
  })

  message("\nRegression: mis_overall ~ pre_epm_overall_mw + season_FE")
  pwalk(rsum, function(model, term, estimate, std.error, statistic, p.value,
                       conf.low, conf.high, n_obs, r_squared) {
    message(sprintf("  slope on pre_epm: %+.3f (SE %.3f, p=%.3g, n=%d)",
                    estimate, std.error, p.value, n_obs))
    message(sprintf("  95%% CI: [%+.3f, %+.3f]", conf.low, conf.high))
    if (estimate < 0 && p.value < 0.05) {
      message("  -> negative and significant: mean reversion present.")
    } else if (estimate < 0) {
      message("  -> negative but not significant at p<0.05.")
    } else {
      message("  -> NOT negative: simple mean-reversion picture not supported.")
    }
  })
  message("--- end ---\n")
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main <- function(paths) {
  df  <- load_complete_events(paths)
  dq  <- within_season_quartile(df)
  qs  <- quartile_summary(dq)
  rs  <- regression_summary(dq)

  dir.create(dirname(paths$out_quartile), showWarnings = FALSE, recursive = TRUE)
  write_csv(qs, paths$out_quartile)
  write_csv(rs, paths$out_reg)
  message("wrote: ", basename(paths$out_quartile), ", ",
          basename(paths$out_reg))

  print_report(qs, rs)
  invisible(list(quartile = qs, regression = rs))
}

if (sys.nframe() == 0) {
  main(paths)
}
