# ==============================================================================
# 03_supermax_reset_model.R
#
# Purpose : The headline analysis. Tests whether players who re-sign with
#           their incumbent team on a supermax (Designated Veteran extension)
#           show systematically worse post-signing performance trajectories
#           than peers who re-signed on standard deals or who changed teams as
#           UFAs - after controlling for age and pre-signing trajectory.
#
# Hypothesis (preregistered in docs/research-question.md):
#   The `re_signed_supermax` cohort has the most negative mean MIS, and the
#   supermax coefficient in the main regression is negative and statistically
#   significant relative to the `re_signed_standard` baseline.
#
# Treatment categories (set by the classifier):
#   re_signed_standard  - baseline (intercept in the regression)
#   re_signed_supermax  - the cohort of interest
#   new_team            - moved as a UFA
#
# Main specification :
#   mis_overall ~ treatment_category
#                 + age_at_signing
#                 + pre_impact_overall_mw
#                 + factor(contract_start_season)
#
# Robustness checks :
#   (a) repeat for mis_offense and mis_defense - does decline show up
#       differently on the two sides of the floor?
#   (b) drop age control - sanity that age isn't doing all the work
#   (c) drop pre-impact control - sanity that pre-trajectory isn't doing it
#   (d) restrict to events with contract_years >= 3 - supermax is always
#       long, so short-contract events may be a different population
#
# Outputs :
#   data/processed/supermax_reset_main.csv    - main spec coefficient table
#   data/processed/supermax_reset_robust.csv  - robustness check coefficients
#   data/processed/supermax_reset_groupmeans.csv - mean MIS by group, for site
#
# Notes :
#   - Restricts to mis_data_quality == "complete" throughout.
#   - Uses fixest::feols for clean fixed-effects output with robust SEs by
#     default. If fixest is unavailable, falls back to lm with sandwich SEs.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(broom)
})

# Try fixest; fall back to lm if absent. Either path produces the same column
# shape downstream.
USE_FIXEST <- requireNamespace("fixest", quietly = TRUE)
if (USE_FIXEST) suppressPackageStartupMessages(library(fixest))
if (!USE_FIXEST) suppressPackageStartupMessages(library(sandwich)) else NULL
if (!USE_FIXEST) suppressPackageStartupMessages(library(lmtest))   else NULL

paths <- list(
  events       = "data/processed/signing_events_classified.csv",
  mis          = "data/processed/signing_events_mis.csv",
  out_main     = "data/processed/supermax_reset_main.csv",
  out_robust   = "data/processed/supermax_reset_robust.csv",
  out_groups   = "data/processed/supermax_reset_groupmeans.csv"
)

# ------------------------------------------------------------------------------
# Load & prep
# ------------------------------------------------------------------------------

load_complete_events <- function(paths) {
  events <- read_csv(paths$events, show_col_types = FALSE)
  mis    <- read_csv(paths$mis,    show_col_types = FALSE)

  joined <- events %>%
    left_join(mis, by = "event_id", suffix = c("", ".mis")) %>%
    filter(mis_data_quality == "complete",
           !is.na(mis_overall),
           !is.na(treatment_category),
           !is.na(age_at_signing),
           !is.na(pre_impact_overall_mw))

  # Make re_signed_standard the regression baseline. This is the natural
  # reference: "stayed but did NOT take a supermax", against which both
  # "stayed on a supermax" and "moved" are compared.
  joined %>%
    mutate(treatment_category = factor(
      treatment_category,
      levels = c("re_signed_standard", "re_signed_supermax", "new_team")
    ))
}

# ------------------------------------------------------------------------------
# Regression engine - fixest preferred, lm fallback
# ------------------------------------------------------------------------------

fit_model <- function(formula, data, fe_var = "contract_start_season") {
  if (USE_FIXEST) {
    # fixest syntax: pull fixed-effects out of the RHS into the | clause.
    rhs <- as.character(formula)[3]
    rhs_clean <- gsub(paste0("\\+\\s*factor\\(", fe_var, "\\)"), "", rhs)
    f2 <- as.formula(paste(as.character(formula)[2], "~", rhs_clean,
                           "|", fe_var))
    fixest::feols(f2, data = data, vcov = "hetero")
  } else {
    lm(formula, data = data)
  }
}

tidy_model <- function(model, label) {
  if (USE_FIXEST) {
    ts <- broom::tidy(model, conf.int = TRUE)
    n  <- nobs(model)
    r2 <- fixest::r2(model, "r2")
  } else {
    ct <- lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC1"))
    ts <- broom::tidy(ct, conf.int = TRUE)
    n  <- nobs(model)
    r2 <- summary(model)$r.squared
  }
  ts %>%
    mutate(model = label, n_obs = n, r_squared = r2) %>%
    select(model, term, estimate, std.error, statistic, p.value,
           conf.low, conf.high, n_obs, r_squared)
}

# ------------------------------------------------------------------------------
# Group means - feeds the findings page directly
# ------------------------------------------------------------------------------

group_means_table <- function(df) {
  df %>%
    group_by(treatment_category) %>%
    summarise(
      n                  = n(),
      mean_mis_overall   = mean(mis_overall,  na.rm = TRUE),
      median_mis_overall = median(mis_overall, na.rm = TRUE),
      sd_mis_overall     = sd(mis_overall,    na.rm = TRUE),
      se_mis_overall     = sd(mis_overall,    na.rm = TRUE) / sqrt(n()),
      mean_mis_offense   = mean(mis_offense,  na.rm = TRUE),
      mean_mis_defense   = mean(mis_defense,  na.rm = TRUE),
      mean_age           = mean(age_at_signing, na.rm = TRUE),
      mean_pre_impact       = mean(pre_impact_overall_mw, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(treatment_category)
}

# ------------------------------------------------------------------------------
# Main specification + robustness suite
# ------------------------------------------------------------------------------

run_main_spec <- function(df) {
  f <- mis_overall ~ treatment_category + age_at_signing +
       pre_impact_overall_mw + factor(contract_start_season)
  tidy_model(fit_model(f, df), "main")
}

run_robustness <- function(df) {
  specs <- list(
    list(label = "offense_only",
         formula = mis_offense ~ treatment_category + age_at_signing +
            pre_impact_offense_mw + factor(contract_start_season),
         data = df),
    list(label = "defense_only",
         formula = mis_defense ~ treatment_category + age_at_signing +
            pre_impact_defense_mw + factor(contract_start_season),
         data = df),
    list(label = "no_age_control",
         formula = mis_overall ~ treatment_category +
            pre_impact_overall_mw + factor(contract_start_season),
         data = df),
    list(label = "no_pre_control",
         formula = mis_overall ~ treatment_category + age_at_signing +
                   factor(contract_start_season),
         data = df),
    list(label = "long_contracts_only",
         formula = mis_overall ~ treatment_category + age_at_signing +
              pre_impact_overall_mw + factor(contract_start_season),
         data = df %>% filter(contract_years >= 3))
  )
  map_dfr(specs, function(s) {
    fit <- fit_model(s$formula, s$data)
    tidy_model(fit, s$label)
  })
}

# ------------------------------------------------------------------------------
# Console report
# ------------------------------------------------------------------------------

print_report <- function(groups, main_tab, robust_tab) {
  message("\n--- SUPERMAX RESET MODEL ---")
  message(sprintf("Backend: %s\n", if (USE_FIXEST) "fixest::feols" else "lm + HC1 SE"))

  message("Group means (complete-data only):")
  message(sprintf("  %-22s %-6s %-12s %-12s %s",
                  "group", "n", "mean_MIS", "median_MIS", "SE"))
  pwalk(groups, function(treatment_category, n,
                         mean_mis_overall, median_mis_overall,
                         sd_mis_overall, se_mis_overall,
                         mean_mis_offense, mean_mis_defense,
                         mean_age, mean_pre_impact) {
    message(sprintf("  %-22s %-6d %+11.3f %+12.3f %.3f",
                    treatment_category, n,
                    mean_mis_overall, median_mis_overall, se_mis_overall))
  })

  message("\nMain specification - coefficients of interest:")
  message("  (re_signed_standard is the baseline; re_signed_supermax")
  message("   and new_team are interpreted as deltas from the baseline.)")
  main_tab %>%
    filter(grepl("treatment_category", term)) %>%
    pwalk(function(model, term, estimate, std.error, statistic, p.value,
                   conf.low, conf.high, n_obs, r_squared) {
      sig <- ifelse(p.value < 0.01, "***",
             ifelse(p.value < 0.05, "**",
             ifelse(p.value < 0.10, "*", "")))
      level <- gsub("treatment_category", "", term)
      message(sprintf("  %-22s %+.3f (SE %.3f) p=%.3g %s   95%% CI [%+.3f, %+.3f]",
                      level, estimate, std.error, p.value, sig,
                      conf.low, conf.high))
    })

  # Headline test
  sm_row <- main_tab %>%
    filter(term == "treatment_categoryre_signed_supermax")
  if (nrow(sm_row) > 0) {
    e <- sm_row$estimate; p <- sm_row$p.value
    message("\nHeadline test (supermax vs standard re-sign):")
    if (e < 0 && p < 0.05) {
      message(sprintf("  Supermax coefficient is %+.3f, p=%.3g - NEGATIVE ",
                      e, p),
              "and significant at p<0.05.")
      message("  The data SUPPORT the supermax-reset hypothesis at the ",
              "conventional threshold.")
    } else if (e < 0) {
      message(sprintf("  Supermax coefficient is %+.3f, p=%.3g - negative ",
                      e, p),
              "but NOT significant at p<0.05.")
      message("  Direction consistent with the hypothesis; effect size ",
              "may be small or sample limited.")
    } else {
      message(sprintf("  Supermax coefficient is %+.3f, p=%.3g - NOT ",
                      e, p),
              "negative.")
      message("  The data do NOT support the supermax-reset hypothesis.")
    }
  }

  message("\nRobustness - supermax coefficient across specs:")
  robust_tab %>%
    filter(term == "treatment_categoryre_signed_supermax") %>%
    pwalk(function(model, term, estimate, std.error, statistic, p.value,
                   conf.low, conf.high, n_obs, r_squared) {
      sig <- ifelse(p.value < 0.01, "***",
             ifelse(p.value < 0.05, "**",
             ifelse(p.value < 0.10, "*", "")))
      message(sprintf("  %-22s %+.3f (SE %.3f) p=%.3g %s  n=%d",
                      model, estimate, std.error, p.value, sig, n_obs))
    })

  message("--- end ---\n")
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main <- function(paths) {
  df <- load_complete_events(paths)

  if (nrow(df) < 30) {
    warning("Only ", nrow(df), " complete-data events. Regression results ",
            "will be unstable; treat coefficients as exploratory only.")
  }

  groups     <- group_means_table(df)
  main_tab   <- run_main_spec(df)
  robust_tab <- run_robustness(df)

  dir.create(dirname(paths$out_main), showWarnings = FALSE, recursive = TRUE)
  write_csv(main_tab,   paths$out_main)
  write_csv(robust_tab, paths$out_robust)
  write_csv(groups,     paths$out_groups)
  message("wrote: ", basename(paths$out_main), ", ",
          basename(paths$out_robust), ", ", basename(paths$out_groups))

  print_report(groups, main_tab, robust_tab)
  invisible(list(groups = groups, main = main_tab, robust = robust_tab))
}

if (sys.nframe() == 0) {
  main(paths)
}