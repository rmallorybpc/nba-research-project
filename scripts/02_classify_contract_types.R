# ==============================================================================
# 02_classify_contract_types.R
#
# Purpose : Classify each NBA signing event by contract type and assign a
#           treatment category for the supermax-reset analysis. The headline
#           job is distinguishing genuine supermax (Designated Veteran) deals
#           from standard max deals, which requires verifying the player's
#           award history against the qualifying-window rules — dollar value
#           alone is not sufficient.
#
# Inputs  : data/processed/signing_events.csv   (one row per player-team-contract)
#           data/processed/cba_thresholds.csv   (salary cap + max tiers by season)
#           data/processed/nba_awards.csv       (MVP / DPOY / All-NBA by season)
#
# Output  : data/processed/signing_events_classified.csv
#
# Depends : dplyr, tidyr, readr, stringr, stringi, purrr
#
# Notes   : Two inputs are not yet complete and the script is written to fail
#           loudly or flag rather than guess:
#             (1) signing_events must carry `years_of_service`. Age is NOT a
#                 substitute — the tier (25/30/35) and the 7-9 supermax band are
#                 defined on YOS.
#             (2) MLE / BAE / minimum per-season dollar values are not yet in
#                 cba_thresholds.csv. Until they are, sub-max contracts are
#                 classified as "standard_or_exception" with a flag, not split.
#           Award data has documented gaps (see nba-awards.md). Any signing whose
#           lookback window touches an incomplete award season is flagged
#           `eligibility_uncertain = TRUE` so a data gap never produces a false
#           "not a supermax".
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(purrr)
})

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

paths <- list(
  events     = "data/processed/signing_events.csv",
  thresholds = "data/processed/cba_thresholds.csv",
  awards     = "data/processed/nba_awards.csv",
  out        = "data/processed/signing_events_classified.csv"
)

# Tolerance band on the max-tier dollar comparison, to absorb rounding and the
# small gap between AAV and true first-year salary. A contract within this
# fraction below a tier threshold still counts as that tier.
TIER_TOLERANCE <- 0.02

# Award seasons known to be incomplete in nba_awards.csv (see nba-awards.md).
# A signing whose lookback window includes any of these cannot have its supermax
# eligibility fully trusted from the awards table alone.
INCOMPLETE_AWARD_SEASONS <- c("2013-14", "2014-15", "2015-16", "2019-20", "2020-21")

# ------------------------------------------------------------------------------
# Helpers: season arithmetic
# ------------------------------------------------------------------------------

# "2023-24" -> 2023
season_start_year <- function(season) {
  as.integer(str_sub(season, 1, 4))
}

# Build the "YYYY-YY" label from a start year. 2022 -> "2022-23"; 1999 -> "1999-00".
make_season_label <- function(start_year) {
  sprintf("%d-%02d", start_year, (start_year + 1) %% 100)
}

# For a contract whose FIRST season is `start_season`, return the n most-recent
# COMPLETED seasons, most-recent first. A deal signed for 2023-24 looks back at
# 2022-23, 2021-22, 2020-21.
lookback_seasons <- function(start_season, n = 3) {
  y <- season_start_year(start_season)
  make_season_label((y - 1):(y - n))
}

# ------------------------------------------------------------------------------
# Helpers: name normalization for the awards join
# ------------------------------------------------------------------------------

# The awards table and the signing-event table will not agree on accents or
# suffixes (Jokic vs Jokić, "Jaren Jackson Jr." vs "Jaren Jackson"). Normalize
# both sides before joining: strip accents, drop generational suffixes, collapse
# punctuation and whitespace, lower-case.
normalize_player_name <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%   # Jokić -> Jokic
    str_replace_all("[.'`]", "") %>%         # drop periods/apostrophes
    str_to_lower() %>%
    str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b", "") %>% # strip suffix after lower-casing
    str_squish()
}

# ------------------------------------------------------------------------------
# Load + validate
# ------------------------------------------------------------------------------

load_inputs <- function(paths) {
  walk(paths[c("events", "thresholds", "awards")], function(p) {
    if (!file.exists(p)) stop("Required input not found: ", p, call. = FALSE)
  })

  events     <- read_csv(paths$events, show_col_types = FALSE)
  thresholds <- read_csv(paths$thresholds, show_col_types = FALSE)
  awards     <- read_csv(paths$awards, show_col_types = FALSE)

  # Required fields on the signing-event table. years_of_service is mandatory.
  required_event_cols <- c(
    "event_id", "player_name", "season", "signing_team", "prior_team",
    "contract_start_season", "years_of_service",
    "average_annual_value", "cap_percentage_at_signing"
  )
  missing <- setdiff(required_event_cols, names(events))
  if (length(missing) > 0) {
    stop("signing_events is missing required columns: ",
         paste(missing, collapse = ", "),
         "\n  years_of_service in particular cannot be derived from age.",
         call. = FALSE)
  }

  required_threshold_cols <- c("season", "salary_cap",
                               "max_25pct", "max_30pct", "max_35pct")
  missing_t <- setdiff(required_threshold_cols, names(thresholds))
  if (length(missing_t) > 0) {
    stop("cba_thresholds is missing required columns: ",
         paste(missing_t, collapse = ", "), call. = FALSE)
  }

  # MLE / BAE / minimum columns are optional for now. Warn if absent.
  exception_cols <- c("mle_nontaxpayer", "bae", "veteran_minimum")
  if (!all(exception_cols %in% names(thresholds))) {
    message("NOTE: MLE/BAE/minimum columns absent from thresholds. ",
            "Sub-max contracts will be labelled 'standard_or_exception' ",
            "and not split until those per-season values are added.")
  }

  list(events = events, thresholds = thresholds, awards = awards)
}

# ------------------------------------------------------------------------------
# Supermax eligibility (vectorized)
#
# A player qualifies for a Designated Veteran (supermax) contract if, in the
# qualifying window relative to the signing, ANY of the following hold:
#   - All-NBA (any team level) in the most recent season, OR in two of the three
#     most recent seasons
#   - MVP in any of the three most recent seasons
#   - DPOY in the most recent season, OR in two of the three most recent seasons
# ------------------------------------------------------------------------------

compute_eligibility <- function(events, awards) {

  awards_norm <- awards %>%
    mutate(player_norm = normalize_player_name(player)) %>%
    select(player_norm, season, award)

  # Expand each event to its three lookback seasons, ranked (1 = most recent).
  event_lb <- events %>%
    select(event_id, player_name, contract_start_season) %>%
    mutate(player_norm = normalize_player_name(player_name)) %>%
    mutate(lb = map(contract_start_season, ~ lookback_seasons(.x, 3))) %>%
    unnest_longer(lb) %>%
    group_by(event_id) %>%
    mutate(lb_rank = row_number()) %>%
    ungroup()

  # Flag events whose lookback window touches a known-incomplete award season.
  uncertainty <- event_lb %>%
    group_by(event_id) %>%
    summarise(
      eligibility_uncertain = any(lb %in% INCOMPLETE_AWARD_SEASONS),
      .groups = "drop"
    )

  # Join awards into the lookback rows.
  hits <- event_lb %>%
    left_join(awards_norm, by = c("player_norm", "lb" = "season"))

  # Aggregate per event into the eligibility decision.
  elig <- hits %>%
    group_by(event_id) %>%
    summarise(
      mvp_any       = any(award == "MVP", na.rm = TRUE),
      allnba_recent = any(award == "ALL_NBA" & lb_rank == 1, na.rm = TRUE),
      allnba_n_seas = n_distinct(lb[which(award == "ALL_NBA")]),
      dpoy_recent   = any(award == "DPOY" & lb_rank == 1, na.rm = TRUE),
      dpoy_n_seas   = n_distinct(lb[which(award == "DPOY")]),
      .groups = "drop"
    ) %>%
    mutate(
      allnba_2of3 = allnba_n_seas >= 2,
      dpoy_2of3   = dpoy_n_seas   >= 2,
      supermax_eligible = mvp_any | allnba_recent | allnba_2of3 |
                          dpoy_recent | dpoy_2of3
    ) %>%
    select(event_id, supermax_eligible,
           mvp_any, allnba_recent, allnba_2of3, dpoy_recent, dpoy_2of3)

  events %>%
    left_join(elig, by = "event_id") %>%
    left_join(uncertainty, by = "event_id") %>%
    mutate(
      supermax_eligible     = coalesce(supermax_eligible, FALSE),
      eligibility_uncertain = coalesce(eligibility_uncertain, FALSE)
    )
}

# ------------------------------------------------------------------------------
# Contract-type waterfall + treatment category
# ------------------------------------------------------------------------------

classify <- function(events, thresholds) {

  has_exceptions <- all(c("mle_nontaxpayer", "bae", "veteran_minimum")
                        %in% names(thresholds))

  df <- events %>%
    left_join(thresholds, by = "season") %>%
    mutate(
      # First-year salary is ideal; AAV is the documented fallback. Flag the proxy.
      salary_basis = average_annual_value,
      using_aav_proxy = TRUE,

      incumbent = !is.na(prior_team) & prior_team == signing_team,

      at_35 = salary_basis >= max_35pct * (1 - TIER_TOLERANCE),
      at_30 = salary_basis >= max_30pct * (1 - TIER_TOLERANCE) &
              salary_basis <  max_35pct * (1 - TIER_TOLERANCE),
      at_25 = salary_basis >= max_25pct * (1 - TIER_TOLERANCE) &
              salary_basis <  max_30pct * (1 - TIER_TOLERANCE),

      yos_band = case_when(
        years_of_service >= 10            ~ "10+",
        years_of_service %in% 7:9         ~ "7-9",
        years_of_service >= 0 &
          years_of_service <= 6           ~ "0-6",
        TRUE                              ~ NA_character_
      )
    )

  # Materialize exception thresholds as columns so the waterfall always has them.
  # When the source columns are absent, set to -Inf so no contract matches that
  # branch (and the contract falls through to "standard_or_exception").
  df <- df %>%
    mutate(
      thr_min = if (has_exceptions) .data$veteran_minimum else -Inf,
      thr_bae = if (has_exceptions) .data$bae             else -Inf,
      thr_mle = if (has_exceptions) .data$mle_nontaxpayer else -Inf
    )

  # Contract type waterfall. Order matters; first match wins.
  df <- df %>%
    mutate(
      contract_type = case_when(
        # 1. Supermax: 7-9 YOS, incumbent, at 35%, award-eligible.
        yos_band == "7-9" & incumbent & at_35 & supermax_eligible
          ~ "supermax",
        # 2. Standard 35% max: 10+ YOS at 35% (automatic, not a supermax).
        yos_band == "10+" & at_35
          ~ "max_35",
        # 3. 30% max: 7-9 YOS at 30%, or 0-6 YOS bumped to 30% (Rose Rule).
        yos_band %in% c("7-9", "0-6") & at_30
          ~ "max_30",
        # 4. 25% max: 0-6 YOS at 25%.
        yos_band == "0-6" & at_25
          ~ "max_25",
        # 5-7. MLE / BAE / minimum (thr_* are -Inf when not yet loaded, so these
        #      branches never fire until the per-season values are added).
        salary_basis <= thr_min ~ "minimum",
        salary_basis <= thr_bae ~ "bae",
        salary_basis <= thr_mle ~ "mle",
        # 8. Everything else.
        TRUE ~ "standard_or_exception"
      ),

      # Review flags for the awkward cases — surfaced, not force-fit.
      classification_flag = case_when(
        # 7-9 at 35% incumbent but not award-eligible: 105% case or gap false-neg.
        yos_band == "7-9" & incumbent & at_35 & !supermax_eligible &
          eligibility_uncertain
          ~ "supermax_review_gap",
        yos_band == "7-9" & incumbent & at_35 & !supermax_eligible &
          !eligibility_uncertain
          ~ "supermax_review_105pct",
        # 0-6 at 30% that IS award-eligible: confirm Rose Rule.
        yos_band == "0-6" & at_30 & supermax_eligible
          ~ "rose_rule",
        is.na(yos_band)
          ~ "missing_yos",
        TRUE ~ NA_character_
      ),

      # Treatment category for the analysis (separate axis from contract type).
      treatment_category = case_when(
        !incumbent                              ~ "new_team",
        incumbent & contract_type == "supermax" ~ "re_signed_supermax",
        incumbent                               ~ "re_signed_standard",
        TRUE                                    ~ NA_character_
      )
    )

  df
}

# ------------------------------------------------------------------------------
# QA summary
# ------------------------------------------------------------------------------

qa_report <- function(df) {
  message("\n--- classification QA ---")
  message("events classified: ", nrow(df))

  ct <- df %>% count(contract_type, sort = TRUE)
  message("\ncontract_type:")
  walk2(ct$contract_type, ct$n, ~ message("  ", .x, ": ", .y))

  tc <- df %>% count(treatment_category, sort = TRUE)
  message("\ntreatment_category:")
  walk2(tc$treatment_category, tc$n, ~ message("  ", .x, ": ", .y))

  flagged <- df %>% filter(!is.na(classification_flag))
  message("\nrows needing review: ", nrow(flagged))
  if (nrow(flagged) > 0) {
    fc <- flagged %>% count(classification_flag, sort = TRUE)
    walk2(fc$classification_flag, fc$n, ~ message("  ", .x, ": ", .y))
  }

  uncertain <- sum(df$eligibility_uncertain, na.rm = TRUE)
  message("\nrows with eligibility_uncertain (lookback hit an incomplete ",
          "award season): ", uncertain)

  missing_yos <- sum(is.na(df$yos_band))
  if (missing_yos > 0) {
    message("WARNING: ", missing_yos, " rows have missing/invalid ",
            "years_of_service and could not be tier-classified.")
  }
  message("--- end QA ---\n")
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main <- function(paths) {
  inp <- load_inputs(paths)
  events_elig <- compute_eligibility(inp$events, inp$awards)
  classified  <- classify(events_elig, inp$thresholds)
  qa_report(classified)

  out_cols <- c(
    "event_id", "player_name", "season", "contract_start_season",
    "signing_team", "prior_team", "incumbent", "years_of_service", "yos_band",
    "average_annual_value", "cap_percentage_at_signing", "using_aav_proxy",
    "supermax_eligible", "eligibility_uncertain",
    "contract_type", "treatment_category", "classification_flag"
  )
  out <- classified %>% select(any_of(out_cols))

  dir.create(dirname(paths$out), showWarnings = FALSE, recursive = TRUE)
  write_csv(out, paths$out)
  message("wrote ", nrow(out), " classified events to ", paths$out)
  invisible(out)
}

# Run only when executed directly (e.g. Rscript), not when sourced for testing.
if (sys.nframe() == 0) {
  main(paths)
}
