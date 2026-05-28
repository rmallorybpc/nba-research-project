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
# Inputs  : data/processed/signing_events.csv      (one row per player-team-contract)
#           data/processed/cba_thresholds.csv      (cap + max tiers + MLE/BAE by season)
#           data/processed/nba_awards.csv          (MVP / DPOY / All-NBA by season)
#           data/processed/nba_minimum_scale.csv   (minimum salary by season + YOS)
#
# Output  : data/processed/signing_events_classified.csv
#
# Depends : dplyr, tidyr, readr, stringr, stringi, purrr
#
# Notes   : One input remains to be built before this runs end to end:
#             signing_events must carry `years_of_service`. Age is NOT a
#             substitute — the tier (25/30/35) and the 7-9 supermax band are
#             defined on YOS, and the minimum branch joins the YOS-specific
#             minimum scale.
#           The thresholds file carries the max tiers plus MLE (non-taxpayer,
#           taxpayer, room) and BAE per season; the minimum scale is a separate
#           (season, YOS) lookup. All sub-max branches are therefore active.
#           Award data is complete for 2013-14 through 2024-25 (all three All-NBA
#           teams plus MVP and DPOY per season). The `eligibility_uncertain` flag
#           remains in the output as defensive infrastructure but will be FALSE
#           for all rows unless the signing window is extended beyond the awards
#           coverage.
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
  min_scale  = "data/processed/nba_minimum_scale.csv",
  out        = "data/processed/signing_events_classified.csv"
)

# Tolerance band on the max-tier dollar comparison, to absorb rounding and the
# small gap between AAV and true first-year salary. A contract within this
# fraction below a tier threshold still counts as that tier.
TIER_TOLERANCE <- 0.02

# Award seasons known to be incomplete in nba_awards.csv. As of the awards table
# being completed for 2013-14 through 2024-25, this is empty — every season has
# all three All-NBA teams plus MVP and DPOY. The mechanism is retained so that if
# the signing window is ever extended to seasons the awards table does not yet
# cover, those seasons can be listed here and affected signings flagged rather
# than silently mis-coded.
INCOMPLETE_AWARD_SEASONS <- character(0)

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

# For a signing/extension that occurred in the offseason of calendar year
# `signing_year`, return the n most-recent COMPLETED seasons at signing time,
# most-recent first. A signing in summer 2017 (UFA for 2017-18, OR an extension
# whose new money starts later) looks back at 2016-17, 2015-16, 2014-15. A
# signing in summer 2024 (UFA for 2024-25, OR an extension starting 2025-26)
# looks back at 2023-24, 2022-23, 2021-22. The lookback anchor is the SIGNING
# DATE, not the contract start — they differ for veteran extensions, where the
# new contract often begins a season or two after the extension is signed.
lookback_seasons <- function(signing_year, n = 3) {
  start_years <- (signing_year - 1):(signing_year - n)
  make_season_label(start_years)
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
  walk(paths[c("events", "thresholds", "awards", "min_scale")], function(p) {
    if (!file.exists(p)) stop("Required input not found: ", p, call. = FALSE)
  })

  events     <- read_csv(paths$events, show_col_types = FALSE)
  thresholds <- read_csv(paths$thresholds, show_col_types = FALSE)
  awards     <- read_csv(paths$awards, show_col_types = FALSE)
  min_scale  <- read_csv(paths$min_scale, show_col_types = FALSE)

  # Required fields on the signing-event table. years_of_service is mandatory.
  # signing_offseason_year is mandatory because it (not contract_start_season)
  # anchors the awards lookback — see lookback_seasons() comments.
  required_event_cols <- c(
    "event_id", "player_name", "season", "signing_team", "prior_team",
    "contract_start_season", "signing_offseason_year", "years_of_service",
    "average_annual_value", "cap_percentage_at_signing"
  )
  missing <- setdiff(required_event_cols, names(events))
  if (length(missing) > 0) {
    stop("signing_events is missing required columns: ",
         paste(missing, collapse = ", "),
         "\n  years_of_service in particular cannot be derived from age.",
         "\n  signing_offseason_year drives the awards lookback for extensions.",
         call. = FALSE)
  }

  required_threshold_cols <- c("season", "salary_cap",
                               "max_25pct", "max_30pct", "max_35pct",
                               "mle_nontaxpayer", "mle_taxpayer", "mle_room", "bae")
  missing_t <- setdiff(required_threshold_cols, names(thresholds))
  if (length(missing_t) > 0) {
    stop("cba_thresholds is missing required columns: ",
         paste(missing_t, collapse = ", "), call. = FALSE)
  }

  required_min_cols <- c("season", "years_of_service", "minimum_salary")
  missing_m <- setdiff(required_min_cols, names(min_scale))
  if (length(missing_m) > 0) {
    stop("nba_minimum_scale is missing required columns: ",
         paste(missing_m, collapse = ", "), call. = FALSE)
  }

  list(events = events, thresholds = thresholds,
       awards = awards, min_scale = min_scale)
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
  # Lookback is anchored on signing_offseason_year — the calendar year of the
  # offseason when the contract was signed — NOT on contract_start_season. For
  # extensions, these can differ by one or two seasons, and using start would
  # shift the window forward incorrectly.
  event_lb <- events %>%
    select(event_id, player_name, signing_offseason_year) %>%
    mutate(player_norm = normalize_player_name(player_name)) %>%
    mutate(lb = map(signing_offseason_year, ~ lookback_seasons(.x, 3))) %>%
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

classify <- function(events, thresholds, min_scale) {

  # Per-player minimum salary is YOS-specific (a rookie minimum is ~a third of a
  # 10+ year veteran minimum), so a single per-season threshold would mis-classify.
  # Join the minimum scale on (season, capped YOS). YOS is capped at 10 because the
  # scale's "10" row is the 10+ ceiling tier.
  min_lookup <- min_scale %>%
    select(season, years_of_service, minimum_salary) %>%
    rename(min_yos = years_of_service)

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
      ),

      # Cap YOS at 10 to match the scale's 10+ ceiling row for the join.
      min_yos = pmin(years_of_service, 10L)
    ) %>%
    left_join(min_lookup, by = c("season", "min_yos")) %>%
    mutate(
      # The player's own minimum for this season/YOS. If the join missed (e.g. a
      # season outside the scale), fall back to -Inf so the minimum branch can't
      # fire on bad data rather than mis-firing on a wrong number.
      thr_min = coalesce(minimum_salary, -Inf),
      # MLE/BAE thresholds (now always present in the thresholds file).
      thr_bae = bae,
      thr_mle = mle_nontaxpayer
    )

  # Contract type waterfall. Order matters; first match wins. Tolerance band on the
  # minimum/BAE/MLE comparisons absorbs the small AAV-vs-first-year gap.
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
        # 5. Minimum: at or below the player's own YOS-specific minimum.
        salary_basis <= thr_min * (1 + TIER_TOLERANCE) ~ "minimum",
        # 6. Bi-annual exception.
        salary_basis <= thr_bae * (1 + TIER_TOLERANCE) ~ "bae",
        # 7. Mid-level exception (non-taxpayer ceiling — the widest MLE band).
        salary_basis <= thr_mle * (1 + TIER_TOLERANCE) ~ "mle",
        # 8. Everything else: a negotiated, non-max, above-exception deal.
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
  classified  <- classify(events_elig, inp$thresholds, inp$min_scale)
  qa_report(classified)

  out_cols <- c(
    "event_id", "player_name", "season", "contract_start_season",
    "signing_offseason_year",
    "signing_team", "prior_team", "incumbent", "years_of_service", "yos_band",
    "average_annual_value", "cap_percentage_at_signing", "using_aav_proxy",
    "minimum_salary", "supermax_eligible", "eligibility_uncertain",
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
