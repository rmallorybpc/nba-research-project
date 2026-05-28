# ===============================================================================
# 01_build_signing_events.R
#
# Purpose : Build the signing-event table consumed by
#           02_classify_contract_types.R. One row per "contract decision" that
#           the supermax-reset study cares about: every UFA signing (regardless
#           of whether the player stayed or moved) and every veteran extension
#           (whose qualifying form, the Designated Veteran Player Extension, is
#           the supermax). Rookie-scale contracts, rookie-scale extensions other
#           than Rose Rule cases, two-way and 10-day contracts, and Exhibit 10
#           deals are excluded.
#
# Inputs  : External data sources (fetched, not files in the repo):
#             Spotrac or Basketball Reference — UFA signings + extensions per
#               season, with contract dollar values and term.
#             hoopR (or nbastatR) — player metadata: birth date, draft year,
#               primary position, season-by-season game appearances (used to
#               compute years of service).
#             hoopR — season rosters per season, used to resolve prior_team.
#           data/processed/cba_thresholds.csv — for cap-percentage normalization.
#
# Output  : data/processed/signing_events.csv
#
# Depends : dplyr, tidyr, readr, stringr, stringi, purrr, lubridate, hoopR or
#           nbastatR (for player metadata), rvest (for Spotrac/BBRef scraping).
#
# Status  : The fetcher functions are honest scaffolds with detailed TODOs —
#           the scraping infrastructure must be filled in before this script
#           can run end to end. The derivation logic (name normalization, YOS
#           computation, prior-team resolution, cap-% calculation, inclusion
#           filter, validation) is real and exercised by the test block at the
#           bottom of the file against synthetic fixtures.
#
#           CRITICAL: normalize_player_name() in this file MUST stay identical
#           to the one in 02_classify_contract_types.R. If you change one,
#           change both — the awards join in the classifier silently fails on
#           name mismatches.
# ===============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(purrr)
  library(lubridate)
})

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

paths <- list(
  thresholds = "data/processed/cba_thresholds.csv",
  raw_dir    = "data/raw",
  out        = "data/processed/signing_events.csv"
)

# Signing study window. The awards table extends earlier to support lookback;
# this window defines which signings actually get classified.
STUDY_SEASONS <- c(
  "2016-17","2017-18","2018-19","2019-20","2020-21",
  "2021-22","2022-23","2023-24","2024-25"
)

# Contract types to EXCLUDE from the signing-event population. The classifier
# treats inclusion as a precondition — anything that lands in the events table
# gets a classification, so excluding noise here keeps the analysis clean.
EXCLUDE_CONTRACT_KINDS <- c(
  "ten_day",          # 10-day hardship/COVID/standard
  "two_way",          # two-way contracts (and conversions handled separately)
  "exhibit_10",       # training-camp deals
  "rookie_scale",     # rookie-scale first contract (no choice involved)
  "rookie_extension"  # automatic rookie-scale extensions (Rose Rule cases kept)
)

# Throttle for any web scraping the fetchers do. Spotrac and Basketball
# Reference both rate-limit aggressively. Be a good citizen.
SCRAPE_SLEEP_SECONDS <- 4

# ------------------------------------------------------------------------------
# Helpers: season arithmetic
# ------------------------------------------------------------------------------

# "2023-24" -> 2023
season_start_year <- function(season) as.integer(str_sub(season, 1, 4))

# 2022 -> "2022-23"; 1999 -> "1999-00".
make_season_label <- function(start_year) {
  sprintf("%d-%02d", start_year, (start_year + 1) %% 100)
}

# Given a signing date (Date or character), return the offseason calendar year
# the signing belongs to. NBA offseasons run roughly July through September of
# year N for contracts starting in season N-(N+1). A signing in October-June is
# typically an in-season extension or trade-deadline move and still anchors to
# the most recently completed season (year N-1 → "(N-1)-N" season).
#
# Convention used here: signing_offseason_year = the calendar year that contains
# the START of the league year in which the signing was reported. Practically:
#   July-December: signing_offseason_year = year of the date
#   January-June:  signing_offseason_year = year of the date
# Because NBA league years run July 1 -> June 30, a signing on Feb 15, 2024 is
# in the league year that started July 2023 — but its most-recent-completed
# season for award lookback is still the prior season. We therefore anchor
# strictly on calendar year and let the lookback function do the work.
infer_signing_offseason_year <- function(signing_date) {
  d <- as.Date(signing_date)
  if (any(is.na(d))) {
    warning("Some signing_dates failed to parse; rows will have NA ",
            "signing_offseason_year and will fail classifier validation.")
  }
  year(d)
}

# ------------------------------------------------------------------------------
# Helpers: name normalization
# CRITICAL: must stay identical to the function of the same name in
# 02_classify_contract_types.R. Changing one without the other breaks the join.
# ------------------------------------------------------------------------------

normalize_player_name <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[.'`]", "") %>%
    str_to_lower() %>%
    str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b", "") %>%
    str_squish()
}

# ------------------------------------------------------------------------------
# Schema definitions
# ------------------------------------------------------------------------------

# Columns the classifier hard-requires. Validated before write.
CLASSIFIER_REQUIRED_COLS <- c(
  "event_id", "player_name", "season",
  "signing_team", "prior_team",
  "contract_start_season", "signing_offseason_year", "years_of_service",
  "average_annual_value", "cap_percentage_at_signing"
)

# Additional fields the ingest populates for downstream use. The classifier
# tolerates extra columns; carrying these forward is cheap and informative.
INGEST_EXTRA_COLS <- c(
  "signing_date", "total_value", "contract_years", "incumbent_at_signing",
  "age_at_signing", "primary_position", "inclusion_basis"
)

# ------------------------------------------------------------------------------
# Data source fetchers
#
# These are SCAFFOLDS. Each returns a tidy data frame with the documented
# columns. Filling in the actual scrape/API call is the next pipeline task.
# ------------------------------------------------------------------------------

# Return: data frame with columns
#   player_name (chr), signing_date (Date), signing_team (chr),
#   contract_start_season (chr "YYYY-YY"), total_value (dbl),
#   contract_years (int), kind (chr in {"ufa_signing", "extension",
#   "rookie_scale", "rookie_extension", "ten_day", "two_way",
#   "exhibit_10", "renegotiation"})
#
# Source: Spotrac (https://www.spotrac.com/nba/free-agents/_/year/{Y}) for UFAs
# and Spotrac's extension tracker; Basketball Reference's per-season transactions
# page as a cross-check. Both require rvest scraping with SCRAPE_SLEEP_SECONDS
# throttling. Cache raw HTML under {raw_dir}/contracts/{season}.html so re-runs
# don't re-hit the network.
fetch_contracts_for_season <- function(season, raw_dir) {
  stop("fetch_contracts_for_season() not implemented. ",
       "TODO: scrape Spotrac UFA + extension tracker for ", season,
       " and Basketball Reference transactions as a cross-check. ",
       "Cache raw HTML in ", raw_dir, "/contracts/. ",
       "Return tibble per the column contract in the function docstring.")
}

# Return: data frame with columns
#   player_name (chr), birth_date (Date), draft_year (int),
#   primary_position (chr in {"PG","SG","SF","PF","C"}),
#   first_nba_season (chr "YYYY-YY"),
#   active_seasons (list of chr vectors, each a season label where the player
#                   appeared in >= 1 NBA game)
#
# Source: hoopR. Relevant functions (verify exact names against installed hoopR
# version): nba_commonplayerinfo() for bio fields; nba_playerseasontotals()
# aggregated per player gives the active_seasons list. For players whose first
# NBA game came in a different year than their draft (international stash,
# G-League starts), first_nba_season takes precedence over draft_year for YOS.
fetch_player_metadata <- function(player_names) {
  stop("fetch_player_metadata() not implemented. ",
       "TODO: query hoopR commonplayerinfo + per-season totals for ",
       length(player_names), " players. ",
       "Return tibble per the column contract in the function docstring.")
}

# Return: data frame with columns
#   season (chr "YYYY-YY"), player_name (chr), primary_team (chr,
#   3-letter abbrev), games_played (int)
#
# Source: hoopR per-season rosters. "primary_team" = the team the player
# logged the most games for in that season (handles mid-season trades by
# picking the team they ended with most minutes/games). Used by
# resolve_prior_team() below to determine where a free agent came from.
fetch_season_rosters <- function(seasons) {
  stop("fetch_season_rosters() not implemented. ",
       "TODO: load season rosters from hoopR for: ",
       paste(seasons, collapse = ", "), ". ",
       "Aggregate to one row per (season, player) with the primary team. ",
       "Return tibble per the column contract in the function docstring.")
}

# ------------------------------------------------------------------------------
# Derivations
# These are real, runnable, and exercised by the test block.
# ------------------------------------------------------------------------------

# YOS at the moment of signing = number of seasons in which the player appeared
# in >= 1 NBA game, where the season's end-year is <= signing_offseason_year.
# A season "YYYY-YY" ends in year YYYY+1. So a season counts toward YOS for a
# signing in summer of year Y iff (YYYY+1) <= Y, i.e. YYYY <= Y - 1.
#
# Edge cases:
#   - International draftee who didn't play immediately: active_seasons captures
#     this correctly because the season they didn't play won't appear.
#   - Players who missed entire seasons due to injury but were on roster: by the
#     strict CBA definition they earn YOS, but our public-data proxy of "played
#     in >= 1 game" misses these. Documented limitation; rare among the veteran
#     population the supermax question targets.
#   - Signing in the offseason of year Y for a contract starting Y-(Y+1):
#     active_seasons through season (Y-1)-(Y) all count.
compute_years_of_service <- function(active_seasons, signing_offseason_year) {
  if (length(active_seasons) == 0) return(0L)
  starts <- vapply(active_seasons, season_start_year, integer(1))
  sum(starts <= signing_offseason_year - 1L)
}

# Age in completed years at the signing date. Standard birth-date arithmetic.
compute_age_at_signing <- function(birth_date, signing_date) {
  b <- as.Date(birth_date); s <- as.Date(signing_date)
  years <- year(s) - year(b)
  not_yet <- (month(s) < month(b)) |
             (month(s) == month(b) & day(s) < day(b))
  years - as.integer(not_yet)
}

# AAV as a fraction of the contract's first-year salary cap. Uses
# contract_start_season to look up the cap (the cap that applies to the first
# year of the new contract — for an extension this is one or two seasons after
# the signing). Note: AAV is a proxy for first-year salary; the cap-percentage
# computed here mirrors that proxy and inherits its imprecision.
compute_cap_percentage <- function(average_annual_value, contract_start_season,
                                   thresholds) {
  thr <- thresholds %>%
    select(season, salary_cap) %>%
    rename(contract_start_season = season)
  tibble(average_annual_value = average_annual_value,
         contract_start_season = contract_start_season) %>%
    left_join(thr, by = "contract_start_season") %>%
    mutate(cap_pct = average_annual_value / salary_cap) %>%
    pull(cap_pct)
}

# Resolve prior_team. For a signing in the offseason of year Y, prior_team is
# the player's primary team in the immediately preceding season (i.e. the
# season that just ended). If the player wasn't in the NBA the prior season
# (rookie, G-League call-up, international return), prior_team is NA — which
# the classifier handles: incumbent is NA-safe and resolves to FALSE.
resolve_prior_team <- function(contracts, season_rosters) {
  prior <- season_rosters %>%
    mutate(prior_season_start = season_start_year(season) + 1L) %>%
    select(player_name, prior_season_start, prior_team = primary_team)

  contracts %>%
    mutate(prior_season_start = signing_offseason_year) %>%
    left_join(prior, by = c("player_name", "prior_season_start")) %>%
    select(-prior_season_start)
}

# ------------------------------------------------------------------------------
# Inclusion filter
# ------------------------------------------------------------------------------

# Apply scope rules: keep UFA signings, extensions, and renegotiations; drop the
# excluded kinds; restrict to study seasons. Tag each surviving row with an
# inclusion_basis for transparency.
apply_inclusion_filter <- function(contracts) {
  contracts %>%
    filter(!kind %in% EXCLUDE_CONTRACT_KINDS) %>%
    filter(contract_start_season %in% STUDY_SEASONS |
           # extensions can be signed within the study window but start later;
           # include them by signing year falling inside study span
           signing_offseason_year %in% (season_start_year(STUDY_SEASONS))) %>%
    mutate(inclusion_basis = case_when(
      kind == "ufa_signing"    ~ "ufa",
      kind == "extension"      ~ "extension",
      kind == "renegotiation"  ~ "renegotiation",
      TRUE                     ~ "other_included"
    ))
}

# ------------------------------------------------------------------------------
# Build pipeline
# ------------------------------------------------------------------------------

build_signing_events <- function(contracts, player_meta, season_rosters,
                                 thresholds) {

  # 1. Add signing_offseason_year from the date, then apply inclusion filter.
  evt <- contracts %>%
    mutate(signing_offseason_year =
             infer_signing_offseason_year(signing_date)) %>%
    apply_inclusion_filter()

  # 2. Resolve prior_team from prior-season rosters.
  evt <- resolve_prior_team(evt, season_rosters)

  # 3. Join player metadata for YOS, age, position.
  meta_joined <- evt %>%
    left_join(player_meta %>% select(player_name, birth_date, primary_position,
                                     active_seasons),
              by = "player_name") %>%
    rowwise() %>%
    mutate(
      years_of_service = compute_years_of_service(active_seasons,
                                                  signing_offseason_year),
      age_at_signing   = compute_age_at_signing(birth_date, signing_date)
    ) %>%
    ungroup() %>%
    select(-active_seasons)

  # 4. AAV and cap-percentage.
  meta_joined <- meta_joined %>%
    mutate(
      average_annual_value     = total_value / contract_years,
      cap_percentage_at_signing = compute_cap_percentage(
        average_annual_value, contract_start_season, thresholds)
    )

  # 5. Derived flags and identifiers.
  meta_joined <- meta_joined %>%
    mutate(
      incumbent_at_signing = !is.na(prior_team) & prior_team == signing_team,
      season               = contract_start_season,   # alias for classifier
      event_id             = sprintf("%s_%s_%s",
                                     str_replace_all(player_name, "\\s+", "_"),
                                     signing_offseason_year,
                                     signing_team)
    )

  meta_joined
}

# ------------------------------------------------------------------------------
# Validation
# ------------------------------------------------------------------------------

validate_against_classifier <- function(events) {
  missing <- setdiff(CLASSIFIER_REQUIRED_COLS, names(events))
  if (length(missing) > 0) {
    stop("Output is missing classifier-required columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  na_rate <- events %>%
    summarise(across(all_of(CLASSIFIER_REQUIRED_COLS),
                     ~ mean(is.na(.x)))) %>%
    pivot_longer(everything(), names_to = "col", values_to = "na_frac") %>%
    filter(na_frac > 0)
  if (nrow(na_rate) > 0) {
    message("WARNING: required columns with NAs (the classifier will flag ",
            "these rows):")
    walk2(na_rate$col, na_rate$na_frac,
          ~ message("  ", .x, ": ", scales::percent(.y, accuracy = 0.1)))
  }
  dup <- events %>% count(event_id) %>% filter(n > 1)
  if (nrow(dup) > 0) {
    stop("event_id is not unique. Duplicate(s): ",
         paste(head(dup$event_id, 5), collapse = ", "), call. = FALSE)
  }
  invisible(events)
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main <- function(paths) {
  if (!file.exists(paths$thresholds)) {
    stop("Required input not found: ", paths$thresholds, call. = FALSE)
  }
  thresholds <- read_csv(paths$thresholds, show_col_types = FALSE)

  message("Fetching contracts for ", length(STUDY_SEASONS), " seasons...")
  contracts <- map_dfr(STUDY_SEASONS,
                       ~ fetch_contracts_for_season(.x, paths$raw_dir))

  player_names_needed <- unique(contracts$player_name)
  message("Fetching metadata for ", length(player_names_needed), " players...")
  player_meta <- fetch_player_metadata(player_names_needed)

  seasons_needed <- unique(c(STUDY_SEASONS,
                             make_season_label(season_start_year(STUDY_SEASONS) - 1L)))
  message("Fetching rosters for ", length(seasons_needed), " seasons...")
  season_rosters <- fetch_season_rosters(seasons_needed)

  events <- build_signing_events(contracts, player_meta, season_rosters,
                                 thresholds)
  validate_against_classifier(events)

  out_cols <- c(CLASSIFIER_REQUIRED_COLS, INGEST_EXTRA_COLS)
  out <- events %>% select(any_of(out_cols))
  dir.create(dirname(paths$out), showWarnings = FALSE, recursive = TRUE)
  write_csv(out, paths$out)
  message("wrote ", nrow(out), " signing events to ", paths$out)
  invisible(out)
}

if (sys.nframe() == 0) {
  main(paths)
}
