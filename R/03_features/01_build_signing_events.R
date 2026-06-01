# ==============================================================================
# 01_build_signing_events.R   (roster-diff spine — launch version)
#
# Purpose : Build the analysis event table from consecutive-season roster
#           comparison plus the curated supermax overlay. This REPLACES the
#           retired contract-tracker spine. It writes signing_events_classified
#           directly, with treatment_category already assigned, so the salary-
#           based classifier (02_classify_contract_types.R) is not on the
#           launch path.
#
# Spine logic :
#   For every pair of consecutive seasons (N, N+1), each player who appears in
#   both rosters generates one event for the offseason between them:
#     prior_team   = the team they ended season N on
#     signing_team = the team they ended season N+1 on
#     team changed -> treatment "new_team"
#     team same    -> treatment "re_signed_standard"
#   Then the curated extensions overlay flips any matched player-offseason to
#   "re_signed_supermax". The overlay is the ONLY source of the supermax label,
#   because roster-diff cannot see contract type.
#
# Known limitation (documented, accepted) :
#   "new_team" mixes free-agent signings with trades. "re_signed_standard"
#   mixes genuine re-signings with players simply continuing a multi-year
#   contract. Only "re_signed_supermax" is mechanism-exact. The cleanest read
#   is therefore supermax vs movers; the standard-stayer baseline is diluted.
#   Contract-value, tier, and free-agency-vs-trade refinements are on the
#   roadmap alongside the forced-move ("Harden effect") work.
#
# Repeated measures :
#   A player who stays for years generates one "stayed" event per offseason, so
#   observations are not independent within player. The supermax regression
#   should cluster standard errors by player (recommended refinement). Season
#   fixed effects already absorb era trends.
#
# Inputs  : R/01_ingest/fetch_season_rosters.R   (roster spine)
#           R/01_ingest/fetch_extensions.R        (supermax overlay)
#           R/01_ingest/fetch_player_impact.R     (age + contributor minutes)
#           data/processed/nba_extensions.csv     (curated overlay data)
#
# Output  : data/processed/signing_events_classified.csv
#
# Depends : dplyr, tidyr, readr, stringr, stringi, purrr, lubridate
# ==============================================================================

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
# Source the fetchers. Paths assume repo root as working directory.
# ------------------------------------------------------------------------------
source("R/01_ingest/fetch_season_rosters.R")
source("R/01_ingest/fetch_extensions.R")
source("R/01_ingest/fetch_player_impact.R")

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

paths <- list(
  extensions = "data/processed/nba_extensions.csv",
  raw_dir    = "data/raw",
  out        = "data/processed/signing_events_classified.csv"
)

# Offseasons in the study window. Offseason year Y sits between season starting
# Y-1 and season starting Y. 2017..2024 = the eight offseasons whose signings
# the study evaluates (matches the supermax overlay span).
STUDY_OFFSEASONS <- 2017:2024

# Uniform post-signing evaluation horizon, in seasons. Applied to every event
# regardless of treatment so movers and stayers are compared on equal footing.
# MIS reads this as contract_years and builds the post-window from it.
ANALYSIS_HORIZON <- 3L

# Contributor filter: a player must have appeared in at least this many games
# in the pre-signing season to count as a meaningful contributor. Cuts end-of-
# bench churn that would swamp the re_signed_standard group with noise. 40 is
# roughly half a season.
MIN_GAMES_PRESEASON <- 40L

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

# Identical to normalize_player_name() in every fetcher and the MIS script.
normalize_player_name <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[.'`]", "") %>%
    str_to_lower() %>%
    str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b", "") %>%
    str_squish()
}

make_season_label <- function(start_year) {
  sprintf("%d-%02d", start_year, (start_year + 1) %% 100)
}

# ------------------------------------------------------------------------------
# Load inputs
# ------------------------------------------------------------------------------

load_inputs <- function(paths) {
  # Seasons needed for the roster diff: every season touched by the offseasons.
  # Offseason Y needs seasons (Y-1) and (Y).
  roster_start_years <- sort(unique(c(STUDY_OFFSEASONS - 1L, STUDY_OFFSEASONS)))
  roster_seasons <- make_season_label(roster_start_years)

  message("Loading rosters for ", length(roster_seasons), " seasons...")
  rosters <- fetch_season_rosters(roster_seasons, paths$raw_dir)

  message("Loading curated supermax overlay...")
  extensions <- fetch_extensions(path = paths$extensions)

  # Impact data is needed here only for AGE in the pre-signing season. The
  # pre-signing seasons are the season starting in (offseason - 1).
  impact_seasons <- make_season_label(sort(unique(STUDY_OFFSEASONS - 1L)))
  message("Loading impact data (for age) for ", length(impact_seasons),
          " seasons...")
  impact <- fetch_player_impact(impact_seasons, paths$raw_dir)

  list(rosters = rosters, extensions = extensions, impact = impact)
}

# ------------------------------------------------------------------------------
# Roster-diff core
# ------------------------------------------------------------------------------

# Build one event per (contributor, offseason). prior_team from season N,
# signing_team from season N+1.
build_roster_diff_events <- function(rosters) {

  map_dfr(STUDY_OFFSEASONS, function(offseason_year) {
    pre_season  <- make_season_label(offseason_year - 1L)
    post_season <- make_season_label(offseason_year)

    prev <- rosters %>%
      filter(season == pre_season) %>%
      transmute(name_norm, player_name,
                prior_team = primary_team,
                games_pre  = games_played)

    curr <- rosters %>%
      filter(season == post_season) %>%
      transmute(name_norm, signing_team = primary_team)

    inner_join(prev, curr, by = "name_norm") %>%
      mutate(
        signing_offseason_year = offseason_year,
        pre_signing_season     = pre_season,
        contract_start_season  = post_season,   # analysis anchor (offseason+1)
        incumbent              = prior_team == signing_team,
        treatment_category     = if_else(incumbent,
                                         "re_signed_standard", "new_team")
      )
  })
}

# ------------------------------------------------------------------------------
# Contributor filter
# ------------------------------------------------------------------------------

apply_contributor_filter <- function(events, min_games) {
  before <- nrow(events)
  out <- events %>% filter(games_pre >= min_games)
  message("  contributor filter (>= ", min_games, " games): ",
          before, " -> ", nrow(out), " events")
  out
}

# ------------------------------------------------------------------------------
# Supermax overlay
# Flip matched player-offseasons to re_signed_supermax. Match on normalized
# name AND offseason year, so only the actual signing offseason is flagged
# (not every season the player stayed).
# ------------------------------------------------------------------------------

overlay_supermax <- function(events, extensions) {

  ext <- extensions %>%
    mutate(
      name_norm = normalize_player_name(player_name),
      ext_year  = as.integer(format(as.Date(signing_date), "%Y"))
    ) %>%
    select(name_norm, ext_year, is_designated_veteran, supermax_full_35pct,
           qualifying_basis)

  joined <- events %>%
    left_join(ext, by = c("name_norm" = "name_norm",
                          "signing_offseason_year" = "ext_year"))

  matched <- joined %>% filter(!is.na(is_designated_veteran))

  # Validation: every overlay row should match exactly one event, and that
  # event must be an incumbent (stayed) event.
  n_ext <- nrow(ext)
  n_hit <- nrow(matched)
  if (n_hit < n_ext) {
    unmatched <- anti_join(ext, events,
                           by = c("name_norm",
                                  "ext_year" = "signing_offseason_year"))
    warning("Overlay: ", n_ext - n_hit, " of ", n_ext,
            " supermax rows did NOT match a roster-diff event. ",
            "Unmatched: ", paste(unmatched$name_norm, collapse = ", "),
            ". Likely the player did not appear in both seasons (injury year) ",
            "or a name-normalization mismatch.")
  }
  bad <- matched %>% filter(!incumbent)
  if (nrow(bad) > 0) {
    warning("Overlay: ", nrow(bad), " supermax events are NOT incumbent ",
            "(team change in the signing offseason). Inspect: ",
            paste(bad$player_name, collapse = ", "))
  }

  joined %>%
    mutate(
      treatment_category = if_else(!is.na(is_designated_veteran),
                                   "re_signed_supermax", treatment_category)
    )
}

# ------------------------------------------------------------------------------
# Age join (from BBRef advanced data, season N)
# ------------------------------------------------------------------------------

join_age <- function(events, impact) {
  if (!"age" %in% names(impact)) {
    warning("impact data has no 'age' column. age_at_signing will be NA. ",
            "Apply the age patch to fetch_player_impact.R (emit the Age ",
            "column) and re-run, or the regression's age control will not bind.")
    return(events %>% mutate(age_at_signing = NA_real_))
  }
  age_lookup <- impact %>%
    mutate(name_norm = normalize_player_name(player_name)) %>%
    select(name_norm, season, age) %>%
    distinct(name_norm, season, .keep_all = TRUE)

  events %>%
    left_join(age_lookup,
              by = c("name_norm", "pre_signing_season" = "season")) %>%
    rename(age_at_signing = age)
}

# ------------------------------------------------------------------------------
# Assemble output
# ------------------------------------------------------------------------------

assemble_output <- function(events) {
  events %>%
    mutate(
      contract_years = ANALYSIS_HORIZON,        # uniform evaluation horizon
      season         = contract_start_season,   # alias some scripts read

      contract_type = case_when(
        treatment_category == "re_signed_supermax" &
          !is.na(supermax_full_35pct) & supermax_full_35pct ~ "supermax",
        treatment_category == "re_signed_supermax"          ~ "supermax_partial",
        treatment_category == "new_team"                    ~ "team_change",
        TRUE                                                ~ "roster_continuity"
      ),

      # Column-compatibility placeholders for downstream scripts that may
      # select these. Not produced under the roster-diff spine.
      years_of_service          = NA_integer_,
      yos_band                  = NA_character_,
      average_annual_value      = NA_real_,
      cap_percentage_at_signing = NA_real_,
      is_designated_veteran     = if_else(is.na(is_designated_veteran),
                                          FALSE, is_designated_veteran),
      supermax_full_35pct       = if_else(is.na(supermax_full_35pct),
                                          FALSE, supermax_full_35pct),

      event_id = sprintf("%s_%d_%s",
                         str_replace_all(name_norm, "\\s+", "_"),
                         signing_offseason_year, signing_team)
    ) %>%
    select(
      event_id, player_name, season, contract_start_season,
      signing_offseason_year, pre_signing_season,
      prior_team, signing_team, incumbent,
      treatment_category, contract_type, contract_years,
      age_at_signing, games_pre,
      years_of_service, yos_band,
      is_designated_veteran, supermax_full_35pct,
      average_annual_value, cap_percentage_at_signing
    )
}

# ------------------------------------------------------------------------------
# Validation + QA report
# ------------------------------------------------------------------------------

validate_and_report <- function(events, extensions) {
  dup <- events %>% count(event_id) %>% filter(n > 1)
  if (nrow(dup) > 0) {
    stop("event_id not unique. Duplicates: ",
         paste(head(dup$event_id, 5), collapse = ", "), call. = FALSE)
  }

  n_supermax <- sum(events$treatment_category == "re_signed_supermax")
  n_ext <- nrow(extensions)

  message("\n--- SIGNING EVENTS (roster-diff spine) ---")
  message("total events: ", nrow(events))
  tc <- events %>% count(treatment_category, sort = TRUE)
  walk2(tc$treatment_category, tc$n,
        ~ message(sprintf("  %-22s %d", .x, .y)))
  message("\nsupermax overlay: ", n_supermax, " of ", n_ext,
          " curated rows landed as re_signed_supermax events")
  age_missing <- mean(is.na(events$age_at_signing))
  message(sprintf("age_at_signing missing: %.1f%%", 100 * age_missing))
  message("--- end ---\n")

  invisible(events)
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main <- function(paths) {
  inp <- load_inputs(paths)

  events <- build_roster_diff_events(inp$rosters)
  events <- apply_contributor_filter(events, MIN_GAMES_PRESEASON)
  events <- overlay_supermax(events, inp$extensions)
  events <- join_age(events, inp$impact)
  out    <- assemble_output(events)

  validate_and_report(out, inp$extensions)

  dir.create(dirname(paths$out), showWarnings = FALSE, recursive = TRUE)
  write_csv(out, paths$out)
  message("wrote ", nrow(out), " events to ", paths$out)
  invisible(out)
}

if (sys.nframe() == 0) {
  main(paths)
}
