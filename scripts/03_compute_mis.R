# ==============================================================================
# 03_compute_mis.R
#
# Purpose : Compute Movement Impact Score (MIS) for each classified signing
#           event. The MIS captures whether the deal delivered the performance
#           the team paid for, measured as the change in Estimated Plus-Minus
#           (BPM) from a pre-signing baseline window to a post-signing
#           realized window.
#
#           The analytical question MIS supports: do supermax recipients show
#           systematically worse pre-to-post trajectories than peers who
#           re-signed on standard deals or who changed teams?
#
# Inputs  : data/processed/signing_events_classified.csv
#             (one row per classified signing - from 02_classify_contract_types)
#           data/processed/nba_player_impact.csv
#             (one row per player-season with an impact metric and minutes -
#              produced by R/01_ingest/fetch_player_impact.R, which currently pulls
#              Box Plus-Minus from Basketball Reference. Column names are metric-
#              agnostic so a future EPM fetcher swaps in transparently.)
#
# Output  : data/processed/signing_events_mis.csv
#
# Depends : dplyr, tidyr, readr, stringr, purrr, tibble
#
# Impact metric schema (required columns in nba_player_impact.csv):
#   player_name    chr     - should normalize to the same form as the classifier
#   season         chr     - "YYYY-YY"
#   impact_overall    dbl     - season-aggregate impact (BPM in launch, EPM-compatible)
#   impact_offense    dbl     - offensive component (OBPM in launch)
#   impact_defense    dbl     - defensive component (DBPM in launch)
#   minutes_played int     - total regular-season minutes
#   games_played   int     - total regular-season games (QA only)
#
# Default formula (revisable - see "Formula choices" below):
#   pre_impact  = minutes-weighted mean impact metric over PRE_WINDOW_SEASONS
#               seasons ending the season before contract_start_season.
#   post_impact = minutes-weighted mean impact metric over contract_years seasons
#               starting at contract_start_season.
#   MIS      = post_impact - pre_impact
#
# Direction: positive MIS = player performed better under the new contract;
# negative MIS = player declined post-signing. The supermax-reset hypothesis
# predicts the re_signed_supermax cohort will have the most negative mean MIS.
#
# Why minutes weighting WITHIN windows but not BETWEEN them: within a multi-
# season window, a 3000-minute season is more informative than a 500-minute
# season, so weight by minutes. Across the pre-to-post comparison, weighting
# would conflate the analytical question ("did they decline") with a separate
# participation question ("did they play"). Participation is reported as a QA
# field for downstream interpretation, not baked into MIS magnitude.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(purrr)
  library(tibble)
})

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

paths <- list(
  events   = "data/processed/signing_events_classified.csv",
  impact   = "data/processed/nba_player_impact.csv",
  out      = "data/processed/signing_events_mis.csv"
)

# Pre-signing baseline: how many seasons back to average for the pre window.
# Two seasons is the default - long enough to smooth single-season noise,
# short enough to capture the player's recent trajectory rather than a
# career-long average.
PRE_WINDOW_SEASONS <- 2L

# Post window length: by default the full contract_years from contract_start_season.
# Set POST_WINDOW_MAX to cap (e.g. 4) if you want to compare deals of different
# lengths on the same horizon. NA means use the full contract length per event.
POST_WINDOW_MAX <- NA_integer_

# Minimum minutes per season for that season to count toward window
# aggregation. Filters out injury-shortened or near-DNP seasons that would
# pollute the mean. 500 is roughly 8 games at 30 min/game - generous enough
# to keep partial seasons, strict enough to exclude DNP-level participation.
MIN_MINUTES_PER_SEASON <- 500L

# Quality flag thresholds.
QA_MIN_POST_SEASONS <- 1L   # need at least 1 valid post-window season
QA_MIN_PRE_SEASONS  <- 1L   # need at least 1 valid pre-window season

# ------------------------------------------------------------------------------
# Helpers: season arithmetic
# ------------------------------------------------------------------------------

season_start_year <- function(season) as.integer(str_sub(season, 1, 4))

make_season_label <- function(start_year) {
  sprintf("%d-%02d", start_year, (start_year + 1) %% 100)
}

# Build a vector of season labels relative to an anchor.
#   direction = "back" returns the n seasons IMMEDIATELY BEFORE anchor.
#   direction = "forward" returns the n seasons STARTING AT anchor.
# Used for pre-window (back from contract_start_season) and post-window
# (forward from contract_start_season).
season_window <- function(anchor_season, n, direction = c("back", "forward")) {
  direction <- match.arg(direction)
  y <- season_start_year(anchor_season)
  if (direction == "back") {
    make_season_label((y - n):(y - 1))
  } else {
    make_season_label(y:(y + n - 1L))
  }
}

# Name normalization - MUST stay identical to the function of the same name in
# 01_build_signing_events.R, fetch_player_metadata.R, fetch_season_rosters.R,
# and 02_classify_contract_types.R. Same caveat as elsewhere: factor into a
# shared utils file at the next refactor pass.
normalize_player_name <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[.'`]", "") %>%
    str_to_lower() %>%
    str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b", "") %>%
    str_squish()
}

# Minutes-weighted mean. Handles NAs and empty inputs gracefully - returns NA
# (not 0) when no valid contribution exists, so downstream rolling means don't
# get diluted by spurious zeros.
mw_mean <- function(values, weights) {
  ok <- !is.na(values) & !is.na(weights) & weights > 0
  if (!any(ok)) return(NA_real_)
  sum(values[ok] * weights[ok]) / sum(weights[ok])
}

# ------------------------------------------------------------------------------
# Load + validate
# ------------------------------------------------------------------------------

load_inputs <- function(paths) {
  walk(paths[c("events", "impact")], function(p) {
    if (!file.exists(p)) {
      stop("Required input not found: ", p, call. = FALSE)
    }
  })

  events <- read_csv(paths$events, show_col_types = FALSE)
  impact <- read_csv(paths$impact,    show_col_types = FALSE)

  required_event_cols <- c(
    "event_id", "player_name", "contract_start_season", "contract_years",
    "treatment_category", "contract_type"
  )
  missing_e <- setdiff(required_event_cols, names(events))
  if (length(missing_e) > 0) {
    stop("Classified events missing required columns: ",
         paste(missing_e, collapse = ", "), call. = FALSE)
  }

  required_impact_cols <- c("player_name", "season",
                         "impact_overall", "impact_offense", "impact_defense",
                         "minutes_played")
  missing_p <- setdiff(required_impact_cols, names(impact))
  if (length(missing_p) > 0) {
    stop("Impact metric table missing required columns: ",
         paste(missing_p, collapse = ", "),
         "\n  Expected schema is documented in the script header.",
         call. = FALSE)
  }

  list(events = events, impact = impact)
}

# ------------------------------------------------------------------------------
# Window building
#
# For each event, attach two list-columns: pre_window_seasons and
# post_window_seasons (vectors of "YYYY-YY" labels).
# ------------------------------------------------------------------------------

attach_windows <- function(events) {
  events %>%
    rowwise() %>%
    mutate(
      pre_window_seasons = list(season_window(
        contract_start_season, PRE_WINDOW_SEASONS, "back"
      )),
      # Post window length = contract_years, capped at POST_WINDOW_MAX if set.
      post_window_length = if (is.na(POST_WINDOW_MAX)) {
        as.integer(contract_years)
      } else {
        min(as.integer(contract_years), POST_WINDOW_MAX)
      },
      post_window_seasons = list(season_window(
        contract_start_season, post_window_length, "forward"
      ))
    ) %>%
    ungroup()
}

# ------------------------------------------------------------------------------
# Window aggregation
#
# For a list of events with a window column, expand to one row per (event,
# season-in-window), join the impact metric, filter by minimum minutes, then
# collapse to one row per event with minutes-weighted means per dimension.
# ------------------------------------------------------------------------------

aggregate_window <- function(events_with_window, impact,
                             window_col, label_suffix) {

  impact_norm <- impact %>%
    mutate(name_norm = normalize_player_name(player_name)) %>%
    select(name_norm, season,
           impact_overall, impact_offense, impact_defense,
           minutes_played, games_played)

  ev <- events_with_window %>%
    mutate(name_norm = normalize_player_name(player_name)) %>%
    select(event_id, name_norm, all_of(window_col))

  expanded <- ev %>%
    unnest_longer(all_of(window_col), values_to = "season") %>%
    left_join(impact_norm, by = c("name_norm", "season"))

  # Apply minimum-minutes filter for window aggregation. Seasons under the
  # threshold are excluded from the mean but still surface in QA counts.
  valid <- expanded %>%
    filter(!is.na(minutes_played) & minutes_played >= MIN_MINUTES_PER_SEASON)

  agg <- valid %>%
    group_by(event_id) %>%
    summarise(
      impact_overall_mw    = mw_mean(impact_overall, minutes_played),
      impact_offense_mw    = mw_mean(impact_offense, minutes_played),
      impact_defense_mw    = mw_mean(impact_defense, minutes_played),
      total_minutes     = sum(minutes_played, na.rm = TRUE),
      total_games       = sum(games_played,   na.rm = TRUE),
      seasons_valid     = n(),
      .groups = "drop"
    )

  # Also count seasons present in the window regardless of validity - useful
  # for QA (a player who played 3 seasons of 200 minutes each is different
  # from one who simply didn't play).
  seasons_seen <- expanded %>%
    group_by(event_id) %>%
    summarise(seasons_in_window = sum(!is.na(season)),
              seasons_with_any_data = sum(!is.na(minutes_played)),
              .groups = "drop")

  out <- agg %>% full_join(seasons_seen, by = "event_id")

  # Rename with the label suffix so pre and post can be joined back without
  # column collisions.
  names(out) <- ifelse(names(out) == "event_id",
                       "event_id",
                       paste(label_suffix, names(out), sep = "_"))
  out
}

# ------------------------------------------------------------------------------
# Main MIS computation
# ------------------------------------------------------------------------------

compute_mis <- function(events, impact) {

  ev_windows <- attach_windows(events)

  pre  <- aggregate_window(ev_windows, impact, "pre_window_seasons",  "pre")
  post <- aggregate_window(ev_windows, impact, "post_window_seasons", "post")

  joined <- ev_windows %>%
    left_join(pre,  by = "event_id") %>%
    left_join(post, by = "event_id")

  joined %>%
    mutate(
      # Core MIS - the headline metric. Negative = decline post-signing.
      mis_overall = post_impact_overall_mw - pre_impact_overall_mw,
      mis_offense = post_impact_offense_mw - pre_impact_offense_mw,
      mis_defense = post_impact_defense_mw - pre_impact_defense_mw,

      # Data quality flag. "complete" means both windows have enough valid
      # seasons; the partials are surfaced so downstream filtering can be
      # explicit rather than implicit-via-NA.
      mis_data_quality = case_when(
        is.na(pre_seasons_valid) | is.na(post_seasons_valid)
          ~ "missing_data",
        pre_seasons_valid  < QA_MIN_PRE_SEASONS &
        post_seasons_valid < QA_MIN_POST_SEASONS
          ~ "insufficient_both",
        pre_seasons_valid  < QA_MIN_PRE_SEASONS
          ~ "insufficient_pre",
        post_seasons_valid < QA_MIN_POST_SEASONS
          ~ "insufficient_post",
        TRUE
          ~ "complete"
      ),

      # Participation field for downstream interpretation - NOT used to scale
      # MIS magnitude (see header note on minutes weighting).
      post_minutes_per_season = if_else(
        post_seasons_valid > 0,
        post_total_minutes / post_seasons_valid,
        NA_real_
      )
    )
}

# ------------------------------------------------------------------------------
# QA summary
# ------------------------------------------------------------------------------

qa_report <- function(df) {
  message("\n--- MIS QA ---")
  message("events processed: ", nrow(df))

  q <- df %>% count(mis_data_quality, sort = TRUE)
  message("\ndata quality:")
  walk2(q$mis_data_quality, q$n, ~ message("  ", .x, ": ", .y))

  # Mean MIS by treatment category - the headline comparison.
  complete <- df %>% filter(mis_data_quality == "complete")
  if (nrow(complete) > 0 && "treatment_category" %in% names(complete)) {
    tc <- complete %>%
      group_by(treatment_category) %>%
      summarise(
        n = n(),
        mean_mis_overall = mean(mis_overall, na.rm = TRUE),
        mean_mis_offense = mean(mis_offense, na.rm = TRUE),
        mean_mis_defense = mean(mis_defense, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(treatment_category)
    message("\nmean MIS by treatment (complete-data events only):")
    pwalk(tc, function(treatment_category, n,
                       mean_mis_overall, mean_mis_offense, mean_mis_defense) {
      message(sprintf("  %-22s n=%-4d overall=%+.3f  off=%+.3f  def=%+.3f",
                      treatment_category, n,
                      mean_mis_overall, mean_mis_offense, mean_mis_defense))
    })
    message("\nHypothesis check: re_signed_supermax should be most negative.")
  }
  message("--- end QA ---\n")
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main <- function(paths) {
  inp <- load_inputs(paths)
  result <- compute_mis(inp$events, inp$impact)
  qa_report(result)

  out_cols <- c(
    "event_id", "player_name", "contract_start_season", "contract_years",
    "treatment_category", "contract_type",
    "pre_impact_overall_mw", "pre_impact_offense_mw", "pre_impact_defense_mw",
    "pre_total_minutes", "pre_seasons_valid",
    "post_impact_overall_mw", "post_impact_offense_mw", "post_impact_defense_mw",
    "post_total_minutes", "post_seasons_valid", "post_minutes_per_season",
    "mis_overall", "mis_offense", "mis_defense", "mis_data_quality"
  )
  out <- result %>% select(any_of(out_cols))

  dir.create(dirname(paths$out), showWarnings = FALSE, recursive = TRUE)
  write_csv(out, paths$out)
  message("wrote ", nrow(out), " MIS rows to ", paths$out)
  invisible(out)
}

if (sys.nframe() == 0) {
  main(paths)
}

# ------------------------------------------------------------------------------
# Formula choices - change these here, not in scattered call sites.
#
# The default MIS = mw_mean_post_impact - mw_mean_pre_impact is the simplest
# defensible formulation and matches the research question directly. If
# methodology evolves, places to revise:
#
#   1. Pre-window length - see PRE_WINDOW_SEASONS. Lengthening to 3 smooths
#      noise but biases toward career arc; shortening to 1 captures recent
#      form at the cost of single-season variance.
#
#   2. Post-window cap - see POST_WINDOW_MAX. Setting to a fixed value (e.g.
#      4) makes long deals and short deals comparable on the same horizon.
#
#   3. Min-minutes threshold - see MIN_MINUTES_PER_SEASON. Raising filters
#      noise from low-minute role players; lowering keeps more events at the
#      cost of letting in low-participation seasons.
#
#   4. Aggregation across windows - currently simple mean of minutes-weighted
#      season means. Alternatives: median, robust mean, or recency-weighted
#      average. Each has tradeoffs; the simple mean is interpretable.
#
#   5. Replacement-level baseline - for an "expected contribution" version,
#      subtract a position-and-age-adjusted league baseline before computing
#      the pre-to-post delta. This isolates the player's own trajectory from
#      league-wide trends. Not the default because it adds complexity without
#      changing the within-cohort comparison the headline result depends on.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Self-test (helpers only - does NOT require data)
# ------------------------------------------------------------------------------

if (FALSE) {
  # season_window
  stopifnot(identical(
    season_window("2024-25", 2, "back"),
    c("2022-23", "2023-24")
  ))
  stopifnot(identical(
    season_window("2024-25", 3, "forward"),
    c("2024-25", "2025-26", "2026-27")
  ))
  # Edge: turn-of-millennium
  stopifnot(identical(
    season_window("2000-01", 2, "back"),
    c("1998-99", "1999-00")
  ))

  # mw_mean
  stopifnot(abs(mw_mean(c(2, 4), c(1, 1)) - 3) < 1e-9)
  stopifnot(abs(mw_mean(c(2, 4), c(1, 3)) - 3.5) < 1e-9)  # weighted toward 4
  stopifnot(is.na(mw_mean(c(NA, NA), c(1, 1))))
  stopifnot(is.na(mw_mean(numeric(0), numeric(0))))

  # Name normalization (must match other scripts)
  stopifnot(normalize_player_name("Jokić") == "jokic")
  stopifnot(normalize_player_name("Jaren Jackson Jr.") == "jaren jackson")

  message("MIS self-test passed.")
}
