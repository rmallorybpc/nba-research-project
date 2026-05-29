# ===============================================================================
# fetch_player_impact.R
#
# Purpose : Implementation of fetch_player_impact() - the fourth data-source
#           fetcher. Pulls a per-player-season impact metric (currently Box
#           Plus-Minus from Basketball Reference's Advanced Stats page),
#           which the MIS script consumes to compute pre/post-signing
#           trajectory deltas.
#
# Metric choice : The launch study uses BPM (Box Plus-Minus) from Basketball
#           Reference. BPM is publicly available, computed back to 1973-74,
#           and serves the same analytical role as Estimated Plus-Minus (EPM)
#           - a single-number impact metric with offense and defense splits.
#           BPM is noisier at the season level than EPM, but for the
#           supermax-reset study's within-cohort comparison the noise mostly
#           cancels and does not bias the supermax coefficient. BPM and EPM
#           correlate at roughly 0.85+ at the player-season level.
#
# Why not EPM : EPM (Dunks and Threes) is a proprietary metric behind a paid
#           subscription. Scraping authenticated pages would create
#           terms-of-service issues that do not apply to BBRef. "We used a
#           publicly available impact metric so the analysis is reproducible"
#           is also a stronger methodological defense than the alternative.
#
# Swapping to EPM later : The output column names of this fetcher are
#           METRIC-AGNOSTIC (impact_overall, impact_offense, impact_defense)
#           specifically so a future EPM fetcher can produce the same shape
#           and the rest of the pipeline does not change. To swap:
#             1. Write a new fetch_player_impact() that pulls EPM and emits
#                the same columns with the same units (per-100-possessions).
#             2. Update the methodology doc.
#             3. Re-run the pipeline. No changes to MIS or analysis scripts.
#
# Primary source : basketball-reference.com/leagues/NBA_{year_end}_advanced.html
#                  One page per season, all players in a single table.
#                  Subject to BBRef's 20-requests-per-minute rate limit; the
#                  fetcher uses the same 3.5s throttle as fetch_contracts.R.
#                  Caches raw HTML so a parse change never costs a refetch.
#
# View-source vs rendered : Same as contracts fetcher - rvest::read_html()
#                  parses the raw HTTP response, NOT a JavaScript-rendered
#                  DOM. BBRef serves its Advanced tables in the initial HTML.
#
# Handling traded players : Basketball Reference shows players traded
#                  mid-season as multiple rows - one "TOT" row aggregating
#                  the season plus one row per team. We keep ONLY the TOT row
#                  (or the single-team row for non-traded players) so the
#                  impact metric reflects the full season, which is what the
#                  pre/post-signing windows need.
# ===============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(rvest)
  library(httr)
  library(tibble)
  library(purrr)
})

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

BBREF_ADVANCED_URL <- "https://www.basketball-reference.com/leagues/NBA_%d_advanced.html"

USER_AGENT_STRING <- paste0(
  "nba-research-project/0.1 ",
  "(behavioral-economics research; ",
  "contact: github.com/rmallorybpc/nba-research-project)"
)

DEFAULT_THROTTLE_SECONDS <- 3.5

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

# MUST match normalize_player_name() in the other five files. Five copies is
# now ridiculous - promote to R/utils/normalize_names.R next refactor.
normalize_player_name <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[.'`]", "") %>%
    str_to_lower() %>%
    str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b", "") %>%
    str_squish()
}

# "2024-25" -> 2025 (BBRef URLs use the season's ending calendar year).
season_to_url_year <- function(season) {
  as.integer(str_sub(season, 1, 4)) + 1L
}

# ------------------------------------------------------------------------------
# Cached fetch - mirrors fetch_contracts.R for consistency
# ------------------------------------------------------------------------------

fetch_with_cache <- function(url, cache_path,
                             throttle_seconds = DEFAULT_THROTTLE_SECONDS,
                             use_cache = TRUE,
                             user_agent = USER_AGENT_STRING) {

  if (use_cache && file.exists(cache_path)) {
    message("  [cache] ", basename(cache_path))
    return(read_html(cache_path))
  }
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)

  Sys.sleep(throttle_seconds)
  message("  [fetch] ", url)
  resp <- GET(url, add_headers(`User-Agent` = user_agent))

  status <- status_code(resp)
  if (status == 429) {
    message("  [429] rate-limited; sleeping 90s and retrying once...")
    Sys.sleep(90)
    resp <- GET(url, add_headers(`User-Agent` = user_agent))
    status <- status_code(resp)
  }
  if (status != 200) {
    stop("HTTP ", status, " from ", url, call. = FALSE)
  }
  writeBin(content(resp, as = "raw"), cache_path)
  read_html(cache_path)
}

# ------------------------------------------------------------------------------
# Parsing - defensive, header-driven
# ------------------------------------------------------------------------------

# Extract the Advanced stats table. Historical id is "advanced_stats"; older
# pages used "advanced". Fall back to scanning for a table with a BPM column.
extract_advanced_table <- function(html) {
  tbl <- html %>% html_element("#advanced_stats")
  if (inherits(tbl, "xml_missing")) {
    tbl <- html %>% html_element("#advanced")
  }
  if (inherits(tbl, "xml_missing")) {
    # Final fallback: scan every <table> for one with a BPM header
    candidates <- html %>% html_elements("table")
    for (t in candidates) {
      headers <- t %>% html_elements("thead th") %>% html_text2()
      if (any(headers == "BPM")) {
        return(html_table(t, header = TRUE))
      }
    }
    stop("Could not locate the Advanced stats table. BBRef may have changed ",
         "the table id - inspect the cached HTML.", call. = FALSE)
  }
  html_table(tbl, header = TRUE)
}

# BBRef inserts "rank" pseudo-header rows every ~20 data rows. They have the
# label "Rk" in the rank cell rather than an integer. Drop these.
drop_repeated_headers <- function(df) {
  rk_col <- names(df)[str_detect(names(df), regex("^Rk$", ignore_case = TRUE))][1]
  if (!is.na(rk_col)) {
    df <- df %>% filter(.data[[rk_col]] != "Rk")
  }
  df
}

# For players traded mid-season, BBRef shows multiple rows. Keep only the row
# that represents the full season:
#   - If a player has a row with Tm == "TOT" (or starts with a digit like "2TM"),
#     keep that row.
#   - Otherwise the player has a single team and we keep that row.
# We also accept the convention that the TOT/multi-team row has the highest
# MP and G, which makes a "max MP" fallback robust.
keep_season_aggregates <- function(df, player_col, team_col, mp_col) {

  # Coerce MP to numeric for the max-MP fallback
  df[[mp_col]] <- suppressWarnings(as.numeric(df[[mp_col]]))

  df %>%
    group_by(.data[[player_col]]) %>%
    mutate(
      is_tot     = .data[[team_col]] %in% c("TOT", "2TM", "3TM", "4TM"),
      is_only    = n() == 1,
      keep_this  = is_only | is_tot
    ) %>%
    # If neither a TOT row nor a single row exists for a player (shouldn't
    # happen but be safe), fall back to the row with max MP.
    mutate(keep_this = keep_this | (.data[[mp_col]] == max(.data[[mp_col]],
                                                           na.rm = TRUE) &
                                    !any(keep_this))) %>%
    filter(keep_this) %>%
    slice(1) %>%   # one row per player even if duplicates somehow remain
    ungroup() %>%
    select(-is_tot, -is_only, -keep_this)
}

# ------------------------------------------------------------------------------
# Per-season fetch
# ------------------------------------------------------------------------------

fetch_one_season_impact <- function(season, raw_dir,
                                    throttle_seconds = DEFAULT_THROTTLE_SECONDS,
                                    use_cache = TRUE) {

  url_year <- season_to_url_year(season)
  cache_path <- file.path(raw_dir, "impact",
                          sprintf("bbref_advanced_%d.html", url_year))
  url <- sprintf(BBREF_ADVANCED_URL, url_year)

  html <- fetch_with_cache(url, cache_path, throttle_seconds, use_cache)
  raw  <- extract_advanced_table(html) %>% drop_repeated_headers()

  col <- function(pattern) {
    hits <- names(raw)[str_detect(names(raw), regex(pattern, ignore_case = TRUE))]
    if (length(hits) == 0) {
      stop("Expected column /", pattern, "/ not found in BBRef advanced ",
           "table for ", season, ". Got: ",
           paste(names(raw), collapse = ", "), call. = FALSE)
    }
    hits[[1]]
  }

  player_col  <- col("^Player$")
  team_col    <- col("^Tm$|^Team$")
  g_col       <- col("^G$|^Games$")
  mp_col      <- col("^MP$|Minutes")
  bpm_col     <- col("^BPM$")
  obpm_col    <- col("^OBPM$")
  dbpm_col    <- col("^DBPM$")

  agg <- keep_season_aggregates(raw, player_col, team_col, mp_col)

  out <- tibble(
    player_name      = agg[[player_col]],
    season           = season,
    impact_overall   = suppressWarnings(as.numeric(agg[[bpm_col]])),
    impact_offense   = suppressWarnings(as.numeric(agg[[obpm_col]])),
    impact_defense   = suppressWarnings(as.numeric(agg[[dbpm_col]])),
    minutes_played   = suppressWarnings(as.integer(agg[[mp_col]])),
    games_played     = suppressWarnings(as.integer(agg[[g_col]])),
    metric_source    = "bpm_basketball_reference"
  )

  # Drop rows that failed to parse to numeric impact - these are almost always
  # data quality issues (rookies who played zero minutes, traded mid-season
  # players whose TOT row is malformed, etc.).
  out <- out %>% filter(!is.na(impact_overall),
                        !is.na(minutes_played),
                        minutes_played > 0)

  message("  [parsed] ", nrow(out), " player rows for ", season)
  out
}

# ------------------------------------------------------------------------------
# Main fetcher
# ------------------------------------------------------------------------------

fetch_player_impact <- function(seasons, raw_dir,
                                throttle_seconds = DEFAULT_THROTTLE_SECONDS,
                                use_cache = TRUE) {

  message("\n  fetching impact metric for ", length(seasons), " seasons ",
          "(cached: instant; cold: ~",
          round(length(seasons) * (throttle_seconds + 1)), "s)...")

  map_dfr(seasons, function(s) {
    fetch_one_season_impact(s, raw_dir, throttle_seconds, use_cache)
  })
}

# ------------------------------------------------------------------------------
# Self-test (parser helpers only - does NOT hit the network)
# ------------------------------------------------------------------------------

if (FALSE) {
  # Season-to-URL conversion
  stopifnot(season_to_url_year("2023-24") == 2024L)
  stopifnot(season_to_url_year("2016-17") == 2017L)
  stopifnot(season_to_url_year("1999-00") == 2000L)

  # keep_season_aggregates against a synthetic frame mirroring BBRef's shape
  # for a traded player
  fake <- tibble(
    Player = c("Player A", "Player B", "Player B", "Player B"),
    Tm     = c("BOS",      "TOT",      "BOS",      "NYK"),
    MP     = c("1800",     "1500",     "900",      "600"),
    BPM    = c("3.5",      "2.1",      "2.5",      "1.5")
  )
  out <- keep_season_aggregates(fake, "Player", "Tm", "MP")
  stopifnot(nrow(out) == 2)
  stopifnot(out$Tm[out$Player == "Player B"] == "TOT")   # kept the TOT row
  stopifnot(out$Tm[out$Player == "Player A"] == "BOS")   # single-team kept

  # Name normalization (must match the other five fetcher/script copies)
  stopifnot(normalize_player_name("Jokic") == "jokic")
  stopifnot(normalize_player_name("Jaren Jackson Jr.") == "jaren jackson")

  message("fetch_player_impact self-test passed.")
}
