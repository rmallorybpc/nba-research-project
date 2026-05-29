# ===============================================================================
# fetch_season_rosters.R
#
# Purpose : Implementation of fetch_season_rosters() - the third of the three
#           data-source fetchers that 01_build_signing_events.R relies on.
#           Resolves which team each player was on at the END of each season,
#           which is the field the main ingest uses for prior_team when
#           classifying free agency events.
#
# Primary source : hoopR::load_nba_player_box(seasons), which pulls a single
#                  bulk file per season from the SportsDataverse data repo -
#                  one row per player-game, including the team. This is much
#                  faster and more reliable than calling nba_commonteamroster()
#                  30 times per season against the NBA Stats API, and it
#                  naturally captures mid-season trades because each game
#                  carries the player's team that day.
#
# Important design choice : "primary_team" here means the team the player was
#                  on AT SEASON'S END - specifically the team of their last
#                  game played that season - not the team they logged the most
#                  games for (modal). This differs from the original stub
#                  docstring and is the right behavior for prior-team
#                  resolution: a player traded at the deadline to team B who
#                  did not re-sign there in the offseason still has team B as
#                  their prior_team, even if team A had them for more games.
#                  The classifier's incumbent flag asks "did the team that
#                  held this player's rights at contract expiry re-sign them?"
#                  which is the last-team question, not the modal-team one.
#                  games_played is carried for QA but not used by the
#                  classifier.
#
# Caching : One RDS per season (~10-30 MB each). Historical seasons are stable;
#           the current in-progress season may want shorter cache TTL. The
#           default never expires - set use_cache = FALSE to force a refresh.
#
# Rate limiting : The SportsDataverse data repo serves static files (CDN-style)
#                 and does not have the same rate-limit profile as the NBA
#                 Stats API. A short 2s throttle between season downloads is
#                 conservative and polite.
# ===============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(purrr)
  library(tibble)
  library(lubridate)
  # hoopR is required at runtime; load explicitly with hoopR:: prefix.
})

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

DEFAULT_BOX_THROTTLE_SECONDS <- 2

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

# MUST match normalize_player_name() in the other three files (classifier,
# main ingest, fetch_player_metadata). Same fragility caveat - factor into a
# shared utils file when you do the next refactor pass.
normalize_player_name <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[.'`]", "") %>%
    str_to_lower() %>%
    str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b", "") %>%
    str_squish()
}

# hoopR's load_nba_player_box() takes seasons as integer START years. Our
# pipeline uses "YYYY-YY" strings. Convert.
season_label_to_start_year <- function(season) {
  as.integer(str_sub(season, 1, 4))
}

# Inverse, for reporting.
start_year_to_season_label <- function(start_year) {
  sprintf("%d-%02d", start_year, (start_year + 1) %% 100)
}

# Normalize an ESPN/hoopR team abbreviation to the canonical BBRef code that
# the contracts fetcher uses. ESPN uses some different codes (NO for Pelicans,
# UTAH instead of UTA, GS for Warriors). Align so the downstream
# (signing_team == prior_team) comparison works without surprise mismatches.
CANONICAL_TEAMS <- c("ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET",
                     "GSW","HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN",
                     "NOP","NYK","OKC","ORL","PHI","PHX","POR","SAC","SAS",
                     "TOR","UTA","WAS")
ESPN_TO_BBREF <- c(
  "GS"   = "GSW",  "NO"   = "NOP",  "UTAH" = "UTA",
  "NY"   = "NYK",  "SA"   = "SAS",  "PHO"  = "PHX",
  "BKN"  = "BKN",  "BRK"  = "BKN",  "NJ"   = "BKN",  "NJN" = "BKN",
  "CHO"  = "CHA",  "NOH"  = "NOP",  "NOK"  = "NOP",
  "WSH"  = "WAS"
)
normalize_team_abbrev <- function(team) {
  if (is.na(team)) return(NA_character_)
  team <- as.character(team)
  if (team %in% CANONICAL_TEAMS) return(team)
  if (team %in% names(ESPN_TO_BBREF)) return(unname(ESPN_TO_BBREF[team]))
  warning("Unrecognized team abbrev: ", team, " - preserving raw value.")
  team
}

# ------------------------------------------------------------------------------
# Per-season box score loader (cached)
# ------------------------------------------------------------------------------

fetch_one_season_box <- function(start_year, raw_dir,
                                 throttle_seconds = DEFAULT_BOX_THROTTLE_SECONDS,
                                 use_cache = TRUE) {

  cache_path <- file.path(raw_dir, "player_box",
                          sprintf("nba_player_box_%d.rds", start_year))

  if (use_cache && file.exists(cache_path)) {
    message("  [cache] ", basename(cache_path))
    return(readRDS(cache_path))
  }
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)

  Sys.sleep(throttle_seconds)
  message("  [fetch] load_nba_player_box(", start_year, ")")

  df <- tryCatch(
    hoopR::load_nba_player_box(seasons = start_year),
    error = function(e) {
      message("  [error] season ", start_year, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(df) || nrow(df) == 0) {
    stop("load_nba_player_box returned no rows for season starting ",
         start_year, ". Possible causes: season not yet available, ",
         "network failure, or hoopR repo path change.", call. = FALSE)
  }
  saveRDS(df, cache_path)
  message("  [cached] ", nrow(df), " player-game rows to ", basename(cache_path))
  df
}

# ------------------------------------------------------------------------------
# Aggregate player-game rows to one row per (season, player) with last team
#
# Logic:
#   - For each player in the season, find the LAST game played by date
#   - The team on that last game is the primary_team (= team at season's end)
#   - games_played is the total across all teams (carried for QA)
#   - regular season only - playoff team affiliation is the same as regular
#     season-end, so filtering to RS gives one clean signal
#
# hoopR's player box schema (verify column names at runtime; ESPN naming):
#   athlete_id, athlete_display_name, team_abbreviation, game_date,
#   season_type (1 = pre, 2 = regular, 3 = post), did_not_play (logical)
# ------------------------------------------------------------------------------

derive_primary_team <- function(box, season_label) {

  required <- c("athlete_id", "athlete_display_name",
                "team_abbreviation", "game_date", "season_type")
  missing <- setdiff(required, names(box))
  if (length(missing) > 0) {
    stop("Player box is missing expected columns: ",
         paste(missing, collapse = ", "),
         "\n  Got: ", paste(names(box), collapse = ", "),
         "\n  hoopR schema may have changed - inspect cached RDS to confirm.",
         call. = FALSE)
  }

  # Filter to regular season; treat NA did_not_play as "played" (column may not
  # exist in older seasons).
  rs <- box %>% filter(.data$season_type == 2)
  if ("did_not_play" %in% names(rs)) {
    rs <- rs %>% filter(!isTRUE(.data$did_not_play))
  }

  rs %>%
    mutate(
      game_date = as.Date(.data$game_date),
      team_norm = vapply(.data$team_abbreviation,
                         normalize_team_abbrev, character(1))
    ) %>%
    group_by(.data$athlete_id, .data$athlete_display_name) %>%
    arrange(.data$game_date, .by_group = TRUE) %>%
    summarise(
      last_game_date  = max(.data$game_date, na.rm = TRUE),
      primary_team    = .data$team_norm[which.max(.data$game_date)],
      games_played    = n_distinct(.data$game_date),
      n_teams         = n_distinct(.data$team_norm),
      .groups         = "drop"
    ) %>%
    transmute(
      season       = season_label,
      player_id    = as.character(.data$athlete_id),
      player_name  = .data$athlete_display_name,
      name_norm    = normalize_player_name(.data$athlete_display_name),
      primary_team = .data$primary_team,
      games_played = as.integer(.data$games_played),
      mid_season_trade = .data$n_teams > 1,
      last_game_date   = .data$last_game_date
    )
}

# ------------------------------------------------------------------------------
# Main fetcher - replaces the stub in 01_build_signing_events.R
# ------------------------------------------------------------------------------

fetch_season_rosters <- function(seasons, raw_dir,
                                 throttle_seconds = DEFAULT_BOX_THROTTLE_SECONDS,
                                 use_cache = TRUE) {

  if (!requireNamespace("hoopR", quietly = TRUE)) {
    stop("hoopR is required. install with: ",
         "if (!requireNamespace('pak', quietly = TRUE)) install.packages('pak'); ",
         "pak::pak('sportsdataverse/hoopR')", call. = FALSE)
  }

  start_years <- vapply(seasons, season_label_to_start_year, integer(1))

  message("\n  loading box scores for ", length(seasons), " seasons ",
          "(cached: instant; cold: ~",
          length(seasons) * (throttle_seconds + 5), "s)...")

  rosters <- map2_dfr(
    start_years, seasons,
    function(start_year, label) {
      box <- fetch_one_season_box(start_year, raw_dir,
                                  throttle_seconds, use_cache)
      derive_primary_team(box, label)
    }
  )

  trade_count <- sum(rosters$mid_season_trade)
  message("  rosters built: ", nrow(rosters), " (player, season) rows; ",
          trade_count, " mid-season trades captured.")

  rosters
}

# ------------------------------------------------------------------------------
# Self-test (helpers only - does NOT hit the network)
# ------------------------------------------------------------------------------

if (FALSE) {
  # Season label conversion
  stopifnot(season_label_to_start_year("2024-25") == 2024L)
  stopifnot(season_label_to_start_year("1999-00") == 1999L)
  stopifnot(start_year_to_season_label(2024) == "2024-25")
  stopifnot(start_year_to_season_label(1999) == "1999-00")

  # Team normalization - covers the ESPN-vs-BBRef mismatches
  stopifnot(normalize_team_abbrev("GS")   == "GSW")
  stopifnot(normalize_team_abbrev("UTAH") == "UTA")
  stopifnot(normalize_team_abbrev("NO")   == "NOP")
  stopifnot(normalize_team_abbrev("BOS")  == "BOS")
  stopifnot(normalize_team_abbrev("BRK")  == "BKN")
  stopifnot(is.na(normalize_team_abbrev(NA_character_)))

  # Name normalization (must match other three files)
  stopifnot(normalize_player_name("Jokić") == "jokic")
  stopifnot(normalize_player_name("Jaren Jackson Jr.") == "jaren jackson")

  # derive_primary_team logic on a synthetic frame
  fake_box <- tibble(
    athlete_id           = c(1,1,1, 2,2),
    athlete_display_name = c("A","A","A", "B","B"),
    team_abbreviation    = c("BOS","BOS","NYK", "LAL","LAL"),
    game_date            = as.Date(c("2024-11-01","2024-12-15","2025-02-20",
                                     "2024-10-25","2024-11-30")),
    season_type          = c(2,2,2, 2,2)
  )
  out <- derive_primary_team(fake_box, "2024-25")
  stopifnot(nrow(out) == 2)
  # Player A was traded mid-season; primary_team should be NYK (last game)
  a <- out %>% filter(player_name == "A")
  stopifnot(a$primary_team == "NYK", a$mid_season_trade == TRUE,
            a$games_played == 3L)
  # Player B never moved; primary_team = LAL
  b <- out %>% filter(player_name == "B")
  stopifnot(b$primary_team == "LAL", b$mid_season_trade == FALSE)

  message("fetch_season_rosters self-test passed.")
}
