# ===============================================================================
# fetch_player_metadata.R
#
# Purpose : Implementation of fetch_player_metadata() — the second of the three
#           data-source fetchers that 01_build_signing_events.R relies on.
#           Pulls per-player bio fields (birth date, draft year, primary
#           position) and the per-season participation history that drives the
#           years-of-service computation, from the NBA Stats API via hoopR.
#
# Primary source : hoopR's nba_commonplayerinfo(player_id), which returns a
#                  named list with three data frames:
#                    - CommonPlayerInfo   (birth date, draft year, position)
#                    - PlayerHeadlineStats (ignored)
#                    - AvailableSeasons   (one row per season the player was on
#                                          an NBA roster — this is the goldmine
#                                          for active_seasons)
#                  Player IDs are resolved from names via nba_commonallplayers().
#
# View-source vs API : This fetcher calls the NBA Stats API, not a scraped HTML
#                      page. Same view-source principle as the BBRef fetcher
#                      still applies in spirit — we trust the response payload
#                      directly, do not depend on JavaScript rendering, and
#                      cache the raw response (parsed by hoopR to a data frame)
#                      before any of our own processing.
#
# Rate-limit caution : stats.nba.com is much more aggressive than BBRef. Missing
#                      User-Agent is the most common cause of silent failures
#                      (returns empty data rather than 429). hoopR sets one
#                      internally; we use a longer throttle than BBRef (6s) to
#                      be safe across ~200-300 calls per full run.
#
# Caching : Two layers. The player-list lookup (large, slow, rarely changes) is
#           cached once per quarter. Each player's commonplayerinfo response is
#           cached by player_id forever — bio facts don't change, and a player
#           who appears in multiple offseasons of our study is fetched once.
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
  # hoopR is required at runtime; we don't library() it here so the file can
  # be sourced for parse-checking without hoopR installed. Calls below use the
  # `hoopR::` prefix explicitly.
})

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

# Sleep between NBA Stats API requests. 6s is well below their unspecified but
# notoriously low ceiling; hoopR's internal handling plus this gives margin.
DEFAULT_NBA_THROTTLE_SECONDS <- 6

# Player list cache duration. Names/IDs rarely change but new players appear
# each season; refresh quarterly.
PLAYER_LIST_CACHE_DAYS <- 90

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

# MUST match normalize_player_name() in 02_classify_contract_types.R and in
# 01_build_signing_events.R. Three copies is fragile; if you change one, change
# all three, or factor into a shared utils file.
normalize_player_name <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[.'`]", "") %>%
    str_to_lower() %>%
    str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b", "") %>%
    str_squish()
}

# NBA Stats season IDs come in two formats depending on the endpoint:
#   - "22020" (5 chars, type+year), where 2 = regular season, 2020 = start year
#   - "2020-21" (already in label form)
# Normalize both to our canonical "YYYY-YY" label.
parse_nba_season_id <- function(x) {
  if (is.na(x) || x == "") return(NA_character_)
  x <- as.character(x)
  if (str_detect(x, "^\\d{4}-\\d{2}$")) return(x)
  if (str_detect(x, "^[1-5]\\d{4}$")) {
    start_year <- as.integer(str_sub(x, 2, 5))
    return(sprintf("%d-%02d", start_year, (start_year + 1) %% 100))
  }
  warning("Unrecognized NBA season id format: ", x)
  NA_character_
}

# Position normalization. NBA Stats returns codes like "G", "F", "C", "G-F",
# "F-C", or sometimes "PG"/"SG"/"SF"/"PF". For research purposes the granular
# distinction usually matters less than guard/wing/big. Return the FIRST listed
# position normalized to a single letter group; carry the raw value too.
normalize_position <- function(pos) {
  if (is.na(pos) || pos == "") return(NA_character_)
  first <- str_split(pos, "-")[[1]][1]
  case_when(
    first %in% c("PG", "SG", "G") ~ "G",
    first %in% c("SF", "PF", "F") ~ "F",
    first %in% c("C") ~ "C",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------------------------------------
# Player list fetcher — name → ID lookup
# Cached per quarter. Returns a tibble (player_id, full_name, name_norm,
# from_year, to_year) covering every player in NBA history (active + historical).
# ------------------------------------------------------------------------------

fetch_player_list <- function(raw_dir,
                              max_age_days = PLAYER_LIST_CACHE_DAYS) {

  cache_path <- file.path(raw_dir, "player_list", "nba_commonallplayers.rds")
  use_cache <- file.exists(cache_path) &&
    difftime(Sys.time(), file.info(cache_path)$mtime, units = "days") <
      max_age_days

  if (use_cache) {
    message("  [cache] player list (", basename(cache_path), ")")
    return(readRDS(cache_path))
  }

  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  message("  [fetch] nba_commonallplayers (all NBA players, this is slow)")

  # is_only_current_season = 0 returns all-time players. League id "00" = NBA.
  resp <- hoopR::nba_commonallplayers(is_only_current_season = 0,
                                      league_id = "00")
  # hoopR returns a named list; the player table key varies by version but
  # is typically "CommonAllPlayers" or the function returns the data frame
  # directly. Handle both shapes.
  df <- if (is.data.frame(resp)) resp
  else if ("CommonAllPlayers" %in% names(resp)) resp$CommonAllPlayers
  else stop("Unexpected nba_commonallplayers return shape: ",
            paste(names(resp), collapse = ", "), call. = FALSE)

  out <- df %>%
    transmute(
      player_id = as.character(.data$PERSON_ID),
      full_name = .data$DISPLAY_FIRST_LAST,
      name_norm = normalize_player_name(.data$DISPLAY_FIRST_LAST),
      from_year = suppressWarnings(as.integer(.data$FROM_YEAR)),
      to_year = suppressWarnings(as.integer(.data$TO_YEAR))
    )

  saveRDS(out, cache_path)
  message("  [cached] ", nrow(out), " players to ", cache_path)
  out
}

# Resolve a list of player names to player IDs against the cached player list.
# Returns a tibble (input_name, signing_year, player_id, resolution_status)
# where resolution_status is one of:
#   "unique"       — exactly one historical match
#   "by_career"    — multiple matches; one career window overlaps the signing
#   "ambiguous"    — multiple matches and overlap was inconclusive
#   "missing"      — no match found
# The caller decides whether to drop, flag, or hand-resolve ambiguous rows.
resolve_player_ids <- function(player_names, signing_years, player_list) {

  stopifnot(length(player_names) == length(signing_years))

  request <- tibble(
    request_id = seq_along(player_names),
    input_name = player_names,
    signing_year = as.integer(signing_years),
    name_norm = normalize_player_name(player_names)
  )

  matches <- request %>%
    left_join(player_list, by = "name_norm", relationship = "many-to-many")

  matches %>%
    group_by(request_id, input_name, signing_year) %>%
    summarise(
      n_matches = sum(!is.na(player_id)),
      ids = list(unique(player_id[!is.na(player_id)])),
      overlap = list(tibble(
        player_id, from_year, to_year
      )[!is.na(player_id) &
        signing_year[1] >= from_year - 1L &
        signing_year[1] <= to_year + 1L, ]),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      resolution_status = case_when(
        n_matches == 0 ~ "missing",
        n_matches == 1 ~ "unique",
        nrow(overlap) == 1 ~ "by_career",
        TRUE ~ "ambiguous"
      ),
      player_id = case_when(
        resolution_status == "unique" ~ ids[[1]][1],
        resolution_status == "by_career" ~ overlap$player_id[1],
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup() %>%
    select(input_name, signing_year, player_id, resolution_status)
}

# ------------------------------------------------------------------------------
# Per-player metadata fetcher (cached per player_id forever — bio facts don't
# change. A player who appears in multiple offseasons gets fetched once.)
# ------------------------------------------------------------------------------

fetch_one_player <- function(player_id, raw_dir,
                             throttle_seconds = DEFAULT_NBA_THROTTLE_SECONDS,
                             use_cache = TRUE) {

  cache_path <- file.path(raw_dir, "player_info",
                          sprintf("%s.rds", player_id))

  if (use_cache && file.exists(cache_path)) {
    return(readRDS(cache_path))
  }
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)

  Sys.sleep(throttle_seconds)
  message("  [fetch] commonplayerinfo player_id=", player_id)

  resp <- tryCatch(
    hoopR::nba_commonplayerinfo(player_id = player_id, league_id = "00"),
    error = function(e) {
      message("  [error] player_id=", player_id, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(resp)) return(NULL)
  saveRDS(resp, cache_path)
  resp
}

# Pull the three fields out of a commonplayerinfo response into a flat tibble
# row. Defensive against missing columns and unexpected shapes.
flatten_player_response <- function(resp, player_id, input_name) {

  if (is.null(resp)) {
    return(tibble(
      player_id = player_id,
      player_name = input_name,
      birth_date = as.Date(NA),
      draft_year = NA_integer_,
      primary_position = NA_character_,
      primary_position_raw = NA_character_,
      first_nba_season = NA_character_,
      active_seasons = list(character(0))
    ))
  }

  cpi <- resp$CommonPlayerInfo
  avs <- resp$AvailableSeasons

  birth <- tryCatch(as.Date(str_sub(cpi$BIRTHDATE[1], 1, 10)),
                    error = function(e) as.Date(NA))
  draft <- suppressWarnings(as.integer(cpi$DRAFT_YEAR[1]))
  pos_raw <- as.character(cpi$POSITION[1])
  seasons <- if (!is.null(avs) && nrow(avs) > 0) {
    vapply(avs$SEASON_ID, parse_nba_season_id, character(1))
  } else {
    character(0)
  }
  seasons <- sort(unique(seasons[!is.na(seasons)]))
  first <- if (length(seasons) > 0) seasons[1] else NA_character_

  tibble(
    player_id = player_id,
    player_name = input_name,
    birth_date = birth,
    draft_year = draft,
    primary_position = normalize_position(pos_raw),
    primary_position_raw = pos_raw,
    first_nba_season = first,
    active_seasons = list(seasons)
  )
}

# ------------------------------------------------------------------------------
# Main fetcher — replaces the stub in 01_build_signing_events.R
# ------------------------------------------------------------------------------

# player_names_with_year: tibble with columns (player_name, signing_year).
# Returns one row per input row. Players that fail to resolve get rows with
# NA bio fields and a non-"unique" status, surfaced for review rather than
# silently dropped.
fetch_player_metadata <- function(player_names_with_year, raw_dir,
                                  throttle_seconds = DEFAULT_NBA_THROTTLE_SECONDS,
                                  use_cache = TRUE) {

  if (!requireNamespace("hoopR", quietly = TRUE)) {
    stop("hoopR is required. install with: ",
         "if (!requireNamespace('pak', quietly = TRUE)) install.packages('pak'); ",
         "pak::pak('sportsdataverse/hoopR')", call. = FALSE)
  }

  if (!all(c("player_name", "signing_year") %in% names(player_names_with_year))) {
    stop("player_names_with_year must have columns: player_name, signing_year",
         call. = FALSE)
  }

  player_list <- fetch_player_list(raw_dir)

  resolved <- resolve_player_ids(
    player_names_with_year$player_name,
    player_names_with_year$signing_year,
    player_list
  )

  # Report resolution status up front so the caller knows how many rows will
  # have NAs and why.
  status_counts <- resolved %>% count(resolution_status, sort = TRUE)
  message("\n  name resolution:")
  walk2(status_counts$resolution_status, status_counts$n,
        ~ message("    ", .x, ": ", .y))

  unique_ids <- resolved %>%
    filter(!is.na(player_id)) %>%
    distinct(player_id) %>%
    pull(player_id)

  message("\n  fetching info for ", length(unique_ids), " unique players ",
          "(cached: instant; cold: ~",
          round(length(unique_ids) * throttle_seconds / 60), "m)...")

  info_by_id <- map(unique_ids,
                    ~ fetch_one_player(.x, raw_dir, throttle_seconds, use_cache))
  names(info_by_id) <- unique_ids

  # Build one row per resolved request (preserves duplicate signings of the
  # same player across seasons with the same metadata).
  resolved %>%
    pmap_dfr(function(input_name, signing_year, player_id, resolution_status) {
      resp <- if (!is.na(player_id)) info_by_id[[player_id]] else NULL
      flatten_player_response(resp, player_id, input_name) %>%
        mutate(
          signing_year = signing_year,
          resolution_status = resolution_status
        )
    }) %>%
    # Final guard: surface unresolved or fully-NA rows so the caller can
    # decide policy rather than silently emitting bad data downstream.
    {
      bad <- filter(., is.na(birth_date) | is.na(draft_year))
      if (nrow(bad) > 0) {
        message("\n  WARNING: ", nrow(bad), " rows lack complete metadata. ",
                "First few input names: ",
                paste(head(unique(bad$player_name), 5), collapse = ", "))
      }
      .
    }
}

# ------------------------------------------------------------------------------
# Self-test (helpers only — does NOT hit the API)
# ------------------------------------------------------------------------------

if (FALSE) {
  # Season ID parsing
  stopifnot(parse_nba_season_id("22020") == "2020-21")
  stopifnot(parse_nba_season_id("2020-21") == "2020-21")
  stopifnot(parse_nba_season_id("21999") == "1999-00")
  stopifnot(is.na(parse_nba_season_id(NA)))
  # Position normalization
  stopifnot(normalize_position("G") == "G")
  stopifnot(normalize_position("PG") == "G")
  stopifnot(normalize_position("F-C") == "F")
  stopifnot(normalize_position("C") == "C")
  stopifnot(is.na(normalize_position(NA)))
  # Name normalization (must match other scripts)
  stopifnot(normalize_player_name("Jokić") == "jokic")
  stopifnot(normalize_player_name("Jaren Jackson Jr.") == "jaren jackson")
  message("parser self-test passed.")
}
