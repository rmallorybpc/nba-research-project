suppressPackageStartupMessages({
  library(dplyr)
  library(httr)
  library(readr)
  library(rvest)
  library(stringr)
  library(tibble)
})

# Keep request pacing shared across calls in this R session.
.bbref_fetch_state <- new.env(parent = emptyenv())
.bbref_fetch_state$last_request_ts <- as.numeric(Sys.time()) - 60

BBREF_MIN_DELAY_SECONDS <- 3.5
BBREF_USER_AGENT <- paste(
  "nba-research-project/1.0",
  "(research pipeline; contact: repo owner; respects robots and rate limits)"
)

season_start_year <- function(season) {
  as.integer(substr(season, 1, 4))
}

bbref_transactions_url <- function(season) {
  end_year <- season_start_year(season) + 1L
  sprintf("https://www.basketball-reference.com/leagues/NBA_%d_transactions.html", end_year)
}

bbref_cache_file <- function(season, raw_dir) {
  cache_dir <- file.path(raw_dir, "contracts", "bbref_transactions")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(cache_dir, sprintf("%s.html", season))
}

read_bbref_html_cached <- function(url, cache_file, min_delay_seconds = BBREF_MIN_DELAY_SECONDS) {
  if (!file.exists(cache_file)) {
    elapsed <- as.numeric(Sys.time()) - .bbref_fetch_state$last_request_ts
    wait_for <- min_delay_seconds - elapsed
    if (is.finite(wait_for) && wait_for > 0) {
      Sys.sleep(wait_for)
    }

    resp <- GET(url, user_agent(BBREF_USER_AGENT))
    stop_for_status(resp)
    writeBin(content(resp, as = "raw"), cache_file)
    .bbref_fetch_state$last_request_ts <- as.numeric(Sys.time())
  }

  read_html(cache_file)
}

normalize_colname <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

get_existing_col <- function(df, candidates) {
  for (nm in candidates) {
    if (nm %in% names(df)) {
      return(df[[nm]])
    }
  }
  rep(NA_character_, nrow(df))
}

to_date <- function(x) {
  out <- suppressWarnings(as.Date(x, tryFormats = c(
    "%Y-%m-%d", "%b %d, %Y", "%B %d, %Y", "%m/%d/%Y"
  )))
  out
}

parse_contract_years <- function(note_text) {
  yr <- str_match(note_text, "(?i)\\b(\\d+)\\s*[- ]?year\\b")[, 2]
  suppressWarnings(as.integer(yr))
}

parse_total_value <- function(note_text) {
  val <- str_match(note_text, "(?i)\\$\\s*([0-9]+(?:\\.[0-9]+)?)\\s*(million|m|billion|b)?")[, 2]
  unit <- tolower(str_match(note_text, "(?i)\\$\\s*[0-9]+(?:\\.[0-9]+)?\\s*(million|m|billion|b)?")[, 2])
  num <- suppressWarnings(as.numeric(val))

  multiplier <- ifelse(is.na(unit), 1,
    ifelse(unit %in% c("million", "m"), 1e6,
      ifelse(unit %in% c("billion", "b"), 1e9, 1)
    )
  )

  num * multiplier
}

standardize_team <- function(team_text) {
  out <- trimws(team_text)
  ifelse(out == "", NA_character_, out)
}

parse_player_from_note <- function(note_text) {
  patterns <- c(
    "(?i)^\\s*(?:the\\s+)?[A-Za-z .'-]+\\s+(?:signed|re-?signed)\\s+([A-Za-z .'-]+?)\\s+(?:to|for)\\b",
    "(?i)^\\s*([A-Za-z .'-]+?)\\s+(?:signed|re-?signed)\\s+(?:with|for|to)\\s+(?:the\\s+)?[A-Za-z .'-]+",
    "(?i)^\\s*(?:agreed to terms with|signed)\\s+([A-Za-z .'-]+?)\\b",
    "(?i)^\\s*renegotiated and extended\\s+([A-Za-z .'-]+?)\\b"
  )

  out <- rep(NA_character_, length(note_text))
  for (i in seq_along(patterns)) {
    m <- str_match(note_text, patterns[i])[, 2]
    pick <- is.na(out) & !is.na(m)
    out[pick] <- str_squish(m[pick])
  }
  out
}

classify_contract_kind <- function(note_text) {
  txt <- tolower(note_text)

  case_when(
    str_detect(txt, "renegotiated and extended|renegotiation") ~ "renegotiation",
    str_detect(txt, "contract extension|extension with|agreed to .*extension") ~ "extension",
    str_detect(txt, "two-way contract|two way contract") ~ "two_way",
    str_detect(txt, "10-day contract|ten-day contract|10 day contract|ten day contract") ~ "ten_day",
    str_detect(txt, "exhibit\\s*10") ~ "exhibit_10",
    str_detect(txt, "rookie-scale extension|rookie scale extension") ~ "rookie_extension",
    str_detect(txt, "rookie-scale contract|rookie scale contract") ~ "rookie_scale",
    str_detect(txt, "signed free agent|signed as a free agent|re-signed|resigned") ~ "ufa_signing",
    TRUE ~ NA_character_
  )
}

extract_transactions_table <- function(html) {
  tables <- html_table(html, fill = TRUE)
  if (length(tables) == 0) {
    return(tibble())
  }

  score_table <- function(tbl) {
    nms <- normalize_colname(names(tbl))
    sum(c("date", "team", "notes", "note", "transaction") %in% nms)
  }

  scores <- vapply(tables, score_table, numeric(1))
  best <- tables[[which.max(scores)]]
  names(best) <- normalize_colname(names(best))
  as_tibble(best)
}

# Fetch and parse BBRef transactions into the signing-events contract schema.
fetch_contracts_for_season <- function(season, raw_dir) {
  url <- bbref_transactions_url(season)
  cache_file <- bbref_cache_file(season, raw_dir)
  html <- read_bbref_html_cached(url, cache_file)

  tx <- extract_transactions_table(html)
  if (nrow(tx) == 0) {
    return(tibble(
      player_name = character(),
      signing_date = as.Date(character()),
      signing_team = character(),
      contract_start_season = character(),
      total_value = numeric(),
      contract_years = integer(),
      kind = character()
    ))
  }

  date_text <- get_existing_col(tx, c("date", "transaction_date"))
  team_text <- get_existing_col(tx, c("team", "team_name", "franchise"))
  note_text <- get_existing_col(tx, c("notes", "note", "transaction", "details", "acquired"))
  player_text <- get_existing_col(tx, c("player", "player_name", "acquired"))

  if (all(is.na(note_text))) {
    note_text <- rep("", nrow(tx))
  }

  parsed_player <- parse_player_from_note(note_text)
  player_name <- ifelse(
    !is.na(player_text) & !str_detect(player_text, "(?i)signed|trade|waive|waived|released"),
    player_text,
    parsed_player
  )

  out <- tibble(
    player_name = str_squish(player_name),
    signing_date = to_date(date_text),
    signing_team = standardize_team(team_text),
    contract_start_season = season,
    total_value = parse_total_value(note_text),
    contract_years = parse_contract_years(note_text),
    kind = classify_contract_kind(note_text)
  ) %>%
    filter(!is.na(kind))

  out
}
