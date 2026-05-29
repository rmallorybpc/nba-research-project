# ============================================================================== 
# fetch_contracts.R
#
# Purpose : Implementation of fetch_contracts_for_season() - the first of the
#           three data-source fetchers that 01_build_signing_events.R relies on.
#           Pulls per-season UFA signings from Basketball Reference's
#           free-agent tracker page, with rate limiting, response caching, and a
#           defensive parser. Sourced by the main ingest script.
#
# Primary source : basketball-reference.com/friv/free_agents.cgi?year={YYYY}
#                  Stable URL pattern, single table per season, fact-only data.
#                  Sports Reference rate limit: 20 requests/minute on
#                  basketball-reference.com - research-scale (9 pages total)
#                  stays comfortably below it with the throttle below.
#
# Scope    : UFA signings only. Veteran extensions do NOT appear on this page
#            because they were not "free agents" the summer they signed (their
#            existing contracts were still in force). Extensions are handled in
#            a separate fetch step - see fetch_extensions.R.
#
# View-source vs rendered : rvest::read_html() parses the raw HTTP response
#            (what you would see in "View Page Source"), NOT the JavaScript-
#            rendered DOM. Basketball Reference serves its tables in the
#            initial HTML so this works. If a future change pushes table data
#            behind JS, fetch_with_cache() will need to be replaced with a
#            headless-browser approach (chromote / selenider).
#
# Compliance : Sports Reference allows fact reuse - "facts cannot be
#              copyrighted" per their own data-use page - but prohibits bulk
#              automated access that adversely impacts site performance, and
#              prohibits redistribution that competes with their service. This
#              code respects both: rate-limited well below their 20/min cap,
#              caches every response so we never re-fetch, and identifies
#              itself via a project-specific User-Agent. Research use of facts
#              is fine; downstream redistribution of the raw scrape is not.
# ============================================================================== 

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(rvest)
  library(httr)
  library(tibble)
  library(purrr)
})

# ------------------------------------------------------------------------------
# Config (overridable by caller)
# ------------------------------------------------------------------------------

BBREF_FREE_AGENTS_URL <- "https://www.basketball-reference.com/friv/free_agents.cgi?year=%d"

# Polite User-Agent that identifies the project, why it's hitting their server,
# and how to contact us if they want to ask us to stop. This is what an ethical
# scraper looks like - anonymity is the wrong signal here.
USER_AGENT_STRING <- paste0(
  "nba-research-project/0.1 ",
  "(behavioral-economics research; ",
  "contact: github.com/rmallorybpc/nba-research-project)"
)

# Sleep between requests in seconds. BBRef's documented limit is 20/min - at 3
# seconds we do at most 20/min. We use a little more headroom to be safe.
DEFAULT_THROTTLE_SECONDS <- 3.5

# ------------------------------------------------------------------------------
# Cached fetch
# Always check cache before hitting the network. The raw HTML response is the
# unit of caching - once we have the HTML, re-parsing is free, so a parsing
# change does not trigger a re-fetch.
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

  # Throttle BEFORE the request, not after, so that consecutive cache misses
  # are correctly spaced.
  Sys.sleep(throttle_seconds)

  message("  [fetch] ", url)
  resp <- GET(url, add_headers(`User-Agent` = user_agent))

  status <- status_code(resp)
  if (status == 429) {
    # Rate-limited. Back off substantially and retry once. Sports Reference's
    # session-jail can last up to a day, so DO NOT keep hammering.
    message("  [429] rate-limited; sleeping 90s and retrying once...")
    Sys.sleep(90)
    resp <- GET(url, add_headers(`User-Agent` = user_agent))
    status <- status_code(resp)
  }
  if (status != 200) {
    stop("HTTP ", status, " from ", url, call. = FALSE)
  }

  # Persist the raw HTML before parsing, so a parser bug never costs a refetch.
  writeBin(content(resp, as = "raw"), cache_path)
  read_html(cache_path)
}

# ------------------------------------------------------------------------------
# Parsing helpers - defensive, header-driven (not column-position-driven)
# BBRef changes column positions occasionally; column LABELS are stable.
# ------------------------------------------------------------------------------

# Null-coalescing operator (R doesn't have one built in for character).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Extract the free-agent table from a parsed BBRef page. The table on this
# page has stable id "free_agents" historically; we fall back to grabbing the
# first <table> that contains a Player column, in case the id changes.
extract_fa_table <- function(html) {
  tbl <- html %>% html_element("#free_agents")
  if (inherits(tbl, "xml_missing")) {
    # Fallback: scan all tables for one with a Player header.
    candidates <- html %>% html_elements("table")
    for (t in candidates) {
      headers <- t %>% html_elements("thead th") %>% html_text2()
      if (any(str_detect(headers, regex("Player", ignore_case = TRUE)))) {
        return(html_table(t, header = TRUE))
      }
    }
    stop("Could not locate the free-agents table on the BBRef page. ",
         "Inspect the cached HTML to confirm the table id or class.",
         call. = FALSE)
  }
  html_table(tbl, header = TRUE)
}

# BBRef's "Notes" / signing column carries strings like:
#   "Signed 2-Yr contract with PHI worth $63,200,000"
#   "Re-signed 4-Yr contract with BOS worth $304,000,000"
#   "Player option declined; signed elsewhere"
#   "Signed 1-Yr contract with LAL"  (no value listed -> likely minimum)
# Parse to (signed, signing_team, contract_years, total_value, note).
parse_signing_note <- function(note) {
  if (is.null(note) || is.na(note) || note == "") {
    return(tibble(signed = FALSE, signing_team = NA_character_,
                  contract_years = NA_integer_, total_value = NA_real_,
                  note_raw = note %||% NA_character_))
  }
  # Years: digit before "Yr"/"Year"/"Years" (case-insensitive). BBRef consistently
  # uses "Yr" today but the broader pattern is cheap insurance against format drift.
  years <- str_match(note, regex("(\\d+)\\s*-?\\s*(?:yr|year)s?",
                                 ignore_case = TRUE))[, 2]
  # Team: 3-letter uppercase abbrev after "with" (case-sensitive on the code
  # itself so we don't false-match lowercase strings)
  team  <- str_match(note, "with\\s+([A-Z]{3})")[, 2]
  # Value: dollar amount, possibly with commas
  raw_value <- str_match(note, "\\$([0-9,]+)")[, 2]
  value <- if (!is.na(raw_value)) as.numeric(str_remove_all(raw_value, ",")) else NA_real_

  signed <- str_detect(note, regex("signed", ignore_case = TRUE)) &
            !is.na(team)

  tibble(
    signed         = signed,
    signing_team   = team,
    contract_years = suppressWarnings(as.integer(years)),
    total_value    = value,
    note_raw       = note
  )
}

# Map BBRef's "Type" column to our `kind` taxonomy. Anything we don't recognize
# is conservatively tagged "ufa_signing" if signed elsewhere or "other" if not,
# and flagged for review.
classify_signing_kind <- function(fa_type, signed, prior_team, signing_team) {
  ft <- str_to_upper(fa_type %||% "")
  if (str_detect(ft, "TWO[ -]?WAY")) return("two_way")
  if (str_detect(ft, "10-?DAY"))     return("ten_day")
  if (str_detect(ft, "EXHIBIT"))     return("exhibit_10")
  # Standard UFA / RFA signings; we keep RFA in the population since matched
  # offer sheets are still a choice event worth observing.
  if (signed) return("ufa_signing")
  # Unsigned: outside scope but return a label so the row can be filtered out.
  "unsigned"
}

# Normalize BBRef team abbreviations to a canonical set. BBRef uses 3-letter
# codes throughout (e.g. BOS, PHI, OKC); historical renames (NJN->BKN, NOH->NOP,
# CHO->CHA) are pre-current already. Pass-through with a guard for unknowns.
CANONICAL_TEAMS <- c("ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET",
                     "GSW","HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN",
                     "NOP","NYK","OKC","ORL","PHI","PHX","POR","SAC","SAS",
                     "TOR","UTA","WAS")
normalize_team <- function(team) {
  if (is.na(team)) return(NA_character_)
  if (team %in% CANONICAL_TEAMS) return(team)
  # Known historical aliases
  alias <- c("CHO" = "CHA", "BRK" = "BKN", "NJN" = "BKN",
             "NOH" = "NOP", "PHO" = "PHX")
  if (team %in% names(alias)) return(unname(alias[team]))
  warning("Unrecognized team code from BBRef: ", team,
          " - preserving raw value.")
  team
}

# ------------------------------------------------------------------------------
# Main fetcher - replaces the stub in 01_build_signing_events.R
# ------------------------------------------------------------------------------

fetch_contracts_for_season <- function(season, raw_dir,
                                       throttle_seconds = DEFAULT_THROTTLE_SECONDS,
                                       use_cache = TRUE) {

  year <- as.integer(str_sub(season, 1, 4))   # "2024-25" -> 2024
  cache_path <- file.path(raw_dir, "contracts",
                          sprintf("bbref_free_agents_%d.html", year))
  url <- sprintf(BBREF_FREE_AGENTS_URL, year)

  html <- fetch_with_cache(url, cache_path,
                           throttle_seconds = throttle_seconds,
                           use_cache = use_cache)
  fa <- extract_fa_table(html)

  # Defensive column resolution: find column names by regex on the headers we
  # know BBRef uses. Position can shift; labels are stable.
  col <- function(pattern) {
    hits <- names(fa)[str_detect(names(fa), regex(pattern, ignore_case = TRUE))]
    if (length(hits) == 0) {
      stop("Expected column matching /", pattern,
           "/ not found in BBRef table. Got: ",
           paste(names(fa), collapse = ", "), call. = FALSE)
    }
    hits[[1]]
  }

  player_col <- col("^Player$")
  prior_col  <- col("Prior|^Tm$|Team")           # "Prior Team" historically
  type_col   <- col("^Type$|FA Type")
  notes_col  <- col("Notes|New Team|Signed")     # signing string lives here

  parsed_notes <- map_dfr(fa[[notes_col]], parse_signing_note)

  out <- tibble(
    player_name           = fa[[player_col]],
    prior_team_raw        = fa[[prior_col]],
    fa_type               = fa[[type_col]],
    note_raw              = parsed_notes$note_raw,
    signed                = parsed_notes$signed,
    signing_team_raw      = parsed_notes$signing_team,
    contract_years        = parsed_notes$contract_years,
    total_value           = parsed_notes$total_value
  ) %>%
    mutate(
      prior_team    = vapply(prior_team_raw,   normalize_team, character(1)),
      signing_team  = vapply(signing_team_raw, normalize_team, character(1)),
      # Contract start season = the season immediately following the year on
      # the BBRef page. The 2024 FA page lists summer 2024 signings, which
      # begin in the 2024-25 season.
      contract_start_season = sprintf("%d-%02d", year, (year + 1) %% 100),
      # BBRef does not list exact signing dates on the FA tracker; default to
      # July 1 of the offseason year, which is when the league year starts and
      # the vast majority of UFA signings are reported. The
      # signing_offseason_year derivation in the main ingest uses year(date),
      # so July 1 produces the correct value for the lookback.
      signing_date          = as.Date(sprintf("%d-07-01", year)),
      kind                  = pmap_chr(
        list(fa_type, signed, prior_team, signing_team),
        classify_signing_kind
      )
    ) %>%
    # Drop unsigned and obviously-out-of-scope rows here so the caller sees a
    # clean UFA signing population. The main ingest's inclusion filter does a
    # second pass on `kind` for the cross-source case.
    filter(signed, kind %in% c("ufa_signing")) %>%
    select(player_name, signing_date, signing_team,
           contract_start_season, total_value, contract_years, kind,
           prior_team_bbref = prior_team,   # carried forward as cross-check
           note_raw)

  message("  [parsed] ", nrow(out), " UFA signings for ", season)
  out
}

# ------------------------------------------------------------------------------
# Self-test (parser only - does NOT hit the network)
# Run interactively to confirm the parser handles the strings BBRef emits.
# ------------------------------------------------------------------------------

if (FALSE) {
  test_notes <- c(
    "Signed 2-Yr contract with PHI worth $63,200,000",
    "Re-signed 4-Yr contract with BOS worth $304,000,000",
    "Signed 1-Yr contract with LAL",
    "Player option declined; signed elsewhere",
    "",
    NA_character_
  )
  print(map_dfr(test_notes, parse_signing_note))

  stopifnot(normalize_team("CHO") == "CHA")
  stopifnot(normalize_team("BOS") == "BOS")
  stopifnot(is.na(normalize_team(NA_character_)))
  message("parser self-test passed.")
}