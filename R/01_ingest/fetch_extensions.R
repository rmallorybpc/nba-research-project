# ============================================================================== 
# fetch_extensions.R
#
# Purpose : Loads curated veteran extensions from nba_extensions.csv and emits
#           rows in the same shape as fetch_contracts_for_season(). The main
#           ingest unions the two sources to build the complete signing-event
#           population.
#
# Why curated, not scraped : Veteran extensions are a small finite set (~12
#           supermax-class signings in the 2016-17 through 2024-25 study
#           window). Curating from the authoritative Hoops Rumors Designated
#           Veteran tracker is cleaner than building a scraper for a category
#           where the underlying truth is small enough to enumerate.
#
# Source   : Hoops Rumors, "Players Who Have Signed Designated Veteran
#            Contracts" - a maintained canonical list updated through 2025.
#            URL recorded in each row's source_url column for audit. Stephen
#            Curry's 2017 deal is intentionally OMITTED because it was a free
#            agent contract (captured by the BBRef FA tracker), not an
#            extension. SGA's 2025 extension is OMITTED because it falls
#            outside the launch study window.
#
# Scope    : The launch study targets supermax/Designated Veteran extensions
#            specifically. Non-supermax veteran extensions (e.g. Pascal
#            Siakam's 2019 rookie-scale extension graduate, or any standard
#            non-DVE veteran extension) are out of scope for the launch and
#            can be added in a follow-up curation pass when the analysis
#            calls for a broader incumbent-re-sign comparison group.
#
# Edge case worth knowing : Rudy Gobert's 2020 extension is a Designated
#            Veteran contract but started at approximately 31.4% of cap, not
#            the full 35% supermax. The is_designated_veteran flag is TRUE
#            but supermax_full_35pct is FALSE. The classifier's max-tier check
#            will not match it as "max_35" on dollar value, and the awkward-
#            case classification_flag machinery will surface it for review.
#            Carrying both flags lets downstream analysis treat it as either
#            "supermax cohort" or "non-supermax DVE cohort" depending on the
#            question being asked.
# ============================================================================== 

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

DEFAULT_EXTENSIONS_PATH <- "data/processed/nba_extensions.csv"

# Columns required by 01_build_signing_events.R from any contracts source.
CONTRACTS_OUTPUT_COLS <- c(
  "player_name", "signing_date", "signing_team",
  "contract_start_season", "total_value", "contract_years",
  "kind", "prior_team_bbref", "note_raw"
)

# Extra columns specific to extensions, carried through for downstream use.
EXTENSIONS_EXTRA_COLS <- c(
  "is_designated_veteran", "supermax_full_35pct",
  "qualifying_basis", "source_url"
)

# ------------------------------------------------------------------------------
# Main loader - drop-in companion to fetch_contracts_for_season()
# ------------------------------------------------------------------------------

# season filter is optional. If provided as a vector of "YYYY-YY" labels, only
# extensions whose signing_offseason_year matches the start year of any label
# are returned (consistent with the inclusion filter in 01_build_signing_events.R,
# which keeps extensions signed inside the study window even when contract
# start falls outside it). If NULL, all curated extensions are returned.
fetch_extensions <- function(path = DEFAULT_EXTENSIONS_PATH,
                             seasons = NULL) {

  if (!file.exists(path)) {
    stop("Extensions CSV not found at ", path,
         ". This file is curated, not fetched - see fetch_extensions.R header ",
         "for source and update procedure.", call. = FALSE)
  }

  ext <- read_csv(
    path,
    col_types = cols(
      player_name           = col_character(),
      signing_date          = col_date(format = "%Y-%m-%d"),
      signing_team          = col_character(),
      prior_team_bbref      = col_character(),
      contract_start_season = col_character(),
      total_value           = col_double(),
      contract_years        = col_integer(),
      kind                  = col_character(),
      is_designated_veteran = col_logical(),
      supermax_full_35pct   = col_logical(),
      qualifying_basis      = col_character(),
      note_raw              = col_character(),
      source_url            = col_character()
    )
  )

  # Schema sanity: fail loudly if the CSV is missing a required field.
  missing <- setdiff(CONTRACTS_OUTPUT_COLS, names(ext))
  if (length(missing) > 0) {
    stop("Extensions CSV is missing required columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  # Optional season filter - by signing year, not contract start year.
  if (!is.null(seasons)) {
    keep_years <- as.integer(str_sub(seasons, 1, 4))
    ext <- ext %>%
      filter(as.integer(format(.data$signing_date, "%Y")) %in% keep_years)
  }

  message("  [extensions] ", nrow(ext), " curated rows loaded from ", path)

  # Return columns in the order the main ingest expects, with the extras
  # tacked on at the end.
  ext %>% select(any_of(c(CONTRACTS_OUTPUT_COLS, EXTENSIONS_EXTRA_COLS)))
}

# ------------------------------------------------------------------------------
# Self-test (parser only - does NOT hit the network)
# Run interactively against the curated CSV after edits to confirm shape.
# ------------------------------------------------------------------------------

if (FALSE) {
  ext <- fetch_extensions()
  stopifnot(nrow(ext) >= 12)
  stopifnot(all(ext$kind == "extension"))
  stopifnot(all(ext$signing_team == ext$prior_team_bbref))  # all incumbent
  stopifnot(all(ext$is_designated_veteran))
  stopifnot(sum(!ext$supermax_full_35pct) == 1)  # Gobert is the only partial
  # Season filter
  ext_2022 <- fetch_extensions(seasons = "2022-23")
  stopifnot(all(format(ext_2022$signing_date, "%Y") == "2022"))
  message("fetch_extensions self-test passed.")
}