# NBA Awards Reference - Supermax Eligibility Inputs

This file supports `R/03_features/02_classify_contract_types.R`. The CBA thresholds
file encodes the *rules* for supermax (Designated Veteran) eligibility. Confirming
that a given contract actually *is* a supermax requires knowing which players met
the performance trigger - an All-NBA selection, MVP, or Defensive Player of the
Year in the qualifying window. This file is that lookup, in `nba_awards.csv`.

## Coverage

The table covers twelve seasons, 2013-14 through 2024-25, and is complete: all
three All-NBA teams (fifteen players) for every season, plus the MVP and DPOY for
every season. There are no outstanding gaps. Total rows: 204.

## Why the window extends before 2016-17

Supermax eligibility looks back at honors earned in the most recent season, or in
two of the three most recent seasons, relative to when the contract is executed. A
contract signed in the summer of 2016 (the start of the signing study window) can
qualify on honors from as far back as 2013-14. The awards table therefore begins
three seasons before the signing window opens. The signing study window is
2016-17 through 2024-25; the awards lookback reaches back to 2013-14, which is the
earliest season the table needs.

## CSV structure

`nba_awards.csv` has one row per award-player-season:

- `season` - NBA season in "YYYY-YY" form (e.g. 2018-19).
- `award` - one of MVP, DPOY, ALL_NBA.
- `team_level` - 1, 2, or 3 for All-NBA team level; blank for MVP and DPOY.
- `player` - player name as it appears in official NBA releases.

The classification script joins this against the signing-event table on
normalized player name and the relevant lookback seasons to set the
supermax-eligible flag. Name-matching is the join risk - accented versus
unaccented spellings and suffixes (Jr.) must be normalized on both sides before
joining. The script's
`normalize_player_name()` handles this.

## Eligibility rule encoded by the consumer script

A player qualifies for a Designated Veteran (supermax) contract if, in the
qualifying window relative to the signing, ANY of the following hold:

- All-NBA (any team level) in the most recent season, OR in two of the three most
  recent seasons;
- MVP in any of the three most recent seasons;
- DPOY in the most recent season, OR in two of the three most recent seasons.

The same award triggers drive the Rose Rule bump (0-6 YOS player moving from the
25% tier to the 30% tier).

## Classification note

A 10+ YOS player who signs at 35% of the cap is on a standard max, not a
supermax - the supermax is specifically the mechanism that lets a 7-9 YOS player
reach 35% early. The awards lookup determines whether a 7-9 YOS player *qualified*
for that early bump. The script requires all three conditions (7-9 YOS, incumbent
team, qualifying award in the lookback window) before tagging
`re_signed_supermax`.

## Spot-check against real cases

The eligibility logic was verified against known supermax-era cases: Russell
Westbrook and Stephen Curry (2017 supermax, MVP/All-NBA triggers), John Wall
(2017 supermax, All-NBA most-recent), and Rudy Gobert (DPOY two-of-three). The
2022-23 selections of Jayson Tatum and Jaylen Brown - widely reported at the time
as locking in their respective designated-veteran extensions - also align with
the rule as encoded.

## Sources

MVP and DPOY: official NBA.com year-by-year award histories, corroborated by FOX
Sports and contemporaneous league releases. All-NBA teams: official NBA press
releases (pr.nba.com) and the NBA.com year-by-year All-NBA history, season by
season, corroborated where needed by ESPN, CBS Sports, and NBC.
