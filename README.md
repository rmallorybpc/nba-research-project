# NBA supermax research project

Do supermax contracts make NBA players worse? No. The numbers show a decline,
but the contract does not cause it. Age and mean reversion explain all of it.
This is a single-builder research project. It tests whether a well-documented
behavioral pattern, the fresh-start effect, shows up in NBA contract decisions.
It reaches a rigorous null on its headline question and two clear results
underneath.

Live site: https://rmallorybpc.github.io/nba-research-project/

## The findings

Three questions, three answers, from the same data.
- Do supermax contracts make players worse? No. Supermax players post the worst
raw decline of any group, about -0.63 in impact. But they are old stars coming
off peak seasons. Control for age and pre-signing performance and the supermax
effect is +0.48 and not significant. The contract adds nothing you can measure.
- Does performance regress to the mean? Yes, strongly. The bottom quarter of
players by prior impact rose +1.07. The top quarter fell -0.80. The slope is
-0.25 (p < 0.001, n=2,378). This is the engine behind the supermax mirage.
- Does changing teams help or hurt? It hurts. Players who changed teams declined
by -0.345 relative to players who stayed (p < 0.001, n=835). That runs against
the fresh-start prediction. Some of it is selection in who gets moved.

## How it works

The study covers eight offseasons, 2017 through 2024.

It builds an event for every contributor who appears in consecutive-season
rosters: the team he left, the team he ended on, and whether he stayed, moved,
or signed a supermax. A hand-checked supermax list supplies the supermax group.

For each event it measures the change in minutes-weighted Box Plus-Minus from
the two seasons before the signing to the three seasons after. That change is the

Movement Impact Score. Then it compares the change across the three groups, with
a regression that controls for age and pre-signing performance and clusters
standard errors by player.

The full method is in docs/methodology.md.

## Data sources

- Season rosters: ESPN player box scores via the hoopR package.
- Player impact: Box Plus-Minus from Basketball Reference advanced stats pages.
- Supermax cohort: a hand-checked list from the Hoops Rumors Designated Veteran
tracker, in data/processed/nba_extensions.csv.

All three are public and reproducible.

## Repository layout

- R/01_ingest/ : data fetchers for rosters, impact, the supermax overlay, and a
retired contracts fetcher.
 R/03_features/01_build_signing_events.R : builds the event table from roster
comparison plus the supermax overlay.
- scripts/ : MIS computation and the four analysis scripts.
- data/processed/ : the curated inputs and the analysis output CSVs.
- dashboard/src/ : the site. Plain HTML, one shared nav script, and pages that
- fetch the findings CSVs.
- docs/ : methodology, roadmap, and reference notes.

## Reproducing the analysis

The environment is a pre-built Rocker tidyverse devcontainer. It installs
fixest, sandwich, lmtest, broom, and hoopR on top of the base image.

Run in order:

1. R/03_features/01_build_signing_events.R writes signing_events_classified.csv.
2. Build the consolidated impact file across 2015-16 through 2025-26 and write
nba_player_impact.csv.
3. scripts/03_compute_mis.R writes signing_events_mis.csv.
4. The four analysis scripts write the findings CSVs.
5. The deploy workflow copies the findings CSVs into the published site directory
so the pages can read them.

## Documentation

- Method: docs/methodology.md
Roadmap: roadmap.md
- NBA awards reference: dashboard/docs/nba-awards-reference.md
- CBA thresholds reference: docs/docs-file.md

The awards and CBA-threshold references support a retired contract-tier
classifier. That approach left the launch when the contract source proved
unusable. The references stay in the repo for the roadmap work on contract value
and tiers. They are not part of the current analysis.

## Status

Version 1 is complete. The pipeline runs end to end and the site reports the
findings. Open items are tracked in roadmap.md. None block the launch.

## A note on what this is not

This is a research project driven by curiosity, not a predictive product. It
does not forecast individual outcomes, advise on roster construction, or replace
front-office analysis. It is a transparent, auditable test of whether a
behavioral pattern appears in NBA contract decisions.
