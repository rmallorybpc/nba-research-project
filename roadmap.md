# Roadmap

This project answers three questions about NBA contract decisions. It is a
launch, not a finish. This file lists what comes next, in rough priority order.

## Refinements to the current findings

- Sharpen the treatment groups. The current method compares team rosters across
  an offseason. It cannot see why a player moved or stayed. The changed-teams
  group mixes free-agent signings with trades. The stayed group mixes fresh
  re-signings with players still under an older contract. A reliable contract
  source would let the study separate true free agents from traded players and
  separate new re-signings from holdovers. That would tighten every comparison.

- Test sensitivity to the contributor filter. A player must appear in at least
  40 games in the pre-signing season to enter the sample. The 40-game line is a
  reasonable cut, but the findings should be re-run at a few thresholds to show
  they do not depend on it.

## New questions the data can already answer

- Offense versus defense. Box Plus-Minus splits into an offensive and a
  defensive component. The raw means hinted that movers decline mostly on
  offense while their defense holds, and that supermax players decline on both
  ends. The columns already exist. The question is where decline shows up, and
  whether it differs by how a player got his contract.

- The James Harden effect. What happens when a star forces a move while still
  under contract, rather than signing or being traded in the normal way? These
  cases need a consistent coding rule before they can be measured.

- Player-empowerment-era selection. Top players increasingly engineer trades
  rather than reach free agency. That may make the pool of free agents a
  non-random, weaker sample. The question is whether selection, not movement,
  drives the team-change penalty.

## Data and presentation

- Year-by-year views. Show mean reversion and the team-change penalty by season,
  for the two well-powered groups. The supermax cohort is too small to show by
  year, so it stays pooled. This matches the season views in the NHL and NFL
  projects, where the data supports them.

- Swap Box Plus-Minus for a tracking-based metric. The pipeline is metric-
  agnostic. Only the fetcher changes. If subscription access to a metric like
  EPM becomes available, the analysis can re-run with no other change.

- Contract value and tier analysis. Dollar figures and salary-cap tiers left the
  launch when the public contract tracker proved unusable. A reliable contract
  source would let cap-percentage and tier questions return.

## Repo hygiene

- Consolidate the name normalizer. The same function is copied across several
  files. It should live in one shared utility, because the joins fail silently
  if the copies drift apart.

- Resolve script path drift. Some analysis scripts sit in scripts/ rather than
  R/04_analysis/. Move them to one place.

- Remove committed package documentation. Installed R-package help files were
  committed to the repo. They are bloat and should be removed.

- Write the methodology note. Document the choice of Box Plus-Minus over a
  tracking metric, the roster-comparison dilution, and the decision to report
  mean reversion as the headline finding rather than the supermax question.
DOC_EOF
