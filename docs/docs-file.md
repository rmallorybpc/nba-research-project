# NBA CBA Thresholds - Contract Classification Reference

This file is the authoritative lookup table for classifying NBA contracts in the
`nba-free-agency-research` pipeline. It is consumed by
`R/03_features/02_classify_contract_types.R` via the companion file
`cba_thresholds.csv`. Do not hand-edit dollar figures in code - read them from
the CSV so there is a single source of truth.

All salary cap figures are the official figures announced by the NBA for each
season. Max-contract dollar figures are computed as the stated percentage of the
salary cap for that season (cap x tier percentage), which is how first-year
maximum salaries are defined under the CBA.

## Salary cap by season

The nine-season research window runs from 2016-17 through 2024-25. Two CBA eras
fall inside the window: the 2017 CBA (in effect 2017-18 through 2022-23) and the
2023 CBA (in effect from 2023-24). The 2016-17 season was the final year of the
prior (2011) CBA. The 2020-21 cap was held flat at the 2019-20 figure because of
the COVID-19 revenue disruption rather than following normal growth.

| Season  | Salary cap    | CBA era   | Note                      |
|---------|---------------|-----------|---------------------------|
| 2016-17 | $94,143,000   | 2011 CBA  | TV-deal cap spike         |
| 2017-18 | $99,093,000   | 2017 CBA  |                           |
| 2018-19 | $101,869,000  | 2017 CBA  |                           |
| 2019-20 | $109,140,000  | 2017 CBA  |                           |
| 2020-21 | $109,140,000  | 2017 CBA  | Held flat (COVID)         |
| 2021-22 | $112,414,000  | 2017 CBA  |                           |
| 2022-23 | $123,655,000  | 2017 CBA  |                           |
| 2023-24 | $136,021,000  | 2023 CBA  | New media-deal era begins |
| 2024-25 | $140,588,000  | 2023 CBA  |                           |

## Maximum-salary tiers (first-year salary)

A player's maximum first-year salary is set by years of service (YOS):

- **25% of cap** - players with 0-6 YOS
- **30% of cap** - players with 7-9 YOS
- **35% of cap** - players with 10+ YOS

In every tier the player may alternatively earn 105% of the final-year salary of
their prior contract if that figure is greater than the cap-percentage max. The
classification script should treat the cap-percentage figure as the tier
threshold and flag any contract whose value exceeds the tier as a likely
105%-of-prior case rather than a misclassification.

| Season  | 25% max       | 30% max       | 35% max       |
|---------|---------------|---------------|---------------|
| 2016-17 | $23,535,750   | $28,242,900   | $32,950,050   |
| 2017-18 | $24,773,250   | $29,727,900   | $34,682,550   |
| 2018-19 | $25,467,250   | $30,560,700   | $35,654,150   |
| 2019-20 | $27,285,000   | $32,742,000   | $38,199,000   |
| 2020-21 | $27,285,000   | $32,742,000   | $38,199,000   |
| 2021-22 | $28,103,500   | $33,724,200   | $39,344,900   |
| 2022-23 | $30,913,750   | $37,096,500   | $43,279,250   |
| 2023-24 | $34,005,250   | $40,806,300   | $47,607,350   |
| 2024-25 | $35,147,000   | $42,176,400   | $49,205,800   |

## Supermax - Designated Veteran Player Extension

The supermax (officially the Designated Veteran Player Extension or Designated
Veteran Player Contract) lets an eligible player start at **35% of the cap** -
the same dollar figure as the 10+ YOS max in the table above - even when they have
only 7-9 YOS and would otherwise be capped at 30%. This is the central contract
type for the research question, because it is only available from the player's
incumbent team and removes the player from genuine free-agent price discovery.

Eligibility (must meet all of the following):

- 7-9 YOS at the time the contract is executed (a player with 10+ YOS gets 35%
	automatically and is not a "designated veteran" case).
- Rendered all those years with the team offering the contract, or changed teams
	only by trade during the first four cap years of their career.
- Met at least one performance trigger in the qualifying window: named to an
	All-NBA team in the most recent season or in two of the prior three seasons;
	named NBA MVP in any of the three most recent seasons; or named Defensive
	Player of the Year in the most recent season or in two of the prior three
	seasons.

Only the incumbent team can offer a supermax. A rival team is limited to 30% for
the same player. This is the structural asymmetry the supermax-reset hypothesis
turns on.

## Rose Rule - Designated Rookie Extension (younger-player analog)

The Rose Rule lets a player with 0-6 YOS start at **30% of the cap** instead of
25% when they meet performance triggers nearly identical to the supermax
(All-NBA, MVP, or DPOY in the qualifying window). It is a separate rule from the
supermax but functions the same way - performance accolades bump a player up one
tier. Anthony Edwards's 2024 extension (30% at 4 YOS) is a clean example.

For classification purposes the Rose Rule produces a 30% contract for a player
who by raw YOS would be in the 25% tier. The script should flag these rather than
treat them as misclassified.

## Annual raises and contract length (for value reconstruction)

- Maximum annual raises: 8% of first-year salary for a player re-signing with
	their own team (including supermax), 5% for a player signing with a new team.
	Raises are simple, not compounding - each year adds the fixed percentage of the
	first-year figure.
- Maximum length: 5 years when re-signing with own team (Bird rights or
	designated veteran), 4 years when signing with a new team.

These let the pipeline reconstruct total contract value from first-year salary
when only the headline number is available, and they explain why a supermax total
can approach or exceed $300M in the later seasons of the window.

## Classification logic summary

For each signing event, classify in this order (first match wins). The minimum,
BAE, and MLE branches are checked smallest-to-largest so a deal is labelled by the
lowest band it falls within:

1. **Supermax / Designated Veteran** - first-year salary at 35% of cap AND 7-9
	 YOS AND re-signed with incumbent team. (10+ YOS at 35% is a standard max, not
	 a supermax.)
2. **Max (35% tier)** - 10+ YOS, first-year salary at or near 35% of cap.
3. **Max (30% tier)** - 7-9 YOS at 30%, or 0-6 YOS at 30% via Rose Rule.
4. **Max (25% tier)** - 0-6 YOS at 25%.
5. **Minimum** - value at or below the player's OWN year-of-service minimum.
	 This is YOS-specific, not a single per-season number: a rookie minimum is
	 roughly a third of a 10+ year veteran minimum, so the script joins the minimum
	 scale (`nba_minimum_scale.csv`) on season and capped YOS. A $3M deal is a
	 minimum for a 12-year veteran but a non-minimum deal for a 2-year player.
6. **Bi-Annual Exception** - value at or below the season's BAE.
7. **Mid-Level Exception** - value at or below the season's non-taxpayer MLE.
8. **Standard** - anything not matching the above (a negotiated, non-max,
	 above-exception deal).

**Important caveat on branches 5-7.** The classification infers contract type from
salary magnitude, not from the cap mechanism actually used. The minimum, BAE, and
non-taxpayer MLE bands overlap in dollar terms - a $3M signing could be a BAE deal,
a partial MLE, or a small standard contract, and the salary alone cannot always
distinguish them. These three labels are therefore a reasonable inference, not a
record of the exception the team actually invoked. This fuzziness is inherent to
salary-based classification and does not affect the supermax/max tiers (branches
1-4), which are the focus of the research question. If exact mechanism is ever
needed for a specific signing, it must come from a transaction source that records
the exception used, not from salary.

Treatment category (separate field from contract type):

- `new_team` - UFA signed with a team other than their prior team.
- `re_signed_standard` - re-signed with incumbent team on any non-supermax deal.
- `re_signed_supermax` - re-signed/extended with incumbent team on a supermax.

## MLE, BAE, and minimum figures

The MLE (non-taxpayer, taxpayer, room) and BAE first-year values are now in
`cba_thresholds.csv` for all nine signing seasons, drawn from the official
pr.nba.com cap announcements (which list all three MLE levels per season) for
2016-17 through 2021-22, and from the 2023-CBA structural changes for 2023-24
onward (non-taxpayer MLE received a one-time 7.5% bump plus the usual cap-linked
increase; taxpayer MLE was flattened to a flat $5M then resumes cap-linked growth;
room MLE received a one-time 30% bump). The 2020-21 figures are held flat at the
2019-20 values because the cap was frozen that season.

The minimum salary scale lives in `nba_minimum_scale.csv` as one row per (season,
YOS). Minimums adjust each season by the same percentage as the cap (the league's
stated rule), so the scale is anchored on two fully verified seasons - 2022-23
(2017 CBA) and 2024-25 (2023 CBA) - and the intermediate seasons are derived by
cap-proportional scaling within each CBA era. Each cell carries a `basis` field
marking it `verified`, `verified_shape` (mid-band interpolation for 2024-25), or
`derived_cap_proportional`.

## Sources

Salary cap and MLE figures: official NBA press releases (pr.nba.com) for each
season; 2020-21 flat-cap confirmation from contemporaneous reporting. BAE and the
2023-CBA exception changes: Hoops Rumors annual exception summaries. Minimum scale
anchors: official figures via Sports Illustrated, Hoops Rumors, and Spotrac for
2022-23 and 2024-25. Max-tier rules and supermax eligibility: NBA CBA Article II,
Section 7; corroborated by CBA-FAQ, Hoops Rumors glossary, and league reporting.
