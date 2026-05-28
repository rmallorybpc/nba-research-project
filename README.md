# nba-research-project

## Documentation

- NBA awards supermax reference: [dashboard/docs/nba-awards-reference.md](dashboard/docs/nba-awards-reference.md)

## Data Validation

Run the awards completeness check before contract classification:

```bash
bash scripts/validate_nba_awards.sh nba_awards.csv
```

This enforces coverage for seasons 2013-14 through 2024-25 and fails if any
season is missing required All-NBA, MVP, or DPOY rows.