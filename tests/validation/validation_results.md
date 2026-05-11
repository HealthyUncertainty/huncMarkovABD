# Validation Results - huncMarkovABD

Deterministic base-case results validated against original R script
(model_abd.R / model_parameters_abd.R). All values match to within
rounding tolerance (< 0.01).

## Base-Case Results (Deterministic)

| Outcome | Original Script | Package Output | Status |
|---------|----------------|----------------|--------|
| Lunavar Total Cost (CAD) | $224,483 | $224,483 | PASS |
| Lunavar QALYs | 16.1372 | 16.1372 | PASS |
| Lunavar Life Years | 20.1022 | 20.1022 | PASS |
| SoC Total Cost (CAD) | $178,974 | $178,974 | PASS |
| SoC QALYs | 14.9067 | 14.9067 | PASS |
| SoC Life Years | 19.7822 | 19.7822 | PASS |
| ICER (CAD/QALY) | $36,986 | $36,986 | PASS |

## Notes

- Validated on 2026-05-10 using R 4
- 2-week cycles (2/52 yr), 50-year horizon, starting age 48, 55% female
- 3% discount rate; mid-cycle discounting convention
- Tunnel states: flare-up states exit after exactly 1 cycle
- No rounding corrections required

