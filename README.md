# huncMarkovABD

**Acute Bronchial Disorder Cost-Effectiveness Model**

An R package implementing an interactive Markov cohort model for cost-effectiveness
analysis of Lunavar+SoC versus SoC alone in patients with Acute Bronchial Disorder
(ABD). Fictional illustrative model for the HEPackageR skill validation exercise.

## Model Structure

| Feature | Value |
|---------|-------|
| Health states | 5 (Stable, FlareUp_Mild, FlareUp_Moderate, FlareUp_Severe, Dead) |
| Comparator | SoC (standard of care) |
| Intervention | Lunavar+SoC (biologic add-on) |
| Cycle length | 2/52 years (2 weeks) |
| Time horizon | 50 years (1,300 cycles) |
| Discount rate | 3% (mid-cycle convention) |
| Background mortality | Canadian life table (55% female, age-dependent) |
| Tunnel states | FlareUp states last exactly 1 cycle, then resolve to Stable |
| Currency | Canadian dollars (CAD) |

## Installation

```r
remotes::install_github("HealthyUncertainty/huncMarkovABD")
```

## Quick Start

```r
library(huncMarkovABD)
launch_app()
res <- run_model()
res$icer
res_psa <- run_model(n_sim = 1000, seed = 42)
plot_ce_plane(res_psa$psa_results)
plot_ceac(res_psa$psa_results)
```

## Validation

Base-case deterministic results validated against original R script output.

| Outcome | SoC | Lunavar+SoC | Incremental |
|---------|-----|-------------|-------------|
| Life years | 19.7822 | 20.1022 | +0.3200 |
| Total costs (CAD) | $178,974 | $224,483 | $45,509 |
| QALYs | 14.9067 | 16.1372 | +1.2304 |
| ICER (CAD/QALY) | - | - | $36,986 |

All results match original script exactly. See `tests/validation/` for details.

## Development

Ian Cromwell (healthyuncertainty@gmail.com)

Developed using the [HEPackageR](https://github.com/HealthyUncertainty/hepackager) skill for Claude AI.

## Disclaimer

Fictional illustrative model. Not for clinical or policy use.

