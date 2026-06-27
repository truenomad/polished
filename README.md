# polished <img src="man/figures/logo.png" align="right" height="139" alt="polished package logo" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/truenomad/polished/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/truenomad/polished/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/truenomad/polished/graph/badge.svg)](https://app.codecov.io/gh/truenomad/polished)
[![lint](https://github.com/truenomad/polished/actions/workflows/lint.yaml/badge.svg)](https://github.com/truenomad/polished/actions/workflows/lint.yaml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://cran.r-project.org/)

<!-- badges: end -->

`polished` does two things: it downloads data from the WHO POLIS OData API, and
it cleans that data for analysis. The downloader writes each table as `raw_*`;
the pipeline reads those `raw_*` inputs and writes cleaned `polished_*` outputs.

## Installation

Install the development version from GitHub with
`pak::pak("truenomad/polished")`. Downloading requires a POLIS API key, read
from the `POLIS_API_KEY` environment variable.

## The workflow

```r
library(polished)

# 1. Download — writes raw_afp, raw_es, ... to a local cache (resumable, parallel)
get_polis_data(tables = c("case", "environmental_sample"), polis_folder = "data/polis")

# 2. Clean + indicators + checks in one call: raw_* in, polished_* out
run_pipeline_dir("data/polis", "data/processed")
#   -> polished_afp.*, polished_es.*, polished_virus.*  + checks_*.xlsx workbooks
```

## Key functions

| Function                                                                              | Purpose                                                                                                                                                                                                                                                      |
| ------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `get_polis_data()`                                                                    | Pull one or many POLIS tables into a local cache. Works around POLIS's year-aligned date filters and Id-range pagination, checkpoints each batch so an interrupted pull resumes cleanly, fetches years in parallel, and verifies completeness against POLIS. |
| `run_pipeline()` / `run_pipeline_dir()`                                               | Run the whole cleaning set — AFP, ES, human specimens, SIA, and the derived virus positives — in memory or from a directory of `raw_*` files, with optional admin reconciliation and surveillance indicators.                                                |
| `clean_afp()` · `clean_es()` · `clean_human_spec()` · `clean_sia()` · `clean_virus()` | The per-stream cleaners: standardise names, sanitise dates, derive analytic variables, reconcile geography, dedup to one row per POLIS id.                                                                                                                   |
| `impute_geo_from_epid()`                                                              | Recover missing administrative geography from the EPID through an ordered, provenance-stamped cascade — fills only blank cells, never fabricates on ambiguity.                                                                                               |
| `calc_polio_indicators()`                                                             | Compute the WHO POLIS indicator catalogue (NPAFP rate, stool adequacy, timeliness, dose, ES, virus, SIA, composite families) from the cleaned tables.                                                                                                        |
| `checks_afp()` … `write_checks_excel()`                                               | Per-stream data-quality checks exported as a styled Excel workbook, one tab per check.                                                                                                                                                                       |
| `init_polis_project()`                                                                | Set up a standard raw / processed / cache project workspace and stream the pipeline into it.                                                                                                                                                                 |

See the [vignettes](https://truenomad.github.io/polished/) and each function's
help page (e.g. `?get_polis_data`) for usage and data-formatting requirements.

## License

MIT © Mohamed A. Yusuf. See [LICENSE](LICENSE) for details. Issues and pull
requests welcome at <https://github.com/truenomad/polished>.
