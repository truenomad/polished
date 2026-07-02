# polished <img src="man/figures/logo.png" align="right" height="139" alt="polished package logo" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/truenomad/polished/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/truenomad/polished/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/truenomad/polished/graph/badge.svg?token=bHamTc9ITd)](https://app.codecov.io/gh/truenomad/polished)
[![lint](https://github.com/truenomad/polished/actions/workflows/lint.yaml/badge.svg)](https://github.com/truenomad/polished/actions/workflows/lint.yaml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://cran.r-project.org/)

<!-- badges: end -->

`polished` retrieves and cleans poliovirus surveillance data from the WHO Polio
Information System (POLIS). The downloader writes each table to a local cache as
`raw_*`; an end-to-end pipeline then reads those `raw_*` inputs and writes
cleaned, analysis-ready `polished_*` tables, with optional surveillance
indicators and data-quality checks.

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

For a complete, reproducible project — the `01_data` domain layout, a wired
`.Rprofile` (the `cfg` manifest), and runnable download + process scripts — in
one call:

```r
# scaffolds the whole pipeline project, then run 2a (download) and 2b (process)
init_polis_pipeline("my_project", regions = "EMRO")

# add renv = TRUE to pin package versions (renv::snapshot / restore) for collaborators
init_polis_pipeline("my_project", regions = "EMRO", renv = TRUE)
```

## Key functions

| Function                                                                              | Purpose                                                                                                                                                                                                                                                      |
| ------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `get_polis_data()`                                                                    | Pull one or many POLIS tables into a local cache. Works around POLIS's year-aligned date filters and Id-range pagination, checkpoints each batch so an interrupted pull resumes cleanly, fetches years in parallel, and verifies completeness against POLIS. |
| `run_pipeline()` / `run_pipeline_dir()`                                               | Run the whole cleaning set — AFP, ES, human specimens, SIA, and the derived virus positives — in memory or from a directory of `raw_*` files, with optional admin reconciliation and surveillance indicators.                                                |
| `clean_afp()` · `clean_es()` · `clean_human_spec()` · `clean_sia()` · `clean_virus()` | The per-stream cleaners: standardise names, sanitise dates, derive analytic variables, reconcile geography, dedup to one row per POLIS id.                                                                                                                   |
| `clean_pop()` | Clean the POLIS population reference into adm0/adm1/adm2 under-5 / under-15 / all-ages denominators, optionally reconciled against WorldPop and rolled up by boundary validity; the rate-indicator base. |
| `impute_geo_from_epid()`                                                              | Recover missing administrative geography from the EPID through an ordered, provenance-stamped cascade — fills only blank cells, never fabricates on ambiguity.                                                                                               |
| `calc_polio_indicators()`                                                             | Compute the WHO POLIS indicator catalogue (NPAFP rate, stool adequacy, timeliness, dose, ES, virus, SIA, composite families) from the cleaned tables.                                                                                                        |
| `checks_afp()` … `write_checks_excel()`                                               | Per-stream data-quality checks exported as a styled Excel workbook, one tab per check.                                                                                                                                                                       |
| `init_polis_pipeline()`                                                               | Scaffold a full pipeline project in one call — the `01_data` domain layout, a wired `.Rprofile` (the `cfg` manifest), a `.gitignore`, and runnable download / process scripts.                                                                               |
| `init_polis_project()`                                                                | Set up a lighter raw / processed / cache project workspace and stream the pipeline into it.                                                                                                                                                                  |

See the [vignettes](https://truenomad.github.io/polished/) and each function's
help page (e.g. `?get_polis_data`) for usage and data-formatting requirements.

## Citation

To cite `polished` in publications, run `citation("polished")` in R, or use:

> Yusuf, Mohamed A. (2026). *polished: Download and Clean WHO POLIS Data*. R
> package version 0.1.0. <https://github.com/truenomad/polished>

```
@Manual{polished,
  title  = {polished: Download and Clean WHO POLIS Data},
  author = {Mohamed A. Yusuf},
  year   = {2026},
  note   = {R package version 0.1.0},
  url    = {https://github.com/truenomad/polished},
}
```

## License

MIT © Mohamed A. Yusuf. See [LICENSE](LICENSE) for details. Issues and pull
requests welcome at <https://github.com/truenomad/polished>.
