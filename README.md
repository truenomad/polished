# polished <img src="man/figures/logo.png" align="right" height="139" alt="polished package logo" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/truenomad/polished/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/truenomad/polished/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/truenomad/polished/graph/badge.svg?token=bHamTc9ITd)](https://app.codecov.io/gh/truenomad/polished)
[![lint](https://github.com/truenomad/polished/actions/workflows/lint.yaml/badge.svg)](https://github.com/truenomad/polished/actions/workflows/lint.yaml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://cran.r-project.org/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22998766.svg)](https://doi.org/10.5281/zenodo.22998766)

<!-- badges: end -->

`polished` retrieves and cleans poliovirus surveillance data from the WHO Polio
Information System (POLIS). The downloader writes each table to a local cache as
`raw_*`; the cleaning pipeline reads those files and writes
`polished_*` tables, with optional surveillance
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

# 2. Clean the downloaded tables and write outputs and quality reports
run_pipeline_dir("data/polis", "data/processed")
#   -> polished_afp.*, polished_es.*, polished_virus.*  + checks_*.xlsx workbooks
```

To create the project directories, a `.Rprofile` that defines `cfg`, and
starter scripts for downloading and processing data:

```r
# scaffolds the whole pipeline project, then run 2a (download) and 2b (process)
init_polis_pipeline("my_project", regions = "EMRO")

# add renv = TRUE to pin package versions (renv::snapshot / restore) for collaborators
init_polis_pipeline("my_project", regions = "EMRO", renv = TRUE)
```

## Key functions

| Function | Purpose |
| --- | --- |
| `get_polis_data()` | Download POLIS tables to a local cache, with resumable batches, parallel year downloads and checks for missing records. |
| `run_pipeline()` / `run_pipeline_dir()` | Clean tables in memory or from `raw_*` files, with optional geography reconciliation and surveillance indicators. |
| `clean_afp()` · `clean_es()` · `clean_human_spec()` · `clean_sia()` | Standardise columns, parse dates, derive variables, reconcile geography and remove duplicate records for each stream. |
| `clean_virus()` | Combine poliovirus-positive records from cleaned AFP and environmental samples. |
| `clean_pop()` | Prepare country, province and district population denominators, with optional WorldPop reconciliation. |
| `impute_geo_from_epid()` | Fill missing administrative names and GUIDs using EPID matches, and record the source of each fill. |
| `calc_polio_indicators()` | Calculate surveillance indicators from cleaned tables, including NPAFP rate, stool adequacy and timeliness. |
| `checks_afp()` … `write_checks_excel()` | Check cleaned tables and export a summary and flagged records to Excel. |
| `init_polis_pipeline()` | Create data directories, a project configuration and starter download and processing scripts. |
| `init_polis_project()` | Create directories for raw data, processed outputs, validation reports, caches and logs. |

See the [vignettes](https://truenomad.github.io/polished/) and each function's
help page (e.g. `?get_polis_data`) for usage and data-formatting requirements.

## Citation

To cite `polished` in publications, run `citation("polished")` in R, or use:

> Yusuf, Mohamed A. (2026). *polished: Retrieve and Prepare GPEI POLIS
> Surveillance Data*. R package version 0.3.0.
> <https://doi.org/10.5281/zenodo.22998766>

```
@Manual{polished,
  title  = {polished: Retrieve and Prepare GPEI POLIS Surveillance Data},
  author = {Mohamed A. Yusuf},
  year   = {2026},
  note   = {R package version 0.3.0},
  url    = {https://github.com/truenomad/polished},
  doi    = {10.5281/zenodo.22998766},
}
```

## License

MIT © Mohamed A. Yusuf. See [license](LICENSE.md) for details. Issues and pull
requests welcome at <https://github.com/truenomad/polished>.
