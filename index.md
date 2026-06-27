# polished

`polished` does two things: it downloads data from the WHO POLIS OData
API, and it cleans that data for analysis. The downloader writes each
table as `raw_*`; the pipeline reads those `raw_*` inputs and writes
cleaned `polished_*` outputs.

## Installation

Install the development version from GitHub with
`pak::pak("truenomad/polished")`. Downloading requires a POLIS API key,
read from the `POLIS_API_KEY` environment variable.

## The workflow

``` r

library(polished)

# 1. Download — writes raw_afp, raw_es, ... to a local cache (resumable, parallel)
get_polis_data(tables = c("case", "environmental_sample"), polis_folder = "data/polis")

# 2. Clean + indicators + checks in one call: raw_* in, polished_* out
run_pipeline_dir("data/polis", "data/processed")
#   -> polished_afp.*, polished_es.*, polished_virus.*  + checks_*.xlsx workbooks
```

## Key functions

| Function | Purpose |
|----|----|
| [`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md) | Pull one or many POLIS tables into a local cache. Works around POLIS’s year-aligned date filters and Id-range pagination, checkpoints each batch so an interrupted pull resumes cleanly, fetches years in parallel, and verifies completeness against POLIS. |
| [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md) / [`run_pipeline_dir()`](https://truenomad.github.io/polished/reference/run_pipeline_dir.md) | Run the whole cleaning set — AFP, ES, human specimens, SIA, and the derived virus positives — in memory or from a directory of `raw_*` files, with optional admin reconciliation and surveillance indicators. |
| [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md) · [`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md) · [`clean_human_spec()`](https://truenomad.github.io/polished/reference/clean_human_spec.md) · [`clean_sia()`](https://truenomad.github.io/polished/reference/clean_sia.md) · [`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md) | The per-stream cleaners: standardise names, sanitise dates, derive analytic variables, reconcile geography, dedup to one row per POLIS id. |
| [`impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md) | Recover missing administrative geography from the EPID through an ordered, provenance-stamped cascade — fills only blank cells, never fabricates on ambiguity. |
| [`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md) | Compute the WHO POLIS indicator catalogue (NPAFP rate, stool adequacy, timeliness, dose, ES, virus, SIA, composite families) from the cleaned tables. |
| [`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md) … [`write_checks_excel()`](https://truenomad.github.io/polished/reference/write_checks_excel.md) | Per-stream data-quality checks exported as a styled Excel workbook, one tab per check. |
| [`init_polis_project()`](https://truenomad.github.io/polished/reference/init_polis_project.md) | Set up a standard raw / processed / cache project workspace and stream the pipeline into it. |

See the [vignettes](https://truenomad.github.io/polished/) and each
function’s help page
(e.g. [`?get_polis_data`](https://truenomad.github.io/polished/reference/get_polis_data.md))
for usage and data-formatting requirements.

## License

MIT © Mohamed A. Yusuf. See
[LICENSE](https://truenomad.github.io/polished/LICENSE) for details.
Issues and pull requests welcome at
<https://github.com/truenomad/polished>.
