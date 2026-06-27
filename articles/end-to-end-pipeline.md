# End-to-end pipeline

The download and cleaning halves share **one naming convention** so the
whole workflow reads top to bottom:
[`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md)
writes each table under a `raw_*` name, and the pipeline reads those
`raw_*` inputs and writes `polished_*` outputs.

| Stream | Downloaded as | Cleaned to |
|----|----|----|
| AFP cases | `raw_afp` | `polished_afp` |
| Environmental samples | `raw_es` | `polished_es` |
| Human specimens | `raw_hum_spec` | `polished_hum_spec` |
| SIA campaigns | `raw_activity` + `raw_sub_activity` | `polished_sia` |
| Poliovirus positives | (derived) | `polished_virus` |

Files written by older versions under the bare table name
(e.g. `case.qs2`) are renamed to their `raw_*` stem in place on the next
[`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md)
run — no re-download.

## From a directory: `run_pipeline_dir()`

[`run_pipeline_dir()`](https://truenomad.github.io/polished/reference/run_pipeline_dir.md)
is the one-call file-based workflow: it reads the `raw_*` tables from a
source directory, runs the full pipeline, and writes `polished_*`
outputs to an output directory. Each output’s **format follows its
source file** (a `raw_afp.rds` yields `polished_afp.rds`; a `raw_es.qs2`
yields `polished_es.qs2`); derived outputs default to `qs2`.

``` r

raw_dir <- file.path(tempdir(), "raw")
out_dir <- file.path(tempdir(), "processed")
dir.create(raw_dir, showWarnings = FALSE)

afp_raw <- data.frame(
  Id = 1:3,
  Epid = c("NIE-BOR-MMC-24-001", "NIE-BOR-MMC-24-001", "NIE-YOB-GUJ-24-014"),
  LastUpdateDate = c("2024-02-01", "2024-04-01", "2024-03-01"),
  ParalysisOnsetDate = c("2024-01-02", "2024-01-02", "2024-02-03"),
  Classification = c("Confirmed (wild)", "Confirmed (wild)", "Discarded"),
  PolioVirusTypes = c("WILD1", "WILD1", NA),
  Admin0Name = "NIGERIA", CountryISO3Code = "NGA",
  check.names = FALSE
)
saveRDS(afp_raw, file.path(raw_dir, "raw_afp.rds"))

cleaned <- run_pipeline_dir(raw_dir, out_dir)
list.files(out_dir)
#> [1] "checks" "data"
```

Two things landed for every cleaned dataset: the `polished_*` table
(here `polished_afp.rds`, matching the source format, plus the derived
`polished_virus.qs2`) **and** a `checks_<dataset>.xlsx` data-quality
workbook (see the *Data-quality checks* article).

[`run_pipeline_dir()`](https://truenomad.github.io/polished/reference/run_pipeline_dir.md)
returns the cleaned set invisibly, so you can keep working in memory
too:

``` r

names(cleaned)
#> [1] "afp"   "virus"
```

## Reconciliation and indicators via `polis_config()`

The pipeline’s optional inputs — an already-processed district `shape`
for admin reconciliation, and an under-15 `population` table for the
rate indicators — are carried on the config object, so they flow to
every cleaner and to
[`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md)
in one place:

``` r

cfg <- polis_config(
  shape = "data/gpei_adm2_shape.rds", # an sf layer or long ADM2 table (or a path)
  population = "data/under15_pop.csv" # guid / year / pop denominators (or a path)
)
run_pipeline_dir(raw_dir, out_dir, cfg = cfg)
```

With a `shape`, each cleaner reconciles admin names/GUIDs and recovers
missing geography; with a `population`, the run also computes the
indicator catalogue.

## A managed workspace: `polis_project`

For a recurring pipeline it helps to keep raw, processed, cache and log
data in one tidy place.
[`init_polis_project()`](https://truenomad.github.io/polished/reference/init_polis_project.md)
lays out that structure:

``` r

proj <- init_polis_project(file.path(tempdir(), "demo_project"), quiet = TRUE)
names(proj)
#> [1] "root"       "raw"        "processed"  "validation" "cache"     
#> [6] "logs"
```

Point the config at the project zones — `inputs` at `raw/`, `output_dir`
at `processed/`, `cache_dir` at `cache/` — and the whole run is one
call. Drop your `raw_*` files in `proj$raw` (or have
`get_polis_data(polis_folder = proj$raw)` write them there):

``` r

saveRDS(afp_raw, file.path(proj$raw, "raw_afp.rds"))

cfg <- polis_config(
  inputs = proj$raw,
  output_dir = proj$processed,
  cache_dir = proj$cache
)
out <- run_pipeline(cfg = cfg)
names(out)
#> [1] "afp"   "virus"
```

Read a country/period slice of the `polished_*` outputs back with
[`load_polished()`](https://truenomad.github.io/polished/reference/load_polished.md)
— it defaults `output_dir` to the active config, so no arguments are
needed beyond the filter:

``` r

back <- load_polished(datasets = "afp")
nrow(back$afp)
#> [1] 2
```

[`clear_cache()`](https://truenomad.github.io/polished/reference/clear_cache.md)
empties the project’s cache zone, and
[`project_path()`](https://truenomad.github.io/polished/reference/project_path.md)
resolves any zone path. See
[`?init_polis_project`](https://truenomad.github.io/polished/reference/init_polis_project.md)
for the full layout.
