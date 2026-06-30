# Scaffold a full polished pipeline project

Creates the domain-numbered data-pipeline layout this package is built
around (`01_data/` domains, `02_scripts/`, `03_outputs/`) and writes a
wired `.Rprofile` (the `cfg` manifest), a `.gitignore`, and starter
`2a_download_data.R` / `2b_process_data.R` scripts. Everything the
generated `cfg` points at exists on disk, so after dropping the WHO
polio GDB layers in `01_data/1a_shapefiles/raw/`, sourcing `2a` then
`2b` runs the whole download -\> clean pipeline (downloads POLIS
streams + WorldPop, processes the shapefile, extracts WorldPop, and runs
[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
over every stream).

## Usage

``` r
init_polis_pipeline(
  root,
  regions = "EMRO",
  start_year = 2020,
  pop_years = 2010:2027,
  pop_source = c("reconciled", "polis", "worldpop"),
  domains = c("shapefiles", "population", "polis", "vaccination"),
  write_rprofile = TRUE,
  write_scripts = TRUE,
  gitignore = TRUE,
  overwrite = FALSE,
  renv = FALSE,
  quiet = FALSE
)
```

## Arguments

- root:

  Path to the project root (created recursively if absent).

- regions:

  WHO region codes the pipeline is scoped to, written into the generated
  `cfg`. Default `"EMRO"`.

- start_year:

  Earliest onset/collection year to retain. Default `2020`.

- pop_years:

  Calendar years for the population / WorldPop step. Default
  `2010:2027`.

- pop_source:

  Population denominator preference for
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md),
  one of `"reconciled"`, `"polis"`, `"worldpop"`. Default
  `"reconciled"`.

- domains:

  Which `01_data` domains to create: any of `"shapefiles"`,
  `"population"`, `"polis"`, `"vaccination"`. Default all four.

- write_rprofile, write_scripts, gitignore:

  Whether to write the wired `.Rprofile`, the starter `02_scripts/`, and
  the `.gitignore`. Default `TRUE`.

- overwrite:

  Overwrite `.Rprofile` / scripts / `.gitignore` that already exist.
  Default `FALSE` – existing files are kept (and skipped with a note),
  so re-running on a live project never clobbers it. Directory creation
  is always idempotent.

- renv:

  Set up `renv` in the project for reproducible package versions. When
  `TRUE` (and the optional `renv` package is installed) the scaffold
  runs
  [`renv::scaffold()`](https://rstudio.github.io/renv/reference/scaffold.html)
  – writing `renv/activate.R`, a starter `renv.lock`, and wiring the
  generated `.Rprofile` to load it – so a later
  [`renv::snapshot()`](https://rstudio.github.io/renv/reference/snapshot.html)
  pins your versions and collaborators reproduce them with
  [`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html).
  Default `FALSE`.

- quiet:

  Suppress the success message. Default `FALSE`.

## Value

Invisibly, a list with `root` and the absolute `dirs` created.

## Details

Distinct from
[`init_polis_project()`](https://truenomad.github.io/polished/reference/init_polis_project.md),
which scaffolds a lighter generic `raw/processed/validation/cache/logs`
layout.

## See also

[`init_polis_project()`](https://truenomad.github.io/polished/reference/init_polis_project.md),
[`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md),
[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md).

## Examples

``` r
proj <- init_polis_pipeline(
  file.path(tempdir(), "polio_pipeline"), regions = "EMRO", quiet = TRUE
)
file.exists(file.path(proj$root, ".Rprofile"))
#> [1] TRUE
```
