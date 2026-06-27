# Read a country / period slice of the polished outputs

Reads the `polished_*` files
[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
wrote to the `data/` sub-directory of `output_dir` and returns them
filtered to a country and/or year, as a named list (one element per
dataset).

## Usage

``` r
load_polished(
  country = NULL,
  year = NULL,
  datasets = NULL,
  output_dir = cfg$output_dir,
  cfg = polis_active_config()
)
```

## Arguments

- country:

  Optional country/ISO3 value(s) to keep. Matched against the first
  present of `iso3`, `country_iso3code`, `country` in each table, so an
  ISO3 like `"AFG"` is expected. The `adm0` *name* column is
  intentionally not used for this filter. `NULL` keeps all.

- year:

  Optional year(s) to keep. Matched against the first present of `year`,
  `year_onset`, `year_start`, `year_collection`, `collect_yr`. `NULL`
  keeps all.

- datasets:

  Optional character vector restricting which datasets to read (by
  output key, e.g. `"afp"`, `"es"`, `"virus"`). `NULL` (default) reads
  every `polished_*` file present.

- output_dir:

  Directory the outputs were written to; defaults to `cfg$output_dir`.
  The files are read from its `data/` sub-directory.

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object; defaults to the session-active config
  ([`polis_active_config()`](https://truenomad.github.io/polished/reference/polis_active_config.md)),
  so `load_polished(country = "AFG")` works when the config carries
  `output_dir`.

## Value

A named list of filtered tibbles, one per `polished_*` dataset found.

## See also

[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md),
which writes the files this reads.

## Examples

``` r
if (FALSE) { # \dontrun{
afg <- load_polished(country = "AFG")
afg$afp
load_polished(country = "AFG", year = 2024, datasets = "afp")$afp
} # }
```
