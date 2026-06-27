# Run the cleaning pipeline from a directory of raw files

File-based convenience over
[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md):
reads the `raw_*` POLIS tables (the names
[`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md)
writes) from `source_dir`, runs the full pipeline, and (optionally)
writes the outputs to `output_dir` as `polished_*` files. Each output's
file format follows its source raw file; derived outputs (virus,
indicators) default to `qs2`.

## Usage

``` r
run_pipeline_dir(
  source_dir,
  output_dir = NULL,
  cfg = polis_active_config(),
  refresh = FALSE
)
```

## Arguments

- source_dir:

  Directory holding the `raw_*` POLIS tables.

- output_dir:

  Optional directory to write `polished_*` outputs to; overrides
  `cfg$output_dir` when supplied. If both are `NULL` the output set is
  only returned.

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object; defaults to the session-active config
  ([`polis_active_config()`](https://truenomad.github.io/polished/reference/polis_active_config.md)).
  Its `shape` and `population` handles drive reconciliation and
  indicators.

- refresh:

  If `TRUE`, ignore any existing cache and re-run every step from
  scratch, overwriting the cache and output files. Default `FALSE`.

## Value

A named list of pipeline outputs (invisibly when writing).

## Details

A thin wrapper over
[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
with `inputs = source_dir`: the directory is read into the input list
(each output inheriting its source file's format) and writing follows
the same rules as
[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
– `polished_*` data in the `data/` sub-directory of `output_dir` and a
`checks_<dataset>.xlsx` workbook per dataset in the `checks/`
sub-directory. The check workbooks require the optional `openxlsx`
package.
