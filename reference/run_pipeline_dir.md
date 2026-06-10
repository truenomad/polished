# Run the cleaning pipeline from a directory of raw files

File-based convenience over
[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md):
reads the raw POLIS tables from `source_dir`, cleans them, and
(optionally) writes the cleaned outputs to `output_dir`.

## Usage

``` r
run_pipeline_dir(
  source_dir,
  output_dir = NULL,
  cfg = polis_config(),
  format = "rds"
)
```

## Arguments

- source_dir:

  Directory holding the raw POLIS exports.

- output_dir:

  Optional directory to write cleaned outputs to. If `NULL` the cleaned
  set is only returned.

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object (default
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)).

- format:

  Output file extension when writing (default `"rds"`).

## Value

A named list of cleaned tibbles (invisibly when writing).
