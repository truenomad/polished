# Process the POLIS SIA campaign-quality tables (LQAS + IM)

Thin entry point that runs
[`process_lqas()`](https://truenomad.github.io/polished/reference/process_lqas.md)
and/or
[`process_im()`](https://truenomad.github.io/polished/reference/process_im.md)
over the raw POLIS `Lqas` and `Im` tables and returns their roll-ups
together. Either input may be `NULL`; the matching slot is then `NULL`.
This is what
[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
calls when an `lqas` / `im` table is present in its inputs.

## Usage

``` r
process_sia_quality(
  lqas = NULL,
  im = NULL,
  cfg = polis_active_config(),
  shape = NULL,
  verbose = TRUE,
  summary = TRUE,
  ...
)
```

## Arguments

- lqas:

  Optional raw POLIS LQAS table (data.frame). `NULL` (default) skips
  LQAS.

- im:

  Optional raw POLIS IM table (data.frame). `NULL` (default) skips IM.

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object (default
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md));
  its `crosswalk` standardises the raw column names.

- shape:

  Optional district shape (an `sf` polygon layer or a long ADM2
  attribute table) used to reconcile admin names/GUIDs via
  [`reconcile_admin_guids()`](https://truenomad.github.io/polished/reference/reconcile_admin_guids.md),
  exactly as the other cleaners use it. Default `NULL` (no shape-based
  reconciliation).

- verbose:

  Emit cli progress + one-line roll-up results (default `TRUE`).

- summary:

  Emit the full cli summary panel per stream (rules, class breakdown,
  mean missed-children). Default `TRUE` for standalone use;
  [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  passes `FALSE` so the pipeline stays terse and only the key steps and
  roll-up counts are reported.

- ...:

  Extra analytic arguments forwarded to
  [`process_lqas()`](https://truenomad.github.io/polished/reference/process_lqas.md)
  (e.g. `pass_threshold`). Each processor keeps only the arguments it
  accepts.

## Value

A named list with `lqas` (the
[`process_lqas()`](https://truenomad.github.io/polished/reference/process_lqas.md)
result, or `NULL`) and `im` (the
[`process_im()`](https://truenomad.github.io/polished/reference/process_im.md)
result, or `NULL`).

## Examples

``` r
lqas <- data.frame(
  Id = 1L, Admin0Name = "NIGERIA", Admin1Name = "KANO",
  Admin2Name = "NASSARAWA", Admin2GUID = "{A2}", Year = 2024L,
  ChildrenChecked = 60L, ChildrenFoundUnvaccinated = 3L,
  Lqas2ClassificationName = "Pass", Lqas3ClassificationName = "Pass"
)
out <- process_sia_quality(lqas = lqas, verbose = FALSE)
out$lqas$district
#> # A tibble: 1 × 14
#>    year adm2_guid adm0    adm1  adm2      n_lots n_pass_polis n_fail_polis
#>   <int> <chr>     <chr>   <chr> <chr>      <int>        <int>        <int>
#> 1  2024 {A2}      NIGERIA KANO  NASSARAWA      1            1            0
#> # ℹ 6 more variables: n_invalid_polis <int>, n_pass_derived <int>,
#> #   n_fail_derived <int>, n_invalid_derived <int>, pass_pct_polis <dbl>,
#> #   pass_pct_derived <dbl>
```
