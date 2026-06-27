# Process raw Independent Monitoring (IM) data into missed-children rates

Cleans the raw POLIS `Im` table with the same recipe as
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
/
[`clean_sia()`](https://truenomad.github.io/polished/reference/clean_sia.md)
– standardise names via the crosswalk, clean strings, parse and sanitise
dates, fix admin names and (when a `shape` is supplied) reconcile admin
GUIDs, then dedup by `id` – and computes the missed-children fraction
per district-year **separately for in-house and out-of-house
monitoring**. For each setting `missed = 1 - sum(marked)/sum(checked)`,
falling back to `mean(result)` when no children were checked, with a
Valid/Invalid status flag (Invalid when the result is missing or
negative).

## Usage

``` r
process_im(
  im,
  cfg = polis_active_config(),
  shape = NULL,
  verbose = TRUE,
  summary = TRUE
)
```

## Arguments

- im:

  Raw IM table (data.frame/tibble).

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object (default
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md));
  its `crosswalk` maps the raw column names to canonical snake_case.

- shape:

  Optional district shape (an `sf` polygon layer or a long ADM2
  attribute table) for admin GUID reconciliation via
  [`reconcile_admin_guids()`](https://truenomad.github.io/polished/reference/reconcile_admin_guids.md)
  (keyed on `year`). Default `NULL`.

- verbose:

  Emit cli progress + the one-line roll-up result (default `TRUE`).

- summary:

  Emit the full cli summary panel (per-setting missed-children). Default
  `TRUE`;
  [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  passes `FALSE` to keep the pipeline terse.

## Value

A list with `district` (per-district-year, carrying both
`missed_frac_inhouse` / `im_status_inhouse` and `missed_frac_outhouse` /
`im_status_outhouse` plus their checked/marked totals) and `meta`.

## Examples

``` r
im <- data.frame(
  Id = 1L, Admin0 = "NIGERIA", Admin1 = "KANO", Admin2 = "NASSARAWA",
  Admin2GUID = "{A2}", ActivityPlannedDateFromYear = 2024L,
  HouseholdsNumberChildrenChecked = 20L,
  HouseholdsNumberChildrenMarked = 18L, HouseholdsResult = NA_real_,
  OutOfHouseNumberChildrenChecked = 10L,
  OutOfHouseNumberChildrenMarked = 8L, OutOfHouseResult = NA_real_
)
process_im(im, verbose = FALSE)$district
#> # A tibble: 1 × 13
#>    year adm2_guid adm0    adm1  adm2      n_checked_inhouse n_marked_inhouse
#>   <int> <chr>     <chr>   <chr> <chr>                 <dbl>            <dbl>
#> 1  2024 {A2}      NIGERIA KANO  NASSARAWA                20               18
#> # ℹ 6 more variables: missed_frac_inhouse <dbl>, n_checked_outhouse <dbl>,
#> #   n_marked_outhouse <dbl>, missed_frac_outhouse <dbl>,
#> #   im_status_inhouse <chr>, im_status_outhouse <chr>
```
